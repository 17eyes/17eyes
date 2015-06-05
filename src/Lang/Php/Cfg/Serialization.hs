{-
Copyright (c) 2015 17eyes.com

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
-}

{-# LANGUAGE GADTs, TemplateHaskell, FlexibleInstances #-}

module Lang.Php.Cfg.Serialization where

import Control.Applicative
import Control.Monad(replicateM)
import Control.Arrow(second,first)

import Control.Monad.State hiding (get,put)
import qualified Control.Monad.State as State

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map(Map)

import Compiler.Hoopl hiding ((<*>)) -- conflicts with Control.Applicative
import qualified Compiler.Hoopl -- to access (<*>) when we need it

-- To serialize Unique and Label we are forced to use the internal Hoopl-GHC API.
import Compiler.Hoopl.GHC(uniqueToInt, uniqueToLbl, lblToUnique)

import Data.DeriveTH(derive)
import Data.Derive.Binary(makeBinary)
import Data.Binary
import Data.Binary.Put(PutM,runPut)
import Data.Binary.Get(Get,runGet)
import qualified Data.ByteString.Lazy as BS

import Lang.Php.Cfg.Types
import Lang.Php.Cfg.Utils(putIntCompact,getIntCompact)

instance Binary Unique where
  put = put . uniqueToInt
  get = intToUnique <$> get

instance Binary Label where
  put = put . lblToUnique
  get = uniqueToLbl <$> get

-- Instances of Binary for some types can be derived automatically. Unfortu-
-- -nately this doesn't work for GADTs.
$(derive makeBinary ''Visibility)
$(derive makeBinary ''Declarable)

------------------------------------------------------------------------------
--                       Serializable & caching
------------------------------------------------------------------------------
-- Serializable works just like Binary but uses additional state for the
-- equivalents of PutM and Get monads. We need this to implement caching and
-- avoid redundancy in the output.
--
-- This should be hidden from the outside of this module (standard interface
-- just uses Data.Binary).

type SGet = StateT (Map Word8 BS.ByteString) Get
type SPutM = StateT (Map BS.ByteString Word8, Word8) PutM

sGetInitial = Map.empty
sPutMInitial = (Map.empty, tagFirstSafeIndex)

tagNewEntry       = 1 :: Word8
tagFirstSafeIndex = 2 :: Word8

-- cachedPut inserts given data only if it is not already present earlier in
-- the serialization output (tracked by the monadic state). For repeated
-- data, only a single byte is appended -- an index that identifies a
-- cache entry. This index is always greater than 2 -- value 0 is reserved
-- and 1 is used to define new entries in the cache.
cachedPut :: PutM () -> SPutM ()
cachedPut putm = do
  let bstring = runPut putm
  (cache, safe_index) <- State.get
  case Map.lookup bstring cache of
    Just ident -> lift (putWord8 ident)
    Nothing -> do
      modify $ second incIndex -- increment the safe index
      modify $ first $ Map.insert bstring safe_index -- cache the result
      lift $ do
        putWord8 tagNewEntry
        putWord8 safe_index
        put bstring
 where
   incIndex x
     | x+1 == 0  = error "Cache index overflow during serialization"
     | otherwise = x+1

cachedGet :: Get a -> SGet a
cachedGet getm = do
  tag <- lift getWord8
  if tag == tagNewEntry
   then do
     -- this is a new entry in the cache
     index <- lift getWord8
     bstring <- lift (get :: Get BS.ByteString)
     modify (Map.insert index bstring)
     return $ runGet getm bstring
   else do
     -- tag is just an identifier in the cache
     cache <- State.get
     case Map.lookup tag cache of
       Nothing -> error "Malformed binary CFG representation"
       Just bstring -> return $ runGet getm bstring

class Serializable a where
  serialize :: a -> SPutM ()
  deserialize :: SGet a

instance Serializable (Maybe (FilePath, Int)) where
  serialize (Just (fp, pos)) = do
    lift (putWord8 1)
    cachedPut (put fp)
    lift (put pos)

  serialize Nothing = lift (putWord8 0)

  deserialize = do
    tag <- lift getWord8
    if tag == 1
     then do
       fp <- cachedGet get
       pos <- lift get
       return $ Just (fp, pos)
     else
       return Nothing

instance Serializable a => Serializable [a] where
  serialize xs = do
    lift $ put (length xs)
    mapM_ serialize xs

  deserialize = do
    n <- lift (get :: Get Int)
    replicateM n deserialize

instance Binary (Graph InstrPos C C) where
  put x = fst <$> runStateT (serialize x) sPutMInitial
  get = fst <$> runStateT deserialize sGetInitial

instance Binary (Graph InstrPos O O) where
  put x = fst <$> runStateT (serialize x) sPutMInitial
  get = fst <$> runStateT deserialize sGetInitial


------------------------------------------------------------------------------
--                               Register
------------------------------------------------------------------------------
-- Binary instance for Register could be derived automatically but this yields
-- a very verbose and inefficient representation. We do it manually using
-- putIntCompact/getIntCompact from Utils to compress the output even further.

opRNull = 78 :: Word8 -- letter 'N'
opRTemp = 84 :: Word8 -- letter 'T'
opRVar  = 86 :: Word8 -- letter 'V'

instance Binary Register where
  put RNull = putWord8 opRNull
  put (RTemp uniq) = putWord8 opRTemp >> putIntCompact (uniqueToInt uniq)
  put (RVar name) = putWord8 opRVar >> put name

  get = getWord8 >>= opGet
   where
     opGet op
       | op == opRNull = return RNull
       | op == opRTemp = RTemp <$> intToUnique <$> getIntCompact
       | op == opRVar  = RVar <$> get

------------------------------------------------------------------------------
--                        Opcodes for instructions                          --
------------------------------------------------------------------------------

opILabel           = 101 :: Word8
opIFuncEntry       = 102 :: Word8
opICatchException  = 103 :: Word8

opIJump            = 201 :: Word8
opICondJump        = 202 :: Word8
opIReturn          = 203 :: Word8
opIThrow           = 204 :: Word8

opICall            = 1   :: Word8
opILoadString      = 2   :: Word8
opILoadNum         = 3   :: Word8
opILoadConst       = 4   :: Word8
opICopyVar         = 5   :: Word8
opIDeclare         = 6   :: Word8
opIUnknown         = 7   :: Word8
opICallLabel       = 8   :: Word8

------------------------------------------------------------------------------
--                        Serialization of Instr                            --
------------------------------------------------------------------------------

-- Lots of ugly and duplicate code here. Since derive makeBinary doesn't work
-- for GADTs, I had to write the serialization code manually. This could have
-- been done more elegantly using TemplateHaskell but there is no time for
-- that now.

instance Binary (Instr C O) where
  put (ILabel l) = putWord8 opILabel >> put l
  put (IFuncEntry name args lab) = putWord8 opIFuncEntry
                                   >> put name >> put args >> put lab
  put (ICatchException lab reg name) = putWord8 opICatchException
                                       >> put lab >> put reg >> put name

  get = getWord8 >>= getOp
   where
    getOp :: Word8 -> Get (Instr C O)
    getOp x
      | x == opILabel          = ILabel <$> get
      | x == opIFuncEntry      = IFuncEntry <$> get <*> get <*> get
      | x == opICatchException = ICatchException <$> get <*> get <*> get


instance Binary (Instr O C) where
  put (IJump lbl) = putWord8 opIJump >> put lbl
  put (ICondJump reg lbl1 lbl2) = putWord8 opICondJump
                                  >> put reg >> put lbl1 >> put lbl2
  put (IReturn m_reg) = putWord8 opIReturn >> put m_reg
  put (IThrow reg) = putWord8 opIThrow >> put reg

  get = getWord8 >>= getOp
   where
    getOp :: Word8 -> Get (Instr O C)
    getOp x
      | x == opIJump     = IJump <$> get
      | x == opICondJump = ICondJump <$> get <*> get <*> get
      | x == opIReturn   = IReturn <$> get
      | x == opIThrow    = IThrow <$> get


instance Binary (Instr O O) where
  put (ICall res callable args) = putWord8 opICall >> putCall res callable args
  put (ICallLabel res callable args labels) = putWord8 opICallLabel >> putCall res callable args >> put labels
  put (ILoadString reg str) = putWord8 opILoadString >> put reg >> put str
  put (ILoadNum reg num) = putWord8 opILoadNum >> put reg >> put num
  put (ILoadConst reg str) = putWord8 opILoadConst >> put reg >> put str
  put (ICopyVar reg1 reg2) = putWord8 opICopyVar >> put reg1 >> put reg2
  put (IDeclare decl) = putWord8 opIDeclare >> put decl
  put IUnknown = putWord8 opIUnknown

  get = getWord8 >>= getOp
   where
    getOp :: Word8 -> Get (Instr O O)
    getOp x
      | x == opICall       = getCall
      | x == opILoadString = ILoadString <$> get <*> get
      | x == opILoadNum    = ILoadNum <$> get <*> get
      | x == opILoadConst  = ILoadConst <$> get <*> get
      | x == opICopyVar    = ICopyVar <$> get <*> get
      | x == opIDeclare    = IDeclare <$> get
      | x == opIUnknown    = return IUnknown
      | x == opICallLabel  = do
          instr <- getCall
          case instr of
            (ICall res callable args) -> do
              labels <- get
              return (ICallLabel res callable args labels)

------------------------------------------------------------------------------
--                         Opcodes for callables                            --
------------------------------------------------------------------------------

opCPrint   = 1 :: Word8
opCBitNot  = 2 :: Word8
opCClone   = 3 :: Word8
opCNegate  = 4 :: Word8
opCCast    = 5 :: Word8
opCEval    = 6 :: Word8
opCIsset   = 7 :: Word8
opCInclude = 8 :: Word8

opCErrorsSuppress = 71 :: Word8
opCErrorsRestore  = 72 :: Word8

opCEq     = 101 :: Word8
opCId     = 102 :: Word8
opCLe     = 103 :: Word8
opCLt     = 104 :: Word8
opCBitAnd = 105 :: Word8
opCConcat = 106 :: Word8
opCDiv    = 107 :: Word8
opCMod    = 108 :: Word8
opCMul    = 109 :: Word8
opCAdd    = 110 :: Word8
opCShiftL = 111 :: Word8
opCShiftR = 112 :: Word8
opCPow    = 113 :: Word8

opCItrGet     = 51 :: Word8
opCItrCurrent = 52 :: Word8
opCItrKey     = 53 :: Word8
opCItrValid   = 54 :: Word8
opCItrNext    = 55 :: Word8

opCPhp       = 31 :: Word8
opCPhpStatic = 32 :: Word8

opCArrayPush  = 91 :: Word8
opCArrayEmpty = 92 :: Word8
opCArrayGet   = 93 :: Word8
opCArraySet   = 94 :: Word8

------------------------------------------------------------------------------
--                       Serialization of Callable                          --
------------------------------------------------------------------------------

-- Similar to Instr in principle. Because of the type trickery involved in the
-- CFG, it would be inconvenient to write instances of Binary for callables.
-- Instead, we use putCall and getCall. Note that getCall returns Instr to
-- solve the typing problems.

putCall :: Show a => Register -> (Callable Register a) -> a -> Put

putCall res CPrint args  = gPC opCPrint res args
putCall res CBitNot args = gPC opCBitNot res args
putCall res CClone args  = gPC opCClone res args
putCall res CNegate args = gPC opCNegate res args

putCall res CErrorsSuppress args = gPC opCErrorsSuppress res args
putCall res CErrorsRestore args  = gPC opCErrorsRestore res args

putCall res CEq args     = gPC opCEq res args
putCall res CId args     = gPC opCId res args
putCall res CLe args     = gPC opCLe res args
putCall res CLt args     = gPC opCLt res args
putCall res CBitAnd args = gPC opCBitAnd res args
putCall res CConcat args = gPC opCConcat res args
putCall res CDiv args    = gPC opCDiv res args
putCall res CMod args    = gPC opCMod res args
putCall res CPow args    = gPC opCPow res args
putCall res CMul args    = gPC opCMul res args
putCall res CAdd args    = gPC opCAdd res args
putCall res CShiftL args = gPC opCShiftL res args
putCall res CShiftR args = gPC opCShiftR res args

putCall res CItrGet args     = gPC opCItrGet res args
putCall res CItrCurrent args = gPC opCItrCurrent res args
putCall res CItrKey args     = gPC opCItrKey res args
putCall res CItrValid args   = gPC opCItrValid res args
putCall res CItrNext args    = gPC opCItrNext res args

putCall res (CPhp name) args =
  putWord8 opCPhp >> put name >> put res >> put args

putCall res (CPhpStatic objs name) args =
  putWord8 opCPhpStatic >> put objs >> put name >> put res >> put args

putCall res CArrayPush args  = gPC opCArrayPush res args
putCall res CArrayEmpty args = gPC opCArrayEmpty res args
putCall res CArrayGet args   = gPC opCArrayGet res args
putCall res CArraySet args   = gPC opCArraySet res args

putCall res CCast args    = gPC opCCast res args
putCall res CEval args    = gPC opCEval res args
putCall res CIsset args   = gPC opCIsset res args
putCall res (CInclude incOrReq onceOrNot) args =
  putWord8 opCInclude >> put incOrReq >> put onceOrNot >> put res >> put args

gPC opcode res args = putWord8 opcode >> put res >> put args

getCall :: Get (Instr O O)
getCall = getWord8 >>= getOp
 where
   getOp x
     | x == opCPrint  = gGC CPrint
     | x == opCBitNot = gGC CBitNot
     | x == opCClone  = gGC CClone
     | x == opCNegate = gGC CNegate

     | x == opCErrorsSuppress = gGC CErrorsSuppress
     | x == opCErrorsRestore  = gGC CErrorsRestore

     | x == opCEq     = gGC CEq
     | x == opCId     = gGC CId
     | x == opCLe     = gGC CLe
     | x == opCLt     = gGC CLt
     | x == opCBitAnd = gGC CBitAnd
     | x == opCConcat = gGC CConcat
     | x == opCDiv    = gGC CDiv
     | x == opCMod    = gGC CMod
     | x == opCPow    = gGC CPow
     | x == opCMul    = gGC CMul
     | x == opCAdd    = gGC CAdd
     | x == opCShiftL = gGC CShiftL
     | x == opCShiftR = gGC CShiftR

     | x == opCItrGet     = gGC CItrGet
     | x == opCItrCurrent = gGC CItrCurrent
     | x == opCItrKey     = gGC CItrKey
     | x == opCItrValid   = gGC CItrValid
     | x == opCItrNext    = gGC CItrNext

     | x == opCPhp = do
         name <- get
         res <- get
         args <- get
         return (ICall res (CPhp name) args)

     | x == opCPhpStatic = do
         objs <- get
         name <- get
         res <- get
         args <- get
         return (ICall res (CPhpStatic objs name) args)

     | x == opCArrayPush  = gGC CArrayPush
     | x == opCArrayEmpty = gGC CArrayEmpty
     | x == opCArrayGet   = gGC CArrayGet
     | x == opCArraySet   = gGC CArraySet

     | x == opCCast    = gGC CCast
     | x == opCEval    = gGC CEval
     | x == opCIsset   = gGC CIsset
     | x == opCInclude = do
         incOrReq <- get
         onceOrNot <- get
         res <- get
         args <- get
         return (ICall res (CInclude incOrReq onceOrNot) args)

gGC callable = ICall <$> get <*> (return callable) <*> get

------------------------------------------------------------------------------
--                     Serialization of InstrPos
------------------------------------------------------------------------------

-- Also a bit ugly and repetitive since we need to give separate definitions
-- for different combinations of C/O parameters.

instance Serializable (InstrPos C O) where
  serialize (IP m_pos instr) = serialize m_pos >> lift (put instr)
  deserialize = IP <$> deserialize <*> lift get

instance Serializable (InstrPos O C) where
  serialize (IP m_pos instr) = serialize m_pos >> lift (put instr)
  deserialize = IP <$> deserialize <*> lift get

instance Serializable (InstrPos O O) where
  serialize (IP m_pos instr) = serialize m_pos >> lift (put instr)
  deserialize = IP <$> deserialize <*> lift get

------------------------------------------------------------------------------
--               Serialization of whole Hoopl graphs
------------------------------------------------------------------------------

tagGNil  = 1 :: Word8
tagGUnit = 2 :: Word8
tagGMany = 3 :: Word8

instance Serializable (Graph InstrPos C C) where
  serialize (GMany NothingO blockMap NothingO) = do
    let blocks = mapElems blockMap
    lift $ put (length blocks)
    mapM serializeBlockCC blocks
    return ()

  deserialize = do
    n <- lift (get :: Get Int)
    foldl (|*><*|) emptyClosedGraph <$> replicateM n deserializeBlockCC


instance Serializable (Graph InstrPos O O) where
  serialize GNil = lift $ putWord8 tagGNil

  serialize (GUnit block) = do
    lift $ putWord8 tagGUnit
    serializeBlockOO block

  serialize (GMany (JustO entry) blockMap (JustO exit)) = do
    lift $ putWord8 tagGMany
    serializeBlockOC entry
    serialize (GMany NothingO blockMap NothingO)
    serializeBlockCO exit

  deserialize = lift get >>= withTag
   where
     withTag tag
       | tag == tagGNil  = return GNil
       | tag == tagGUnit = deserializeBlockOO
       | tag == tagGMany = do
           entry <- deserializeBlockOC
           middle <- deserialize :: SGet (Graph InstrPos C C)
           exit <- deserializeBlockCO
           return (entry |*><*| middle |*><*| exit)

serializeBlockOO :: Block InstrPos O O -> SPutM ()
serializeBlockOO block = case blockToNodeList block of
  (NothingC, nodes, NothingC) -> serialize nodes

serializeBlockOC :: Block InstrPos O C -> SPutM ()
serializeBlockOC block = case blockToNodeList block of
  (NothingC, nodes, JustC last) -> do
    serialize nodes
    serialize last

serializeBlockCO :: Block InstrPos C O -> SPutM ()
serializeBlockCO block = case blockToNodeList block of
  (JustC first, nodes, NothingC) -> do
    serialize first
    serialize nodes

serializeBlockCC :: Block InstrPos C C -> SPutM ()
serializeBlockCC block = case blockToNodeList block of
  (JustC first, middle, JustC last) -> do
    serialize first
    serialize middle
    serialize last

deserializeBlockOO :: SGet (Graph InstrPos O O)
deserializeBlockOO = do
  nodes <- deserialize :: SGet [InstrPos O O]
  let mNodes = map mkMiddle nodes
  return (foldl (Compiler.Hoopl.<*>) emptyGraph mNodes)

deserializeBlockOC :: SGet (Graph InstrPos O C)
deserializeBlockOC = do
  g_middle <- deserializeBlockOO
  last <- deserialize :: SGet (InstrPos O C)
  return ((Compiler.Hoopl.<*>) g_middle (mkLast last))

deserializeBlockCO :: SGet (Graph InstrPos C O)
deserializeBlockCO = do
  first <- deserialize :: SGet (InstrPos C O)
  g_middle <- deserializeBlockOO
  return ((Compiler.Hoopl.<*>) (mkFirst first) g_middle)

deserializeBlockCC :: SGet (Graph InstrPos C C)
deserializeBlockCC = do
  first <- deserialize :: SGet (InstrPos C O)
  g_rest <- deserializeBlockOC
  return ((Compiler.Hoopl.<*>) (mkFirst first) g_rest)
