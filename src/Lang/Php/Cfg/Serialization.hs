{-# LANGUAGE GADTs, TemplateHaskell, FlexibleInstances #-}

module Lang.Php.Cfg.Serialization where

import Control.Applicative
import Control.Monad(replicateM)

import Compiler.Hoopl hiding ((<*>)) -- conflicts with Control.Applicative
import qualified Compiler.Hoopl -- to access (<*>) when we need it

-- To serialize Unique and Label we are forced to use the internal Hoopl-GHC API.
import Compiler.Hoopl.GHC(uniqueToInt, uniqueToLbl, lblToUnique)

import Data.DeriveTH(derive)
import Data.Derive.Binary(makeBinary)
import Data.Binary

import Lang.Php.Cfg.Types

instance Binary Unique where
  put = put . uniqueToInt
  get = intToUnique <$> get

instance Binary Label where
  put = put . lblToUnique
  get = uniqueToLbl <$> get

-- Instances of Binary for some types can be derived automatically. Unfortu-
-- -nately this doesn't work for GADTs.
$(derive makeBinary ''Register)
$(derive makeBinary ''Visibility)
$(derive makeBinary ''Declarable)

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
  put (ILoadString reg str) = putWord8 opILoadString >> put reg >> put str
  put (ILoadNum reg num) = putWord8 opILoadNum >> put reg >> put num
  put (ILoadConst reg str) = putWord8 opILoadConst >> put reg >> put str
  put (ICopyVar reg1 reg2) = putWord8 opICopyVar >> put reg1 >> put reg2
  put (IDeclare decl) = putWord8 opIDeclare >> put decl

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

instance Binary (InstrPos C O) where
  put (IP m_pos instr) = put m_pos >> put instr
  get = IP <$> get <*> get

instance Binary (InstrPos O C) where
  put (IP m_pos instr) = put m_pos >> put instr
  get = IP <$> get <*> get

instance Binary (InstrPos O O) where
  put (IP m_pos instr) = put m_pos >> put instr
  get = IP <$> get <*> get

------------------------------------------------------------------------------
--               Serialization of whole Hoopl graphs
------------------------------------------------------------------------------

tagGNil  = 1 :: Word8
tagGUnit = 2 :: Word8
tagGMany = 3 :: Word8

instance Binary (Graph InstrPos O O) where
  put GNil = putWord8 tagGNil
  put (GUnit block) = putWord8 tagGUnit >> putBlockOO block
  put (GMany (JustO entry) blockMap (JustO exit)) = do
    putWord8 tagGMany
    let blocks = mapElems blockMap
    putBlockOC entry
    put (length blocks)
    mapM putBlockCC blocks
    putBlockCO exit

  get = get >>= getTag
   where
     getTag tag
       | tag == tagGNil  = return GNil
       | tag == tagGUnit = getBlockOO
       | tag == tagGMany = do
           entry <- getBlockOC
           n <- get :: Get Int
           middle <- foldl (|*><*|) emptyClosedGraph <$> replicateM n getBlockCC
           exit <- getBlockCO
           return (entry |*><*| middle |*><*| exit)

putBlockOO :: Block InstrPos O O -> Put
putBlockOO block = case blockToNodeList block of
  (NothingC, nodes, NothingC) -> put nodes

putBlockOC :: Block InstrPos O C -> Put
putBlockOC block = case blockToNodeList block of
  (NothingC, nodes, JustC last) -> put nodes >> put last

putBlockCO :: Block InstrPos C O -> Put
putBlockCO block = case blockToNodeList block of
  (JustC first, nodes, NothingC) -> put first >> put nodes

putBlockCC :: Block InstrPos C C -> Put
putBlockCC block = case blockToNodeList block of
  (JustC first, middle, JustC last) -> put first >> put middle >> put last

getBlockOO :: Get (Graph InstrPos O O)
getBlockOO = do
  nodes <- get :: Get [InstrPos O O]
  let mNodes = map mkMiddle nodes
  return (foldl (Compiler.Hoopl.<*>) emptyGraph mNodes)

getBlockOC :: Get (Graph InstrPos O C)
getBlockOC = do
  g_middle <- getBlockOO
  last <- get :: Get (InstrPos O C)
  return ((Compiler.Hoopl.<*>) g_middle (mkLast last))

getBlockCO :: Get (Graph InstrPos C O)
getBlockCO = do
  first <- get :: Get (InstrPos C O)
  g_middle <- getBlockOO
  return ((Compiler.Hoopl.<*>) (mkFirst first) g_middle)

getBlockCC :: Get (Graph InstrPos C C)
getBlockCC = do
  first <- get :: Get (InstrPos C O)
  g_rest <- getBlockOC
  return ((Compiler.Hoopl.<*>) (mkFirst first) g_rest)
