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
{-
Copyright (c) 2009, Dan Corson, Sasha Rush, Facebook, inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of Facebook, inc. nor the names of its contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
-}

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}

module Lang.Php.Ast.Unparse where

import Control.Monad.Identity
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import Lang.Php.Ast.Types
import qualified Data.Intercal as IC
import Data.Either

--
-- Statements
--

instance Unparse Stmt where
  unparse stmt = case stmt of
    StmtBlock a -> unparse a
    StmtBreak iMb w end -> tokBreak ++ unparse iMb ++ unparse w ++ unparse end
    StmtClass a -> unparse a
    StmtContinue iMb w end -> tokContinue ++ unparse iMb ++ unparse w ++
      unparse end
    StmtDeclare a -> unparse a
    StmtDoWhile a -> unparse a
    StmtEcho a end -> tokEcho ++ intercalate tokComma (map unparse a) ++
      unparse end
    StmtExpr a b c -> unparse a ++ unparse b ++ unparse c
    StmtFor a -> unparse a
    StmtForeach a -> unparse a
    StmtFuncDef a -> unparse a
    StmtGlobal a end -> tokGlobal ++
      intercalate tokComma (map unparse a) ++ unparse end
    StmtGoto a end -> tokGoto ++ unparse a ++ unparse end
    StmtIf a -> unparse a
    StmtInterface a -> unparse a
    StmtLabel a -> unparse a ++ tokColon
    StmtNothing end -> unparse end
    StmtNamespace namespace -> tokNamespace ++ unparse namespace
    StmtReturn rMb w end -> tokReturn ++ unparse rMb ++ unparse w ++
      unparse end
    StmtStatic a end -> tokStatic ++ intercalate tokComma (map unparse a) ++
      unparse end
    StmtSwitch a -> unparse a
    StmtThrow a end -> tokThrow ++ unparse a ++ unparse end
    StmtTry a cs -> tokTry ++ unparse a ++ unparse cs
    StmtUnset (WSCap w1 a w2) end -> tokUnset ++ unparse w1 ++ tokLParen ++
      intercalate tokComma (map unparse a) ++ tokRParen ++ unparse w2 ++
      unparse end
    StmtUse a b c -> tokUse ++ unparse a ++ unparse b ++
      case c of
        Just ((ws1,ws2),name) -> unparse ws1 ++ tokAs ++ unparse ws2 ++
          unparse name
        otherwise -> ""
    StmtWhile a -> unparse a

instance Unparse StmtEnd where
  unparse StmtEndSemi = tokSemi
  unparse (StmtEndClose a) = tokClosePhp ++ unparse a

instance Unparse TopLevel where
  unparse (TopLevel s echoOrTok) = s ++
    maybe "" (either ((tokOpenPhpEcho ++) . unparse) ("<?" ++)) echoOrTok

instance (Unparse a) => Unparse (Block a) where
  unparse (Block a) = tokLBrace ++ unparse a ++ tokRBrace

unparsePre :: [(String, WS)] -> String
unparsePre = concatMap (\ (a, b) -> a ++ unparse b)

instance Unparse Class where
  unparse (Class pre (WSCap w1 name w2) extends impls block) = concat [
    unparsePre pre, tokClass, unparse w1, name, unparse w2,
    maybe [] ((tokExtends ++) . unparse) extends,
    if null impls then []
      else tokImplements ++ intercalate tokComma (map unparse impls),
    unparse block]

instance Unparse ClassStmt where
  unparse stmt = case stmt of
    CStmtVar pre a end -> IC.intercalUnparser id unparse pre ++
      intercalate tokComma (map unparse a) ++ unparse end
    CStmtConst a -> cStmtConstUnparser a
    CStmtFuncDef pre a -> unparsePre pre ++ unparse a
    CStmtAbstrFunc a -> unparse a
    CStmtCategory a -> tokCategory ++ a ++ tokSemi
    CStmtChildren a -> tokChildren ++ a ++ tokSemi
    CStmtAttribute a -> tokAttribute ++ a ++ tokSemi

cStmtConstUnparser :: (Unparse a) => [a] -> String
cStmtConstUnparser vars = tokConst ++
  intercalate tokComma (map unparse vars) ++ tokSemi

instance Unparse AbstrFunc where
  unparse (AbstrFunc pre ref name args ws end) = concat [unparsePre pre,
    tokFunction, maybe "" ((++ tokAmp) . unparse) ref, unparse name, tokLParen,
    either unparse (intercalate tokComma . map unparse) args, tokRParen,
    unparse ws, unparse end]

instance (Unparse a) => Unparse (VarEqVal a) where
  unparse (VarEqVal var w expr) = unparse var ++ w2With tokEquals w ++
    unparse expr

instance Unparse FuncArg where
  unparse (FuncArg const refWs var) = concat [
    maybe [] (\ (c, w) -> maybe tokArray unparse c ++ unparse w) const,
    maybe [] ((tokAmp ++) . unparse) refWs, unparse var]

-- todo: the block form too?  does anyone use it?  declare is terrible anyway..
instance Unparse Declare where
  unparse (Declare (WSCap w1 (name, expr) w2) end) = concat [tokDeclare,
    unparse w1, tokLParen, unparse name, tokEquals, unparse expr, tokRParen,
    unparse w2, unparse end]

instance Unparse DoWhile where
  unparse (DoWhile block (WSCap w1 (WSCap w2 expr w3) w4) end) = concat [tokDo,
    unparse block, tokWhile, unparse w1, tokLParen, unparse w2, unparse expr,
    unparse w3, tokRParen, unparse w4, unparse end]

instance Unparse For where
  unparse (For (WSCap w1 (inits, conds, incrs) w2) block StdSyntax) = concat [
    tokFor, unparse w1, tokLParen,
    intercalate tokSemi $ map unparse [inits, conds, incrs],
    tokRParen, unparse w2, unparse block]
  unparse (For (WSCap w1 (inits, conds, incrs) w2) block AltSyntax) =
    concat [tokFor, unparse w1, tokLParen,
        intercalate tokSemi $ map unparse [inits, conds, incrs],
        tokRParen, unparse w2, tokColon, " ", unparse block, tokEndfor]

instance Unparse ForPart where
  unparse (ForPart e) = either unparse (intercalate tokComma . map unparse) e

instance Unparse Foreach where
  unparse (Foreach (WSCap w1 (expr, lvalKey, lvalVal) w2) block syntax) = 
    case syntax of
      StdSyntax -> concat [tokForeach, unparse w1, tokLParen, unparse expr,
        tokAs, unparserlVar lvalKey, unparse lvalVal, tokRParen, unparse w2,
        unparse block]
      AltSyntax -> concat [tokForeach, unparse w1, tokLParen, unparse expr,
        tokAs, unparserlVar lvalKey, unparse lvalVal, tokRParen, unparse w2,
        tokColon, " ", unparse block, tokEndforeach]
   where
    unparserlVar (Just x) = unparse x ++ tokDubArrow
    unparserlVar _ = ""

instance Unparse Func where
  unparse (Func w1 ref name (WSCap w2 args w3) useVars block) = concat [tokFunction,
    unparse w1, maybe [] ((tokAmp ++) . unparse) ref, unparse name, unparse w2,
    tokLParen, argsUnparser args,
    maybe "" (\(WSCap _ args _) -> tokUse ++ argsUnparser args) useVars,
    tokRParen, unparse w3, unparse block]

argsUnparser :: (Unparse t, Unparse s) => Either t [s] -> String
argsUnparser = either unparse (intercalate tokComma . map unparse)

instance Unparse If where
  unparse (If syntax ifElseblocks elseBlock) =
    concat [unparseBlocks syntax ifElseblocks,
      unparseElse syntax elseBlock]
   where
    unparseBlocks :: StmtSyntax -> [IfBlock] -> String
    unparseBlocks syntax xs = 
      concat [tokIf, unparseBlock syntax $ head xs] ++
      (concatMap unparse
        [(tokElseif, unparseBlock syntax x) | x <- tail xs]) ++
      if syntax == AltSyntax then tokEndif else ""


    unparseBlock :: StmtSyntax -> IfBlock -> String
    unparseBlock syntax (IfBlock (WSCap w1 (w2, expr) w3) w4 (bs, w5)) =
      concat [unparse w1, tokLParen, unparse w2, unparse expr,
        unparse w3, tokRParen, unparse w4, 
        if syntax == AltSyntax then tokColon else "",
        unparse bs, unparse w5]

    unparseElse :: StmtSyntax -> Maybe (BlockOrStmt, WS) -> String
    unparseElse syntax (Just (block, ws)) =
      concat [tokElse,
        if syntax == AltSyntax then tokColon else "",
        unparse block, unparse ws]
    unparseElse syntax Nothing = ""

instance Unparse Interface where
  unparse (Interface name extends block) = concat [tokInterface, unparse name,
    if null extends then []
      else tokExtends ++ intercalate tokComma (map unparse extends),
    unparse block]

instance Unparse IfaceStmt where
  unparse (IfaceConst vars) = cStmtConstUnparser vars
  unparse (IfaceFunc a) = unparse a

instance Unparse Label where
  unparse (Label a) = a

instance Unparse VarMbVal where
  unparse (VarMbVal var exprMb) = unparse var ++ maybe []
    (\ (w, expr) -> w2With tokEquals w ++ unparse expr) exprMb

instance Unparse Switch where
  unparse (Switch syntax (WSCap w1 expr w2) w3 cases tl) =
    concat [tokSwitch,
            unparse w1, tokLParen, unparse expr, tokRParen, unparse w2,
            left, unparse w3, unparse tl, unparse cases, right]
   where
    (left, right) = case syntax of
        StdSyntax -> (tokLBrace, tokRBrace)
        AltSyntax -> (tokColon, tokEndswitch)

instance Unparse Case where
  unparse (Case expr stmtList) =
    either ((tokDefault ++) . unparse) ((tokCase ++) . unparse) expr ++
    tokColon ++ unparse stmtList

instance Unparse Catch where
  unparse (Catch (WSCap w1 (const, expr) w2) w3 block) = concat [tokCatch,
    unparse w1, tokLParen, unparse const, unparse expr,
    unparse w2, tokRParen, unparse w3, unparse block]

instance Unparse While where
  unparse (While (WSCap w1 expr w2) block StdSyntax) =
    concat [tokWhile, unparse w1, tokLParen, unparse expr, tokRParen,
            unparse w2, unparse block]
  
  unparse (While (WSCap w1 expr w2) block AltSyntax) =
    concat [tokWhile, unparse w1, tokLParen, unparse expr, tokRParen,
            unparse w2, tokColon, unparse block, " ", tokEndwhile, " "]

instance Unparse TernaryIf where
  unparse (TernaryIf e1 (w1, w2) e2 (w3, w4) e3) = unparse e1 ++ unparse w1 ++
    tokQMark ++ unparse w2 ++ unparse e2 ++ unparse w3 ++ tokColon ++
    unparse w4 ++ unparse e3

instance Unparse StrLit where
  unparse (StrLit ic) = start ++ concatMap f xs
   where
    (start, xs) = IC.breakStart ic
    f ((SLEComplex, e), str) = "{" ++ unparse e ++ "}" ++ str
    f ((SLENormal, e), str) = unparse e ++ str

instance Unparse Namespace where
    unparse (Namespace a b) = unparse a ++ unparse b

---
--- EXPRESSIONS
---

instance Unparse Var where
  unparse (Var s indexes) = tokDollar ++ s ++
    concatMap (\ (ws, (isBracket, expr)) -> unparse ws ++
      if isBracket
        then tokLBracket ++ unparse expr ++ tokRBracket
        else tokLBrace ++ unparse expr ++ tokRBrace
      ) indexes
  unparse (VarDyn ws var) = tokDollar ++ unparse ws ++ unparse var
  unparse (VarDynExpr ws expr) = tokDollar ++ unparse ws ++ tokLBrace ++
    unparse expr ++ tokRBrace

unparseConstOrDynConst statics s =
    (intercalate tokDubColon . map unparse) (statics ++ [s])

instance Unparse Const where
  unparse (Const statics s) = 
    unparseConstOrDynConst statics s

instance Unparse DynConst where
  unparse (DynConst statics var) =
    unparseConstOrDynConst statics var

instance Unparse LRVal where
  unparse (LRValVar a) = unparse a
  unparse (LRValInd a w e) = unparse a ++ unparse w ++ tokLBracket ++
    unparse e ++ tokRBracket
  unparse (LRValMemb v (ws1, ws2) m) =
    unparse v ++ unparse ws1 ++ tokArrow ++ unparse ws2 ++ unparse m

instance Unparse LOnlyVal where
  unparse (LOnlyValList w args) = tokList ++ unparse w ++ tokLParen ++
    either unparse (intercalate tokComma . map unparse) args ++ tokRParen
  unparse (LOnlyValAppend v (ws1, ws2)) =
    unparse v ++ unparse ws1 ++ tokLBracket ++ unparse ws2 ++ tokRBracket
  unparse (LOnlyValInd v ws expr) =
    unparse v ++ unparse ws ++ tokLBracket ++ unparse expr ++ tokRBracket
  unparse (LOnlyValMemb v (ws1, ws2) m) =
    unparse v ++ unparse ws1 ++ tokArrow ++ unparse ws2 ++ unparse m

instance Unparse ROnlyVal where
  unparse (ROnlyValConst a) = unparse a
  unparse (ROnlyValFunc v ws (Left w)) = unparse v ++ unparse ws ++
    tokLParen ++ unparse w ++ tokRParen
  unparse (ROnlyValFunc v ws (Right args)) = unparse v ++ unparse ws ++
    tokLParen ++ intercalate tokComma (map unparse args) ++ tokRParen

instance Unparse Memb where
  unparse (MembExpr e) = tokLBrace ++ unparse e ++ tokRBrace
  unparse (MembStr s) = s
  unparse (MembVar a) = unparse a

instance Unparse Val where
  unparse (ValLOnlyVal a) = unparse a
  unparse (ValROnlyVal a) = unparse a
  unparse (ValLRVal a) = unparse a

instance Unparse LVal where
  unparse (LValLOnlyVal a) = unparse a
  unparse (LValLRVal a) = unparse a

instance Unparse RVal where
  unparse (RValROnlyVal a) = unparse a
  unparse (RValLRVal a) = unparse a

instance Unparse NewDoc where
  unparse (NewDoc a) = tokHereDoc ++ a

instance Unparse HereDoc where
  unparse (HereDoc a) = tokHereDoc ++ show a

-- Expr

instance Unparse Expr where
  unparse expr = case expr of
    ExprArray w elemsOrW -> tokArray ++ unparse w ++ tokLParen ++
      either unparse f elemsOrW ++ tokRParen where
      f (elems, wEnd) = intercalate tokComma .
        maybe id (flip (++) . (:[]) . unparse) wEnd $ map unparse elems
    ExprAssign o v w e -> unparse v ++ w2With (unparse o ++ tokEquals) w ++
      unparse e
    ExprBackticks a -> unparse a
    ExprBinOp o e1 (w1, w2) e2 -> unparse e1 ++ unparse w1 ++ unparse o ++
      unparse w2 ++ unparse e2
    ExprCast (WSCap w1 t w2) w e -> tokLParen ++ unparse w1 ++ t ++
      unparse w2 ++ tokRParen ++ unparse w ++ unparse e
    ExprClosure a -> unparse a
    ExprEmpty w e -> tokEmpty ++ unparse w ++ tokLParen ++ unparse e ++
      tokRParen
    ExprEval w e -> tokEval ++ unparse w ++ tokLParen ++ unparse e ++
      tokRParen
    ExprExit isExit a -> (if isExit then tokExit else tokDie) ++
      maybe "" (\ (w, x) -> unparse w ++ tokLParen ++
        either unparse unparse x ++ tokRParen) a
    ExprHereDoc a -> unparse a
    ExprInclude a b w e -> unparse a ++ unparse b ++ unparse w ++ unparse e
    ExprIndex a w b ->
      unparse a ++ unparse w ++ tokLBracket ++ unparse b ++ tokRBracket
    ExprInstOf e w t -> unparse e ++ w2With tokInstanceof w ++ unparse t
    ExprIsset w vs -> tokIsset ++ unparse w ++ tokLParen ++
      intercalate tokComma (map unparse vs) ++ tokRParen
    ExprNew w a argsMb -> tokNew ++ unparse w ++ unparse a ++ maybe ""
      (\ (wPre, args) -> unparse wPre ++ tokLParen ++ either unparse
        (intercalate tokComma . map unparse) args ++ tokRParen) argsMb
    ExprNumLit a -> unparse a
    ExprParen a -> tokLParen ++ unparse a ++ tokRParen
    ExprPostOp o e w -> unparse e ++ unparse w ++ unparse o
    ExprPreOp o w e -> unparse o ++ unparse w ++ unparse e
    ExprRef r -> unparse r
    ExprRVal a -> unparse a
    ExprStrLit a -> unparse a
    ExprTernaryIf a -> unparse a

instance Unparse Ref where
  unparse (Ref w v) = tokAmp ++ unparse w ++ unparse v

instance Unparse BinOpBy where
  unparse binOp = case binOp of
    BBitAnd -> tokAmp
    BBitOr -> tokBitOr
    BConcat -> tokConcat
    BDiv -> tokDiv
    BMinus -> tokMinus
    BMod -> tokMod
    BMul -> tokMul
    BPlus -> tokPlus
    BShiftL -> tokShiftL
    BShiftR -> tokShiftR
    BXor -> tokXor

instance Unparse BinOp where
  unparse binOp = case binOp of
    BAnd -> tokAnd
    BAndWd -> tokAndWd
    BEQ -> tokEQ
    BGE -> tokGE
    BGT -> tokGT
    BID -> tokID
    BLE -> tokLE
    BLT -> tokLT
    BNE -> tokNE
    BNEOld -> tokNEOld
    BNI -> tokNI
    BOr -> tokOr
    BOrWd -> tokOrWd
    BXorWd -> tokXorWd
    BByable o -> unparse o

instance Unparse PreOp where
  unparse preOp = case preOp of
    PrPrint -> tokPrint
    PrAt -> tokAt
    PrBitNot -> tokBitNot
    PrClone -> tokClone
    PrNegate -> tokMinus
    PrNot -> tokNot
    PrPos -> tokPlus
    PrSuppress -> tokAt
    PrIncr -> tokIncr
    PrDecr -> tokDecr

instance Unparse PostOp where
  unparse postOp = case postOp of
    PoIncr -> tokIncr
    PoDecr -> tokDecr

instance Unparse IncOrReq where
  unparse Inc = tokInclude
  unparse Req = tokRequire

instance Unparse OnceOrNot where
  unparse Once = "_once"
  unparse NotOnce = ""

instance Unparse DubArrowMb where
  unparse (DubArrowMb k v) = maybe "" (\ (e, (w1, w2)) -> unparse e ++
    unparse w1 ++ tokDubArrow ++ unparse w2) k ++ unparse v
