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

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Lang.Php.Ast.Types where

import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import qualified Data.Intercal as IC


---
--- EXPRESSIONS
---

-- Val's are defined to only contain: "$", identifiers, "[Expr]", "[]",
-- "(Exprs)", "${Expr}", "::", "->".  The most important consideration is which
-- ones can be assigned to (LVal's) and which ones can be assigned from
-- (RVal's).  In PHP, most but not all LVal's are also RVal's.

-- Note that this grammar allows "$$a[]->a = 5;" but Zend does not.  However,
-- Zend allows "${$a}[]->a = 5;", and it's not clear what is gained by treating
-- $a and ${..} asymmetrically here.  PHP also allows "${$a}[0]->a = 5" and
-- "$$a[0]->a = 5;".  So we're regarding this as a by-product of the Zend
-- implementation.  In particular, we think they simplify their job by slurping
-- all [Expr?]'s onto Var's and only later analyze things with regard to LVal
-- considerations, simply fataling if something is then awry.
--
-- Modeling that nuance is impractical under the clear division of
-- Var's, LVal's, and RVal's that we desire to make the AST nice for
-- refactoring.

data StrLitExprStyle = SLENormal | SLEComplex -- | "$foo" vs "{$foo}"
  deriving (Eq, Show, Typeable, Data)

data StrLit = StrLit (IC.Intercal String (StrLitExprStyle, RVal))
  deriving (Eq, Show, Typeable, Data)

strLitAsSimple :: StrLit -> Maybe String
strLitAsSimple (StrLit (IC.Interend x)) = Just x
strLitAsSimple _ = Nothing

data NewDoc = NewDoc String String
  deriving (Eq, Show, Typeable, Data)

data HereDoc = HereDoc StrLit String
  deriving (Eq, Show, Typeable, Data)

data Val = ValLOnlyVal LOnlyVal | ValROnlyVal ROnlyVal | ValLRVal LRVal
  deriving (Eq, Show, Typeable, Data)

data LVal = LValLOnlyVal LOnlyVal | LValLRVal LRVal
  deriving (Eq, Show, Typeable, Data)

data RVal = RValROnlyVal ROnlyVal | RValLRVal LRVal
  deriving (Eq, Show, Typeable, Data)

data Var =
  -- In php, indexing is oddly coupled very tightly with being a non-dyn var.
  Var        String [((WS, (Bool, WSCap Expr)))] | -- "$a", "$a[0]", "$a[0][0]"
  VarDyn     WS Var          | -- "$$a"
                               -- note: "$$a[0]()->a" == "${$a[0]}()->a"
  VarDynExpr WS (WSCap Expr)   -- "${$a . '_'}"
  deriving (Eq, Show, Typeable, Data)

data DynConst = 
  -- "$a::$a" etc.
  DynConst [WSCap (Either String Var)] (WSCap (Either String Var))
  deriving (Eq, Show, Typeable, Data)

data LRVal =
  LRValVar  DynConst |
  LRValInd  RVal WS (WSCap Expr) | -- "$a->a[0]"
  LRValMemb RVal WS2 Memb -- "$a->a"
  deriving (Eq, Show, Typeable, Data)

data LOnlyVal =
  LOnlyValList   WS (Either WS [Either WS (WSCap LVal)]) |
  LOnlyValAppend LVal WS2                 | -- "$a[]"
  LOnlyValInd    LOnlyVal WS (WSCap Expr) | -- "$a[][0]"
  LOnlyValMemb   LOnlyVal WS2 Memb          -- "$a[]->a"
  deriving (Eq, Show, Typeable, Data)

data Const = Const [WSCap String] (WSCap String) -- "a::a"
  deriving (Eq, Show, Typeable, Data)

data ROnlyVal =
  ROnlyValConst Const |
  -- "a()", "$a()"
  ROnlyValFunc  (Either LRVal Const) WS (Either WS [WSCap (Either Expr LVal)])
  deriving (Eq, Show, Typeable, Data)

data Memb =
  MembStr  String |
  MembVar  Var    |
  MembExpr (WSCap Expr)
  deriving (Eq, Show, Typeable, Data)

-- Expr's

data Expr =
  ExprArray     WS (Either WS ([WSCap DubArrowMb], Maybe WS)) |
  ExprAssign    (Maybe BinOpBy) LVal WS2 Expr |
  ExprBackticks StrLit |
  ExprBinOp     BinOp Expr WS2 Expr |
  -- we're lazy so just String here instead of like PhpType
  ExprCast      (WSCap String) WS Expr |
  ExprClosure   Func |
  ExprEmpty     WS (WSCap Expr) |
  ExprEval      WS (WSCap Expr) |
  ExprExit      Bool (Maybe (WS, Either WS (WSCap Expr))) |
  ExprHereDoc   HereDoc |
  -- FIXME: this fb extension should be separated to a superclass-like Lang?
  ExprIndex     Expr WS (WSCap Expr) |
  ExprInclude   IncOrReq OnceOrNot WS Expr |
  -- true story: "instanceof" takes LRVal's but not non-Const ROnlyVal's..
  ExprInstOf    Expr WS2 (Either LRVal Const) |
  ExprIsset     WS [WSCap LRVal] |
  ExprNew       WS RVal (Maybe (WS, Either WS [WSCap Expr])) |
  ExprNewDoc    NewDoc |
  ExprNumLit    NumLit |
  ExprParen     (WSCap Expr) |
  ExprPostOp    PostOp Expr WS |
  ExprPreOp     PreOp WS Expr |
  -- note: "list"/"&" is actually more limited
  -- ("list() = &$a;" is nonsyntactic)
  ExprRef       Ref | 
  ExprRVal      RVal |
  ExprStrLit    StrLit |
  ExprTernaryIf TernaryIf |
  ExprYield     WS (Maybe (Expr, WS))
  deriving (Eq, Show, Typeable, Data)

data Ref = Ref {
  refWS :: WS,
  ref   :: (Either Expr Val)}
  deriving (Eq, Show, Typeable, Data)

data BinOp = BAnd | BAndWd | BEQ | BGE | BGT | BID | BLE | BLT | BNE |
  -- <> has different precedence than !=
  BNEOld | BNI | BOr | BOrWd | BXorWd | BByable BinOpBy
  deriving (Eq, Show, Typeable, Data)

data BinOpBy = BBitAnd | BBitOr | BConcat | BDiv | BMinus | BMod | BPow |
  BMul | BPlus | BShiftL | BShiftR | BXor
  deriving (Eq, Show, Typeable, Data)

data PreOp = PrPrint | PrAt | PrBitNot | PrClone | PrNegate | PrNot | PrPos |
  PrSuppress | PrIncr | PrDecr
  deriving (Eq, Show, Typeable, Data)

data PostOp = PoIncr | PoDecr
  deriving (Eq, Show, Typeable, Data)

data IncOrReq = Inc | Req
  deriving (Eq, Show, Typeable, Data)

data OnceOrNot = Once | NotOnce
  deriving (Eq, Show, Typeable, Data)

data TernaryIf = TernaryIf {
  ternaryIfCond :: Expr,
  ternaryIfWS1  :: WS2,
  ternaryIfThen :: Maybe Expr, -- since PHP 5.3 this may be omitted
  ternaryIfWS2  :: WS2,
  ternaryIfElse :: Expr}
  deriving (Eq, Show, Typeable, Data)

data DubArrowMb = DubArrowMb (Maybe (Expr, WS2)) Expr
  deriving (Eq, Show, Typeable, Data)

---
---  STATEMENTS
--- 

type StmtList = IC.Intercal WS (StoredPos Stmt)

data Stmt =
  StmtBlock     (Block Stmt)                  |
  StmtBreak     (Maybe (WS, Expr)) WS StmtEnd |
  StmtClass     Class                         |
  StmtConst     (WSCap (VarEqVal Const)) StmtEnd|
  StmtContinue  (Maybe (WS, Expr)) WS StmtEnd |
  StmtDeclare   Declare                       |
  StmtDoWhile   DoWhile                       |
  -- this list must have at least one element.. should i make a type for that?
  StmtEcho      [WSCap Expr] StmtEnd          |
  StmtExpr      Expr WS StmtEnd               |
  StmtFor       For                           |
  StmtForeach   Foreach                       |
  StmtFuncDef   Func                          |
  -- this list must have at least one element.. should i make a type for that?
  StmtGlobal    [WSCap Var] StmtEnd             |
  StmtGoto      (WSCap Label) StmtEnd           |
  -- We keep rest of script in StmtHaltCompiler (it's pretty weird construct)
  -- http://php.net/manual/en/function.halt-compiler.php
  StmtHaltCompiler WS StmtEnd String            |
  StmtIf        If                              |
  StmtInterface Interface                       |
  StmtLabel     (WSCap Label)                   |
  StmtNothing   StmtEnd                         |
  StmtNamespace Namespace                       |
  StmtReturn    WS (Maybe (Expr, WS)) StmtEnd   |
  -- this list must have at least one element.. should i make a type for that?
  StmtStatic    [WSCap VarMbVal] StmtEnd        |
  StmtSwitch    Switch                          |
  StmtThrow     (WSCap Expr) StmtEnd            |
  StmtTry       (WSCap (Block Stmt)) -- try block
                  (IC.Intercal Catch WS) -- [catch] blocks
                  (Maybe (WS2, (Block Stmt))) -- finally block
                                                |
  StmtUnset     (WSCap [WSCap LRVal]) StmtEnd   |
  StmtUse       WS (Maybe (String, WS)) String
                  (Maybe (WS2, String))         |
  StmtWhile     While
  deriving (Eq, Show, Typeable, Data)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS (StoredPos a))
  deriving (Eq, Show, Typeable, Data)

data Func = Func {
  funcWS      :: WS,
  funcRef     :: Maybe WS,
  funcName    :: Maybe String,
  funcArgs    :: WSCap (Either WS [WSCap FuncArg]),
  funcUseVars :: Maybe (WSCap (Either WS [WSCap FuncArg])),
  funcBlock   :: Block Stmt}
  deriving (Eq, Show, Typeable, Data)

data Interface = Interface {
  ifaceName    :: WSCap Const,
  ifaceExtends :: [WSCap Const],
  ifaceBlock   :: Block IfaceStmt}
  deriving (Eq, Show, Typeable, Data)

data IfaceStmt =
  IfaceConst [WSCap (VarEqVal Const)] |
  IfaceFunc AbstrFunc
  deriving (Eq, Show, Typeable, Data)

data Namespace = Namespace {
    nsName :: WSCap String,
    nsBlock :: WSCap BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data Label =
  Label String
  deriving (Eq, Show, Typeable, Data)

data AbstrFunc = AbstrFunc {
  abstrFuncPre  :: [(String, WS)],
  abstrFuncRef  :: Maybe WS,
  abstrFuncName :: WSCap Const,
  abstrFuncArgs :: Either WS [WSCap FuncArg],
  abstrFuncWS   :: WS,
  abstrFuncStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data Class = Class {
  classPre     :: [(String, WS)],
  className    :: WSCap String,
  classExtends :: Maybe (WSCap Const),
  classImplements :: [WSCap Const],
  classBlock   :: Block ClassStmt}
  deriving (Eq, Show, Typeable, Data)

data FuncArg = FuncArg {
  funcArgType :: Maybe (Maybe Const, WS),
  funcArgRef  :: Maybe WS,
  funcArgVar  :: VarMbVal}
  deriving (Eq, Show, Typeable, Data)

data VarMbVal = VarMbVal Var (Maybe (WS2, Expr))
  deriving (Eq, Show, Typeable, Data)

data VarEqVal a = VarEqVal a WS2 Expr
  deriving (Eq, Show, Typeable, Data)

-- Differentiate between standard control flow statements and the special
-- syntax with colons (if-endif, while-endwhile, etc.).
data StmtSyntax = StdSyntax | AltSyntax
  deriving (Eq, Show, Typeable, Data)

data ClassStmt =
  -- this list must have at least one element.. should i make a type for that?
  CStmtVar (IC.Intercal String WS) [WSCap VarMbVal] StmtEnd |
  CStmtConst [WSCap (VarEqVal Const)] |
  CStmtFuncDef [(String, WS)] Func |
  CStmtAbstrFunc AbstrFunc |
  CStmtCategory String |
  CStmtChildren String |
  CStmtAttribute String
  deriving (Eq, Show, Typeable, Data)

data DoWhile = DoWhile {
  doWhileBlock   :: BlockOrStmt,
  doWhileExpr    :: WSCap2 Expr,
  doWhileStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data Declare = Declare {
  declareHeader  :: WSCap (WSCap Const, WSCap Expr),
  declareBlock   :: WSCap BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data For = For {
  forHeader :: WSCap (ForPart, ForPart, ForPart),
  forBlock  :: BlockOrStmt,
  forSyntax :: StmtSyntax}
  deriving (Eq, Show, Typeable, Data)

data ForPart = ForPart (Either WS [WSCap Expr])
  deriving (Eq, Show, Typeable, Data)

data Foreach = Foreach {
  foreachHeader :: WSCap (WSCap Expr,
    Maybe (WSCap (Either LVal Ref)), WSCap (Either LVal Ref)),
  foreachBlock  :: BlockOrStmt,
  foreachSyntax :: StmtSyntax}
  deriving (Eq, Show, Typeable, Data)

data If = If {
  ifSyntax     :: StmtSyntax,
  ifAndIfelses :: [IfBlock],
  ifElse       :: Maybe (BlockOrStmt, WS)}
  deriving (Eq, Show, Typeable, Data)

data IfBlock = IfBlock {
  ifBlockExpr  :: WSCap (WS, Expr),
  ifBlockWS    :: WS,
  ifBlockBlock :: (BlockOrStmt, WS)}
  deriving (Eq, Show, Typeable, Data)

data Switch = Switch {
  switchSyntax  :: StmtSyntax,
  switchExpr    :: WSCap2 Expr,
  switchWS      :: WS,
  switchTL      :: Maybe (WSCap TopLevel), -- allows for '?>' before first case
  switchCases   :: [StoredPos Case]
 } deriving (Eq, Show, Typeable, Data)

data Case = Case {
  caseExpr     :: Either WS (WSCap Expr),
  caseStmtList :: StmtList}
  deriving (Eq, Show, Typeable, Data)

data Catch = Catch {
  catchHeader :: WSCap (WSCap Const, Expr),
  catchWS     :: WS,
  catchBlock  :: Block Stmt}
  deriving (Eq, Show, Typeable, Data)

data While = While {
  whileExpr   :: WSCap2 Expr,
  whileBlock  :: BlockOrStmt,
  whileSyntax :: StmtSyntax}
  deriving (Eq, Show, Typeable, Data)

data TopLevel = TopLevel String (Maybe (Either (WSCap Expr, StmtEnd) String))
  deriving (Eq, Show, Typeable, Data)

data StmtEnd = StmtEndSemi | StmtEndClose TopLevel
  deriving (Eq, Show, Typeable, Data)

type BlockOrStmt = WSCap (Either (StoredPos Stmt) (Block Stmt))

---
--- makeBinary
---

$(derive makeBinary ''AbstrFunc)
$(derive makeBinary ''Block)
$(derive makeBinary ''Case)
$(derive makeBinary ''Catch)
$(derive makeBinary ''Class)
$(derive makeBinary ''ClassStmt)
$(derive makeBinary ''Declare)
$(derive makeBinary ''DoWhile)
$(derive makeBinary ''For)
$(derive makeBinary ''ForPart)
$(derive makeBinary ''Foreach)
$(derive makeBinary ''Func)
$(derive makeBinary ''FuncArg)
$(derive makeBinary ''If)
$(derive makeBinary ''IfaceStmt)
$(derive makeBinary ''IfBlock)
$(derive makeBinary ''Interface)
$(derive makeBinary ''Label)
$(derive makeBinary ''Namespace)
$(derive makeBinary ''Stmt)
$(derive makeBinary ''StmtEnd)
$(derive makeBinary ''Switch)
$(derive makeBinary ''TopLevel)
$(derive makeBinary ''VarMbVal)
$(derive makeBinary ''VarEqVal)
$(derive makeBinary ''While)
$(derive makeBinary ''StmtSyntax)
$(derive makeBinary ''BinOp)
$(derive makeBinary ''BinOpBy)
$(derive makeBinary ''Const)
$(derive makeBinary ''DubArrowMb)
$(derive makeBinary ''DynConst)
$(derive makeBinary ''Expr)
$(derive makeBinary ''Ref)
$(derive makeBinary ''IncOrReq)
$(derive makeBinary ''LOnlyVal)
$(derive makeBinary ''LRVal)
$(derive makeBinary ''LVal)
$(derive makeBinary ''Memb)
$(derive makeBinary ''OnceOrNot)
$(derive makeBinary ''PostOp)
$(derive makeBinary ''PreOp)
$(derive makeBinary ''ROnlyVal)
$(derive makeBinary ''RVal)
$(derive makeBinary ''TernaryIf)
$(derive makeBinary ''Val)
$(derive makeBinary ''Var)
$(derive makeBinary ''StrLitExprStyle)
$(derive makeBinary ''StrLit)
$(derive makeBinary ''HereDoc)
$(derive makeBinary ''NewDoc)
