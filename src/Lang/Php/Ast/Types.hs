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

data DynConst = DynConst [(String, WS2)] Var -- "a::$a"
  deriving (Eq, Show, Typeable, Data)

data LRVal =
  LRValVar  DynConst |
  LRValInd  RVal WS (WSCap Expr) | -- "$a->a[0]"
  LRValMemb RVal WS2 Memb -- $a->a
  deriving (Eq, Show, Typeable, Data)

data LOnlyVal =
  LOnlyValList   WS (Either WS [Either WS (WSCap LVal)]) |
  LOnlyValAppend LVal WS2                 | -- "$a[]"
  LOnlyValInd    LOnlyVal WS (WSCap Expr) | -- "$a[][0]"
  LOnlyValMemb   LOnlyVal WS2 Memb          -- "$a[]->a"
  deriving (Eq, Show, Typeable, Data)

data Const = Const [(String, WS2)] String -- "a::a"
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
  ExprBackticks String |
  ExprBinOp     BinOp Expr WS2 Expr |
  -- we're lazy so just String here instead of like PhpType
  ExprCast      (WSCap String) WS Expr |
  ExprClosure   Func |
  ExprEmpty     WS (WSCap LRVal) |
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
  ExprNumLit    NumLit |
  ExprParen     (WSCap Expr) |
  ExprPostOp    PostOp Expr WS |
  ExprPreOp     PreOp WS Expr |
  -- note: "list"/"&" is actually more limited
  -- ("list() = &$a;" is nonsyntactic)
  ExprRef       WS (Either Expr Val) |
  ExprRVal      RVal |
  ExprStrLit    StrLit |
  ExprTernaryIf TernaryIf
  deriving (Eq, Show, Typeable, Data)

data BinOp = BAnd | BAndWd | BEQ | BGE | BGT | BID | BLE | BLT | BNE |
  -- <> has different precedence than !=
  BNEOld | BNI | BOr | BOrWd | BXorWd | BByable BinOpBy
  deriving (Eq, Show, Typeable, Data)

data BinOpBy = BBitAnd | BBitOr | BConcat | BDiv | BMinus | BMod | BMul |
  BPlus | BShiftL | BShiftR | BXor
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
  StmtGlobal    [WSCap Var] StmtEnd           |
  StmtGoto      (WSCap Label)                 |
  StmtIf        If                            |
  StmtInterface Interface                     |
  StmtLabel     (WSCap Label)                 |
  StmtNothing   StmtEnd                       |
  StmtReturn    WS (Maybe (Expr, WS)) StmtEnd |
  -- this list must have at least one element.. should i make a type for that?
  StmtStatic    [WSCap VarMbVal] StmtEnd      |
  StmtSwitch    Switch                        |
  StmtThrow     (WSCap Expr) StmtEnd          |
  StmtTry       (WSCap (Block Stmt)) (IC.Intercal Catch WS) |
  StmtUnset     (WSCap [WSCap LRVal]) StmtEnd   |
  StmtWhile     While
  deriving (Eq, Show, Typeable, Data)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS (StoredPos a))
  deriving (Eq, Show, Typeable, Data)

data Func = Func {
  funcWS    :: WS,
  funcRef   :: Maybe WS,
  funcName  :: Maybe String,
  funcArgs  :: WSCap (Either WS [WSCap FuncArg]),
  funcBlock :: Block Stmt}
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
  doWhileBlock   :: WSCap BlockOrStmt,
  doWhileExpr    :: WSCap2 Expr,
  doWhileStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data Declare = Declare {
  declareHeader  :: WSCap (WSCap Const, WSCap Expr),
  declareStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data For = For {
  forHeader :: WSCap (ForPart, ForPart, ForPart),
  forBlock  :: BlockOrStmt,
  forSyntax :: StmtSyntax}
  deriving (Eq, Show, Typeable, Data)

data ForPart = ForPart (Either WS [WSCap Expr])
  deriving (Eq, Show, Typeable, Data)

data Foreach = Foreach {
  foreachHeader :: WSCap (WSCap Expr, WSCap DubArrowMb),
  foreachBlock  :: BlockOrStmt,
  foreachSyntax :: StmtSyntax}
  deriving (Eq, Show, Typeable, Data)

data If = If {
  ifSyntax     :: StmtSyntax,
  ifAndIfelses :: [IfBlock],
  ifElse       :: Maybe BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data IfBlock = IfBlock {
  ifBlockExpr  :: Expr,
  ifBlockBlock :: BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data Switch = Switch {
  switchSyntax  :: StmtSyntax,
  switchExpr    :: WSCap2 Expr,
  switchWS      :: WS,
  switchTL      :: Maybe TopLevel, -- allows for '?>' before first case
  switchCases   :: [Case]
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

type BlockOrStmt = Either (StoredPos Stmt) (Block Stmt)

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
