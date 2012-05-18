{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}

module Lang.Php.Ast.Parse where

import Control.Monad.Identity
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import Lang.Php.Ast.Types
import Text.ParserCombinators.Parsec.Expr
import qualified Data.Intercal as IC

--
-- STATEMENTS
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
    StmtGoto a end -> unparse a ++ unparse end
    StmtIf a -> unparse a
    StmtInterface a -> unparse a
    StmtLabel a -> unparse a
    StmtNothing end -> unparse end
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

-- FIXME: for-endfor syntax does not store all whitespace in the AST
instance Unparse For where
  unparse (For (WSCap w1 (inits, conds, incrs) w2) block StdSyntax) = concat [
    tokFor, unparse w1, tokLParen,
    intercalate tokSemi $ map unparse [inits, conds, incrs],
    tokRParen, unparse w2, unparse block]
  unparse (For (WSCap w1 (inits, conds, incrs) w2) (Right (Block stmts)) AltSyntax) =
    concat [tokFor, unparse w1, tokLParen,
        intercalate tokSemi $ map unparse [inits, conds, incrs],
        tokRParen, unparse w2, tokColon, " ", unparse stmts, tokEndfor]

instance Unparse ForPart where
  unparse (ForPart e) = either unparse (intercalate tokComma . map unparse) e

-- FIXME: foreach-endforeach syntax does not store all whitespace in the AST
instance Unparse Foreach where
  unparse (Foreach (WSCap w1 (expr, dubArrow) w2) block StdSyntax) = concat [tokForeach,
    unparse w1, tokLParen, unparse expr, tokAs, unparse dubArrow, tokRParen,
    unparse w2, unparse block]
  unparse (Foreach (WSCap w1 (expr, dubArrow) w2) (Right (Block stmts)) AltSyntax) =
    concat [tokForeach, unparse w1, tokLParen, unparse expr, tokAs, unparse dubArrow,
            tokRParen, unparse w2, tokColon, " ", unparse stmts, tokEndforeach]

instance Unparse Func where
  unparse (Func w1 ref name (WSCap w2 args w3) block) = concat [tokFunction,
    unparse w1, maybe [] ((tokAmp ++) . unparse) ref, show name, unparse w2,
    tokLParen, argsUnparser args, tokRParen, unparse w3, unparse block]

argsUnparser :: (Unparse t, Unparse s) => Either t [s] -> String
argsUnparser = either unparse (intercalate tokComma . map unparse)

instance Unparse If where
  unparse _ = undefined -- FIXME: not implemented

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
  -- FIXME: while-endwhile syntax does not store all whitespace in the AST
  unparse (While (WSCap w1 expr w2) (Right (Block stmts)) AltSyntax) =
    concat [tokWhile, unparse w1, tokLParen, unparse expr, tokRParen,
            unparse w2, tokColon, unparse stmts, " ", tokEndwhile, " "]


stmtListParser :: Parser StmtList
stmtListParser = liftM2 IC.unbreakStart parse parse

instance Parse (Stmt, WS) where
  parse =
    first StmtFor <$> parse <|>
    first StmtForeach <$> parse <|>
    first StmtIf <$> parse <|>
    first StmtWhile <$> parse <|>
    tryParser <|>
    liftM2 (,) simpleStmtParser parse

simpleStmtParser :: Parser Stmt
simpleStmtParser =
  try (StmtLabel <$> parse <* tokColonP) <|>
  StmtBlock <$> parse <|>
  breaklikeParser StmtBreak tokBreakP <|>
  StmtClass <$> parse <|>
  breaklikeParser StmtContinue tokContinueP <|>
  StmtDeclare <$> parse <|>
  StmtDoWhile <$> parse <|>
  liftM2 StmtEcho (tokEchoP >> sepBy1 parse tokCommaP) parse <|>
  try (StmtFuncDef <$> parse) <|>
  liftM2 (uncurry StmtExpr) parse parse <|>
  liftM2 StmtGlobal (tokGlobalP >> sepBy1 parse tokCommaP) parse <|>
  StmtInterface <$> parse <|>
  StmtNothing <$> parse <|>
  liftM3 StmtReturn (tokReturnP >> parse) (optionMaybe parse) parse <|>
  liftM2 StmtStatic (tokStaticP >> sepBy1 parse tokCommaP) parse <|>
  liftM2 StmtGoto (tokGotoP >> parse) parse <|>
  StmtNamespace <$> parse <|>
  StmtSwitch <$> parse <|>
  liftM2 StmtThrow (tokThrowP >> parse) parse <|>
  liftM2 StmtUnset
    (tokUnsetP >> liftM3 WSCap parse (issetListParser parse) parse)
    parse 

instance Parse (If, WS) where
  parse = do
    tokIfP >> discardWS
    e <- parenExprP
    syn <- option StdSyntax ((lookAhead tokColonP) >> return AltSyntax)
    block <- blockP syn
    elseifs <- many (elseifP syn)
    elseBlock <- optionMaybe (tokElseP >> discardWS >> blockP syn)
    if syn == AltSyntax then tokEndifP >> return () else return ()
    ws <- parse
    return (If syn ((IfBlock e block):elseifs) elseBlock, ws)
   where
    elseifP :: StmtSyntax -> Parser IfBlock
    elseifP syn = do
        tokElseifP >> discardWS
        e <- parenExprP
        block <- blockP syn
        return (IfBlock e block)

    parenExprP :: Parser Expr
    parenExprP = do
        tokLParenP >> discardWS
        (e, _) <- parse :: Parser (Expr, WS)
        tokRParenP >> discardWS
        return e

    blockP :: StmtSyntax -> Parser BlockOrStmt
    blockP StdSyntax = do
        (block, _) <- parse :: Parser (BlockOrStmt, WS)
        return block

    blockP AltSyntax = do
        tokColonP >> discardWS
        stmts <- stmtListParser
        lookAhead (tokEndifP <|> tokElseP <|> tokElseifP)
        return (Right $ Block stmts)

tryParser :: Parser (Stmt, WS)
tryParser = tokTryP >> do
  block <- parse
  first (StmtTry block) <$> intercalParserW parse

intercalParserW :: Parser a -> Parser (IC.Intercal a WS, WS)
intercalParserW a =
  (\ (aInit, (aLast, w)) -> (IC.unbreakEnd aInit aLast, w)) . unsnoc <$>
    many (liftM2 (,) a parse)

instance Parse Catch where
  parse = tokCatchP >> liftM3 Catch
    (liftM2 capify parse
      (tokLParenP >> liftM2 (curry rePairLeft) parse parse <* tokRParenP))
    parse
    parse

breaklikeParser :: (Maybe (WS, Expr) -> WS -> StmtEnd -> t) -> Parser b ->
  Parser t
breaklikeParser constr p = p >> do
  w1 <- parse
  eMb <- optionMaybe parse
  let
    (eMb', w) = case eMb of
      Just (e, w2) -> (Just (w1, e), w2)
      _ -> (Nothing, w1)
  constr eMb' w <$> parse

instance Parse Label where
  parse = Label <$> identifierParser

instance Parse Class where
  parse = liftM5 Class
    (many (liftM2 (,) (tokAbstractP <|> tokFinalP) parse))
    (tokClassP >> liftM3 WSCap parse identifierParser parse)
    (optionMaybe $ tokExtendsP >> parse)
    ((tokImplementsP >> sepBy1 parse tokCommaP) <|> return [])
    parse

instance Parse ClassStmt where
  parse = classConstParser CStmtConst <|>
    xhpClassAttrParser <|>
    (tokChildrenP >> CStmtChildren <$> thruSemiParser) <|>
    (tokCategoryP >> CStmtCategory <$> thruSemiParser) <|>
    do
      r <- funcOrVarTypeToksP
      case r of
        (True, pre) -> classAbstrFuncParser CStmtAbstrFunc pre
        (False, []) -> classFuncParser []
        (False, pre) -> classFuncParser pre <|> classVarsParser pre

thruSemiParser :: Parser String
thruSemiParser = many (satisfy (/= ';')) <* tokSemiP

xhpClassAttrParser :: Parser ClassStmt
xhpClassAttrParser = tokAttributeP >> CStmtAttribute . concat <$> many (
  unparse <$> (parse :: Parser StrLit) <|>
  unparse <$> (parse :: Parser NumLit) <|>
  unparse <$> (parse :: Parser WSElem) <|>
  genIdentifierParser <|> tokAtP <|> tokMinusP <|>
  tokEqualsP <|> tokCommaP <|> tokLBraceP <|> tokRBraceP) <* tokSemiP

funcOrVarTypeToksP :: Parser (Bool, [(String, WS)])
funcOrVarTypeToksP = first or . unzip . map rePairRight <$> many (liftM2 (,) (
  (,) False <$> (tokProtectedP <|> tokPrivateP <|> tokPublicP <|>
    tokStaticP <|> tokVarP <|> tokFinalP) <|>
  (,) True <$> tokAbstractP) parse)

classConstParser :: ([WSCap (VarEqVal Const)] -> c) -> Parser c
classConstParser constr = tokConstP >>
  constr <$> sepBy1 parse tokCommaP <* tokSemiP

instance (Parse (a, WS)) => Parse (VarEqVal a, WS) where
  parse = do
    (var, w1) <- parse
    w2 <- tokEqualsP >> parse
    (val, w3) <- parse
    return (VarEqVal var (w1, w2) val, w3)

instance Parse (VarMbVal, WS) where
  parse = do
    (var, w1) <- parse
    liftM2 (\ w2 (val, w3) -> (VarMbVal var $ Just ((w1, w2), val), w3))
      (tokEqualsP >> parse) parse <|>
      return (VarMbVal var Nothing, w1)

classFuncParser :: [(String, WS)] -> Parser ClassStmt
classFuncParser pre = CStmtFuncDef pre <$> parse

classAbstrFuncParser :: (AbstrFunc -> c) -> [(String, WS)] -> Parser c
classAbstrFuncParser constr pre = constr <$> liftM5 (AbstrFunc pre)
  (tokFunctionP >> optionMaybe (try $ parse <* tokAmpP)) parse
  (argListParser parse) parse parse

instance Parse (FuncArg, WS) where
  parse = do
    t <- optionMaybe
      (first Just <$> parse <|> (tokArrayP >> (,) Nothing <$> parse))
    ref <- optionMaybe (tokAmpP >> parse)
    first (FuncArg t ref) <$> parse

unsnoc :: [a] -> ([a], a)
unsnoc = first reverse . swap . uncons . reverse

classVarsParser :: [(String, WS)] -> Parser ClassStmt
classVarsParser pre = let (preInit, (s, w)) = unsnoc pre in
  liftM2 (CStmtVar (IC.unbreakEnd preInit s))
    (liftM2 (:) (capify w <$> parse)
      (many (tokCommaP >> parse)))
    parse

instance Parse Declare where
  parse = tokDeclareP >> liftM2 Declare
    (liftM3 WSCap parse (tokLParenP >>
      liftM2 (,) parse (tokEqualsP >> parse)) (tokRParenP >> parse))
    parse

instance Parse DoWhile where
  parse = liftM3 DoWhile (tokDoP >> parse) (tokWhileP >>
      liftM3 WSCap parse (tokLParenP >> parse <* tokRParenP) parse)
    parse

instance (Parse (a, WS), Parse (b, WS)) => Parse (Either a b, WS) where
  parse = first Right <$> parse <|> first Left <$> parse

-- FIXME: for-endfor syntax does not store all whitespace in the AST
instance Parse (For, WS) where
  parse = tokForP >> do
    h <- liftM3 WSCap parse (tokLParenP >> liftM3 (,,) parse
      (tokSemiP >> parse <* tokSemiP) parse <* tokRParenP) parse
    (do -- try parsing for-endfor combination
        tokColonP >> discardWS
        stmts <- stmtListParser
        tokEndforP
        ws <- parse
        return (For h (Right $ Block stmts) AltSyntax, ws)
     ) <|> (do
        -- standard for(...) {...}
        (block, ws) <- parse
        return (For h block StdSyntax, ws)
     )

instance Parse ForPart where
  parse = do
    w1 <- parse
    forPartExpry w1 <|> return (ForPart $ Left w1)

forPartExpry :: WS -> Parser ForPart
forPartExpry w1 = ForPart . Right <$>
  liftM2 (:) (capify w1 <$> parse) (many $ tokCommaP >> parse)

-- FIXME: foreach-endforeach syntax does not store all whitespace in the AST
instance Parse (Foreach, WS) where
  parse = tokForeachP >> do
    h <- liftM3 WSCap parse
      (tokLParenP >> liftM2 (,) parse (tokAsP >> parse) <* tokRParenP)
      parse
    parseAltSyntax h <|> parseStdSyntax h
   where
    parseAltSyntax h = do
        tokColonP >> discardWS
        stmts <- stmtListParser
        tokEndforeachP
        ws <- parse
        return (Foreach h (Right $ Block stmts) AltSyntax, ws)
    parseStdSyntax h = do
        (block, ws) <- parse
        return (Foreach h block StdSyntax, ws)

instance Parse Func where
  parse = tokFunctionP >> liftM5 Func parse
    ((tokAmpP >> Just <$> parse) <|> return Nothing) (Just <$> identifierParser)
    (liftM3 WSCap parse (argListParser parse) parse) parse

instance Parse Interface where
  parse = tokInterfaceP >> liftM3 Interface
    parse
    ((tokExtendsP >> sepBy1 parse tokCommaP) <|> return [])
    parse

instance Parse Namespace where
  parse = tokNamespaceP >> (liftM2 Namespace (liftM3 WSCap parse namespaceParser parse) parse)

instance Parse IfaceStmt where
  parse =
    classConstParser IfaceConst <|>
    classAbstrFuncParser IfaceFunc =<< snd <$> funcOrVarTypeToksP

-- FIXME: This is (and was) buggy. See parser_bugs/switch.php.
instance Parse Switch where
  parse = try (parseGen (Switch StdSyntax) tokLBraceP tokRBraceP)
           <|> parseGen (Switch AltSyntax) tokColonP  tokEndswitchP
   where
      parseGen f left right = tokSwitchP >> liftM4 f
        (liftM3 WSCap parse (tokLParenP >> parse <* tokRParenP) parse)
        (left >> parse)
        -- (return Nothing) -- (optionMaybe $ try $ tokClosePhpP >> parse)
        (optionMaybe $ try $ tokClosePhpP >> parse <* discardWS)
        (parse <* right)

instance Parse Case where
  parse = liftM2 Case
    ((tokDefaultP >> Left <$> parse) <|> (tokCaseP >> Right <$> parse))
    ((tokColonP <|> tokSemiP) >> stmtListParser)

-- FIXME: while-endwhile syntax does not store all whitespace in the AST
instance Parse (While, WS) where
  parse = tokWhileP >> do
    e <- liftM3 WSCap parse (tokLParenP >> parse <* tokRParenP) parse
    (do -- try parse while-endwhile combination
        tokColonP >> discardWS
        stmts <- stmtListParser
        tokEndwhileP
        ws <- parse
        return (While e (Right $ Block stmts) AltSyntax, ws)
     ) <|> (do
        -- otherwise revert to standard while(...) {...}
        (block, ws) <- parse
        return (While e block StdSyntax, ws)
     )

instance Parse (a, WS) => Parse (Block a) where
  parse = tokLBraceP >> Block <$> liftM2 IC.unbreakStart parse parse <*
    tokRBraceP

instance Parse TopLevel where
  parse = do
    (gotChars, text) <- upToCharsOrEndParser (const True) '<' '?'
    echoOrTok <- if gotChars
      then fmap Just $
        (char '=' >> Left <$> liftM2 (,) parse parse) <|>
        Right <$> (identCI "php" <|> return "")
      else return Nothing
    return $ TopLevel text echoOrTok

instance Parse StmtEnd where
  parse = (tokSemiP >> return StmtEndSemi) <|>
    (tokClosePhpP >> StmtEndClose <$> parse)

---
--- EXPRESSIONS
---

-- Val

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

instance Unparse Const where
  unparse (Const statics s) = concatMap (\ (s, (ws1, ws2)) -> s ++
    unparse ws1 ++ tokDubColon ++ unparse ws2) statics ++ s

instance Unparse DynConst where
  unparse (DynConst statics var) = concatMap (\ (s, (ws1, ws2)) -> s ++
    unparse ws1 ++ tokDubColon ++ unparse ws2) statics ++ unparse var

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

instance Parse (Var, WS) where
  parse = tokDollarP >> (undyn <|> dyn) where
    undyn = do
      i <- genIdentifierParser
      -- try is here unless we combine processing for [expr] vs []
      (inds, ws) <- IC.breakEnd <$> IC.intercalParser parse (try $
        (tokLBracketP >> (,) True <$> parse <* tokRBracketP) <|>
        (tokLBraceP >> (,) False <$> parse <* tokRBraceP))
      return (Var i inds, ws)
    dyn = do
      ws <- parse
      first (VarDyn ws) <$> parse <|> first (VarDynExpr ws) <$> liftM2 (,)
        (tokLBraceP >> parse <* tokRBraceP) parse

parseABPairsUntilAOrC :: Parser a -> Parser b -> Parser c ->
  Parser ([(a, b)], Either a c)
parseABPairsUntilAOrC a b c = (,) [] . Right <$> c <|> do
  aR <- a
  (b >>= \ bR -> first ((aR, bR):) <$> parseABPairsUntilAOrC a b c) <|>
    return ([], Left aR)

dynConstOrConstParser :: Parser (Either DynConst Const, WS)
dynConstOrConstParser = do
  (statics, cOrD) <-
    first (map (\ ((a, b), c) -> (a, (b, c)))) <$>
    parseABPairsUntilAOrC (liftM2 (,) identifierParser parse)
    (tokDubColonP >> parse) parse
  return $ case cOrD of
    Left c -> first (Right . Const statics) c
    Right d -> first (Left . DynConst statics) d

exprOrLValParser :: Parser (Either Expr LVal, WS)
exprOrLValParser = try (first Left <$> parse) <|> first Right <$> parse

instance Parse (Val, WS) where
  parse = listVal <|> otherVal where
    listVal = tokListP >> liftM2 (,)
      (ValLOnlyVal <$> liftM2 LOnlyValList parse (mbArgListParser parse))
      parse
    otherVal = do
      (dOrC, ws) <- dynConstOrConstParser
      valExtend =<< case dOrC of
        Left d -> return (ValLRVal $ LRValVar d, ws)
        Right c -> (first ValROnlyVal <$>) $
          liftM2 (,) (ROnlyValFunc (Right c) ws <$> argListParser exprOrLValParser) parse
          <|> return (ROnlyValConst c, ws)

firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM = runKleisli . first . Kleisli

instance Parse (LVal, WS) where
  parse = firstM f =<< parse where
    f r = case r of
      ValLOnlyVal v -> return $ LValLOnlyVal v
      ValROnlyVal _ -> fail "Expecting an LVal but found an ROnlyVal."
      ValLRVal v -> return $ LValLRVal v

instance Parse (RVal, WS) where
  parse = firstM f =<< parse where
    f r = case r of
      ValLOnlyVal _ -> fail "Expecting an RVal but found an LOnlyVal."
      ValROnlyVal v -> return $ RValROnlyVal v
      ValLRVal v -> return $ RValLRVal v

instance Parse (LRVal, WS) where
  parse = firstM f =<< parse where
    f r = case r of
      ValLOnlyVal _ -> fail "Expecting an LRVal but found an LOnlyVal."
      ValROnlyVal _ -> fail "Expecting an LRVal but found an ROnlyVal."
      ValLRVal v -> return v

-- val extending is like this:
-- L --member,index,append--> L
-- R --member--> LR
-- LR --member,index--> LR
-- LR --func--> R
-- LR --append--> L
valExtend :: (Val, WS) -> Parser (Val, WS)
valExtend v@(state, ws) = case state of
  ValLOnlyVal a ->
    do
      ws2 <- tokArrowP >> parse
      (memb, wsEnd) <- parse
      valExtend (ValLOnlyVal $ LOnlyValMemb a (ws, ws2) memb, wsEnd)
    <|> valExtendIndApp (LValLOnlyVal a) (ValLOnlyVal . LOnlyValInd a ws) ws
    <|> return v
  ValROnlyVal a -> valExtendMemb (RValROnlyVal a) ws
    <|> do
      ws2 <- tokLBracketP >> parse
      st <- ValLRVal . LRValInd (RValROnlyVal a) ws . capify ws2 <$>
        parse <* tokRBracketP
      valExtend =<< (,) st <$> parse
    <|> return v
  ValLRVal a ->
    do
      r <- liftM2 (,) (ValROnlyVal . ROnlyValFunc (Left a) ws <$>
        argListParser exprOrLValParser) parse
      valExtend r
    <|> valExtendIndApp (LValLRVal a) (ValLRVal . LRValInd (RValLRVal a) ws) ws
    <|> valExtendMemb (RValLRVal a) ws
    <|> return v

valExtendMemb :: RVal -> WS -> Parser (Val, WS)
valExtendMemb a ws = tokArrowP >> do
  ws2 <- parse
  (memb, wsEnd) <- parse
  valExtend (ValLRVal $ LRValMemb a (ws, ws2) memb, wsEnd)

instance Parse (Memb, WS) where
  parse =
    liftM2 (,) (
      (tokLBraceP >> MembExpr <$> parse <* tokRBraceP) <|>
      MembStr <$> genIdentifierParser) parse <|>
    first MembVar <$> parse

valExtendIndApp :: LVal -> (WSCap Expr -> Val) -> WS -> Parser (Val, WS)
valExtendIndApp lVal mkVal ws = tokLBracketP >> do
  ws2 <- parse
  st <-
    (tokRBracketP >>
      return (ValLOnlyVal $ LOnlyValAppend lVal (ws, ws2))) <|>
    mkVal . capify ws2 <$> (parse <* tokRBracketP)
  valExtend =<< (,) st <$> parse

varOrStringParser :: Parser (Either Var String, WS)
varOrStringParser = first Left <$> parse <|>
  liftM2 (,) (Right <$> identifierParser) parse

instance Parse (DynConst, WS) where
  parse = do
    statics <- many . liftM2 (,) identifierParser . liftM2 (,) parse $
      tokDubColonP >> parse
    first (DynConst statics) <$> parse

instance Parse (Const, WS) where
  parse = first (uncurry Const) . rePairLeft . first (map rePairRight) .
    IC.breakEnd <$> IC.intercalParser (liftM2 (,) identifierParser parse)
    (tokDubColonP >> parse)

lRValOrConstParser :: Parser (Either LRVal Const, WS)
lRValOrConstParser = do
  (v, w) <- parse
  case v of
    ValLRVal a -> return (Left a, w)
    ValROnlyVal (ROnlyValConst a) -> return (Right a, w)
    _ -> fail "Expected LRVal or Const but fould a different Val type."

-- Expr

instance Unparse Expr where
  unparse expr = case expr of
    ExprArray w elemsOrW -> tokArray ++ unparse w ++ tokLParen ++
      either unparse f elemsOrW ++ tokRParen where
      f (elems, wEnd) = intercalate tokComma .
        maybe id (flip (++) . (:[]) . unparse) wEnd $ map unparse elems
    ExprAssign o v w e -> unparse v ++ w2With (unparse o ++ tokEquals) w ++
      unparse e
    ExprBackticks a -> a
    ExprBinOp o e1 (w1, w2) e2 -> unparse e1 ++ unparse w1 ++ unparse o ++
      unparse w2 ++ unparse e2
    ExprCast (WSCap w1 t w2) w e -> tokLParen ++ unparse w1 ++ t ++
      unparse w2 ++ tokRParen ++ unparse w ++ unparse e
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
    ExprRef w v -> tokAmp ++ unparse w ++ unparse v
    ExprRVal a -> unparse a
    ExprStrLit a -> unparse a
    ExprTernaryIf a -> unparse a

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

instance Unparse TernaryIf where
  unparse (TernaryIf e1 (w1, w2) e2 (w3, w4) e3) = unparse e1 ++ unparse w1 ++
    tokQMark ++ unparse w2 ++ unparse e2 ++ unparse w3 ++ tokColon ++
    unparse w4 ++ unparse e3

instance Parse (Expr, WS) where
  parse = buildExpressionParser exprParserTable simpleExprParser

simpleExprParser :: Parser (Expr, WS)
simpleExprParser = 
    try (liftM2 (,) (ExprStrLit <$> parse) parse)
  <|>
    try (liftM2 (,) (ExprHereDoc <$> parse) parse)
  <|>
    assignOrRValParser
  <|> do
    ws1 <- tokLParenP >> parse
    ambigCastParser ws1 <|> castOrParenParser ws1
  <|> do
    ws1 <- tokNewP >> parse
    (v, ws2) <- parse
    argsWSMb <- optionMaybe $ argListParser parse
    case argsWSMb of
      Just args -> (,) (ExprNew ws1 v $ Just (ws2, args)) <$> parse
      _ -> return (ExprNew ws1 v Nothing, ws2)
  <|> includeParser
  <|> do
    isExit <- return True <$> tokExitP <|> return False <$> tokDieP
    ws1 <- parse
    argMb <- optionMaybe $ exitListParser parse
    case argMb of
      Just arg -> (,) (ExprExit isExit $ Just (ws1, arg)) <$> parse
      _ -> return (ExprExit isExit Nothing, ws1)
  <|> do
    w <- tokAmpP >> parse
    first (ExprRef w . Right) <$> parse <|> do
      (e, wEnd) <- parse
      case e of
        ExprNew _ _ _ -> return (ExprRef w (Left e), wEnd)
        _ -> fail "Expecting a Val or ExprNew."
  <|> liftM2 (,) (
    ExprNumLit <$> parse <|>
    (tokArrayP >> liftM2 ExprArray parse (arrListParser parse)) <|>
    funclike1Parser ExprEmpty tokEmptyP <|>
    funclike1Parser ExprEval tokEvalP <|>
    (tokIssetP >> liftM2 ExprIsset parse (issetListParser parse)) <|>
    ExprBackticks <$> backticksParser
    ) parse
  <|>
    (tokFunctionP >> 
    (liftM2 (,) (ExprClosure <$> anonFuncParser) parse))


anonFuncParser = liftM5 Func parse
  ((tokAmpP >> Just <$> parse) <|> return Nothing) (return Nothing)
  (liftM3 WSCap parse (argListParser parse) parse) parse 

ambigCastParser :: WS -> Parser (Expr, WS)
ambigCastParser ws1 = try $ do
  i <- identsCI ["array", "unset"]
  ws2 <- parse
  ws3 <- tokRParenP >> parse
  first (ExprCast (WSCap ws1 i ws2) ws3) <$> parse

castOrParenParser :: WS -> Parser (Expr, WS)
castOrParenParser ws1 = do
  iMb <- optionMaybe $ identsCI ["int", "integer", "bool", "boolean",
    "float", "double", "real", "string", "binary", "object"]
  case iMb of
    Just i -> do
      ws2 <- parse
      ws3 <- tokRParenP >> parse
      first (ExprCast (WSCap ws1 i ws2) ws3) <$> parse
    _ -> liftM2 (,) (ExprParen . capify ws1 <$> parse <* tokRParenP) parse

assignOrRValParser :: Parser (Expr, WS)
assignOrRValParser = do
  (val, w) <- parse
  case val of
    ValLOnlyVal v -> assignCont (LValLOnlyVal v) w
    ValLRVal v -> assignCont (LValLRVal v) w <|>
      return (ExprRVal $ RValLRVal v, w)
    ValROnlyVal v -> return (ExprRVal $ RValROnlyVal v, w)

assignCont :: LVal -> WS -> Parser (Expr, WS)
assignCont l w1 = do
  o <- (tokEqualsP >> return Nothing) <|> Just <$> (
    (tokPlusByP   >> return BPlus) <|>
    (tokMinusByP  >> return BMinus) <|>
    (tokMulByP    >> return BMul) <|>
    (tokDivByP    >> return BDiv) <|>
    (tokConcatByP >> return BConcat) <|>
    (tokModByP    >> return BMod) <|>
    (tokBitAndByP >> return BBitAnd) <|>
    (tokBitOrByP  >> return BBitOr) <|>
    (tokXorByP    >> return BXor) <|>
    (tokShiftLByP >> return BShiftL) <|>
    (tokShiftRByP >> return BShiftR))
  w2 <- parse
  first (ExprAssign o l (w1, w2)) <$> parse

includeParser :: Parser (Expr, WS)
includeParser = try $ do
  i <- map toLower <$> genIdentifierParser
  f <- if i == tokRequireOnce then return $ ExprInclude Req Once else
    if i == tokIncludeOnce then return $ ExprInclude Inc Once else
    if i == tokRequire then return $ ExprInclude Req NotOnce else
    if i == tokInclude then return $ ExprInclude Inc NotOnce else
    fail "Expecting an include/require expression."
  ws <- parse
  first (f ws) <$> parse

instance Parse (DubArrowMb, WS) where
  parse = do
    (k, ws) <- parse
    vMb <- optionMaybe (tokDubArrowP >> liftM2 (,) parse parse)
    return $ case vMb of
      Just (ws2, (v, ws3)) -> (DubArrowMb (Just (k, (ws, ws2))) v, ws3)
      _ -> (DubArrowMb Nothing k, ws)

funclike1Parser :: (Parse (a, WS)) => (WS -> WSCap a -> b) -> Parser c ->
  Parser b
funclike1Parser constr tokP = liftM2 constr (tokP >> parse)
  (tokLParenP >> parse <* tokRParenP)

exprParserTable :: [[Oper (Expr, WS)]]
exprParserTable = [
  [Postfix eptIndex],
  [Prefix eptClone],
  [Prefix eptPreIncr, Prefix eptPreDecr,
   Postfix eptPostIncr, Postfix eptPostDecr],
  [Postfix eptInstOf],
  [Prefix . preRep $ eptNot <|> eptBitNot <|> eptNegate <|> eptPos <|>
    eptSuppress],
  ial [eptMul, eptDiv, eptMod],
  ial [eptPlus, eptMinus, eptConcat],
  ial [eptShiftL, eptShiftR],
  ian [eptLT, eptLE, eptGT, eptGE, eptNEOld],
  ian [eptEQ, eptNE, eptID, eptNI],
  ial [eptBitAnd],
  ial [eptXor],
  ial [eptBitOr],
  [Prefix eptPrint],
  ial [eptAnd],
  ial [eptOr],
  [Postfix eptTernaryIf],
  ial [eptAndWd],
  ial [eptXorWd],
  ial [eptOrWd]]

preRep, postRep :: Parser (a -> a) -> Parser (a -> a)
preRep p = (p >>= \ f -> (f .) <$> preRep p) <|> return id
postRep p = (p >>= \ f -> (. f) <$> postRep p) <|> return id

ial :: [Parser (a -> a -> a)] -> [Oper a]
ial = map $ flip Infix AssocLeft
ian = map $ flip Infix AssocNone

eptClone = preOp PrClone tokCloneP
eptPreIncr = preOp PrIncr tokIncrP
eptPreDecr = preOp PrDecr tokDecrP
eptPostIncr = postOp PoIncr tokIncrP
eptPostDecr = postOp PoDecr tokDecrP

preOp :: PreOp -> Parser a -> Parser ((Expr, WS) -> (Expr, WS))
preOp o p = do
  ws1 <- p >> parse
  return . first $ ExprPreOp o ws1

postOp :: PostOp -> Parser a -> Parser ((Expr, WS) -> (Expr, WS))
postOp o p = do
  ws2 <- p >> parse
  return $ \ (e, ws1) -> (ExprPostOp o e ws1, ws2)

binOp :: BinOp -> Parser a -> Parser ((Expr, WS) -> (Expr, WS) -> (Expr, WS))
binOp o p = do
  ws2 <- p >> parse
  return $ \ (e1, ws1) (e2, ws3) -> (ExprBinOp o e1 (ws1, ws2) e2, ws3)

eptBitNot = preOp PrBitNot tokBitNotP
eptNegate = preOp PrNegate tokMinusP
eptPos    = preOp PrPos tokPlusP
eptSuppress = preOp PrSuppress tokAtP

eptInstOf = do
  tokInstanceofP
  ws2 <- parse
  (t, ws3) <- lRValOrConstParser
  return $ \ (e, ws1) -> (ExprInstOf e (ws1, ws2) t, ws3)

eptNot = preOp PrNot tokNotP

eptMul = binOp (BByable BMul) tokMulP
eptDiv = binOp (BByable BDiv) tokDivP
eptMod = binOp (BByable BMod) tokModP
eptPlus   = binOp (BByable BPlus) tokPlusP
eptMinus  = binOp (BByable BMinus) tokMinusP
eptConcat = binOp (BByable BConcat) tokConcatP
eptShiftL = binOp (BByable BShiftL) tokShiftLP
eptShiftR = binOp (BByable BShiftR) tokShiftRP
eptLT     = binOp BLT     tokLTP
eptLE     = binOp BLE     tokLEP
eptGT     = binOp BGT     tokGTP
eptGE     = binOp BGE     tokGEP
eptNEOld  = binOp BNEOld  tokNEOldP
eptEQ     = binOp BEQ     tokEQP
eptNE     = binOp BNE     tokNEP
eptID     = binOp BID     tokIDP
eptNI     = binOp BNI     tokNIP

eptBitAnd = binOp (BByable BBitAnd) tokAmpP
eptXor    = binOp (BByable BXor) tokXorP
eptBitOr  = binOp (BByable BBitOr) tokBitOrP

eptPrint  = preOp PrPrint tokPrintP

eptAnd    = binOp BAnd    tokAndP
eptOr     = binOp BOr     tokOrP

eptTernaryIf :: Parser ((Expr, WS) -> (Expr, WS))
eptTernaryIf = do
  w2 <- tokQMarkP >> parse
  -- then-expression is optional; when it's missing m_e2 is Nothing
  (m_e2, w3) <- try (parse >>= \(e2, w3) -> return (Just e2, w3))
                <|> (parse >>= \w3 -> return (Nothing, w3))
  w4 <- tokColonP >> parse
  (e3, w5) <- parse
  return $ \ (e1, w1) ->
    (ExprTernaryIf $ TernaryIf e1 (w1, w2) m_e2 (w3, w4) e3, w5)

eptAndWd = binOp BAndWd tokAndWdP
eptXorWd = binOp BXorWd tokXorWdP
eptOrWd  = binOp BOrWd  tokOrWdP

eptIndex :: Parser ((Expr, WS) -> (Expr, WS))
eptIndex = do
  e2 <- tokLBracketP >> parse
  w2 <- tokRBracketP >> parse
  return $ \ (e1, w1) -> (ExprIndex e1 w1 e2, w2)
