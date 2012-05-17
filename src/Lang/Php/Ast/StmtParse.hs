{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}

module Lang.Php.Ast.StmtParse where

import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.Expr
import Lang.Php.Ast.ExprTypes
import Lang.Php.Ast.Lex
import Lang.Php.Ast.StmtTypes
import qualified Data.Intercal as IC

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
    StmtGoto a -> unparse a
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
    unparse w1, maybe [] ((tokAmp ++) . unparse) ref, name, unparse w2,
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
  liftM2 (uncurry StmtExpr) parse parse <|>
  StmtFuncDef <$> parse <|>
  liftM2 StmtGlobal (tokGlobalP >> sepBy1 parse tokCommaP) parse <|>
  StmtInterface <$> parse <|>
  StmtNothing <$> parse <|>
  liftM3 StmtReturn (tokReturnP >> parse) (optionMaybe parse) parse <|>
  liftM2 StmtStatic (tokStaticP >> sepBy1 parse tokCommaP) parse <|>
  liftM StmtGoto (tokGotoP >> parse) <|>
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
    ((tokAmpP >> Just <$> parse) <|> return Nothing) identifierParser
    (liftM3 WSCap parse (argListParser parse) parse) parse

instance Parse Interface where
  parse = tokInterfaceP >> liftM3 Interface
    parse
    ((tokExtendsP >> sepBy1 parse tokCommaP) <|> return [])
    parse

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

