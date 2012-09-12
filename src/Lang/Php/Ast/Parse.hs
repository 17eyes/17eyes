{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}

module Lang.Php.Ast.Parse where

import Control.Monad.Identity
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import Lang.Php.Ast.Types
import Lang.Php.Ast.Unparse
import Text.ParserCombinators.Parsec.Expr
import qualified Data.Intercal as IC
import Data.Either
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
  try (liftM2 StmtStatic (tokStaticP >> sepBy1 parse tokCommaP) parse) <|>
  liftM2 (uncurry StmtExpr) parse parse <|>
  liftM2 StmtGlobal (tokGlobalP >> sepBy1 parse tokCommaP) parse <|>
  StmtInterface <$> parse <|>
  StmtNothing <$> parse <|>
  liftM3 StmtReturn (tokReturnP >> parse) (optionMaybe parse) parse <|>
  liftM2 StmtGoto (tokGotoP >> parse) parse <|>
  StmtNamespace <$> parse <|>
  (StmtUse <$>
    (tokUseP >> parse) <*>
    namespaceParser <*>
    (try $ Just <$> liftM2 (,) (liftM2 (,) parse (tokAsP >> parse))
      namespaceParser <|> return Nothing)) <|>
  StmtSwitch <$> parse <|>
  liftM2 StmtThrow (tokThrowP >> parse) parse <|>
  liftM2 StmtUnset
    (tokUnsetP >> liftM3 WSCap parse (issetListParser parse) parse)
    parse 

instance Parse (If, WS) where
  parse = do
    (e, wsl) <- tokIfP >> parenExprP
    syn <- option StdSyntax ((lookAhead tokColonP) >> return AltSyntax)
    block <- blockP syn
    elseifs <- many (elseifP syn)
    elseBlock <- optionMaybe (tokElseP >> blockP syn)
    if syn == AltSyntax then tokEndifP >> return () else return ()
    wsr <- parse
    return (If syn ((IfBlock e wsl block):elseifs) elseBlock, wsr)
   where
    elseifP :: StmtSyntax -> Parser IfBlock
    elseifP syn = do
        (e, wsl) <- tokElseifP >> parenExprP
        (block, wsr) <- blockP syn
        return (IfBlock e wsl (block, wsr))

    parenExprP :: Parser ((WSCap (WS, Expr)), WS)
    parenExprP = do
        (wsbp, wsap) <- liftM2 (,) parse (tokLParenP >> parse)
        (e, wsr) <- parse :: Parser (Expr, WS)
        ws <- tokRParenP >> parse
        return $ (WSCap wsbp (wsap, e) wsr, ws)

    blockP :: StmtSyntax -> Parser (BlockOrStmt, WS)
    blockP StdSyntax = parse :: Parser (BlockOrStmt, WS)

    blockP AltSyntax = do
        wsl <- tokColonP >> parse
        stmts <- stmtListParser
        wsr <- parse 
        ws <- parse   
        lookAhead (tokEndifP <|> tokElseP <|> tokElseifP)
        return (WSCap wsl (Right $ Block stmts) wsr, ws)

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

instance Parse (For, WS) where
  parse = tokForP >> do
    h <- liftM3 WSCap parse (tokLParenP >> liftM3 (,,) parse
      (tokSemiP >> parse <* tokSemiP) parse <* tokRParenP) parse
    (do -- try parsing for-endfor combination
        wsl <- parse <* tokColonP
        stmts <- stmtListParser
        wsr <- parse
        tokEndforP
        ws <- parse
        return (For h (WSCap wsl (Right $ Block stmts) wsr) AltSyntax, ws)
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

instance Parse (Foreach, WS) where
  parse = tokForeachP >> do
    h <- liftM3 WSCap parse (tokLParenP >> liftM3 (,,) (parse <* tokAsP)
      (optionMaybe (try $ parse <* tokDubArrowP)) (parse <* tokRParenP)) parse
    parseAltSyntax h <|> parseStdSyntax h
   where
    parseAltSyntax h = do
        wsl <- tokColonP >> parse
        stmts <- stmtListParser
        wsr <- parse
        tokEndforeachP
        ws <- parse
        return (Foreach h (WSCap wsl (Right $ Block stmts) wsr) AltSyntax, ws)

    parseStdSyntax h = do
        (block, ws) <- parse
        return (Foreach h block StdSyntax, ws)

instance Parse Func where
  parse = tokFunctionP >> 
    Func <$>
      parse <*>
      ((tokAmpP >> Just <$> parse) <|> return Nothing) <*>
      (Just <$> identifierParser) <*>
      (liftM3 WSCap parse (argListParser parse) parse) <*>
      (return Nothing) <*>
      parse

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
        (optionMaybe $ try $ liftM3 WSCap (tokClosePhpP >> parse) parse parse)
        (parse <* right)

instance Parse Case where
  parse = liftM2 Case
    ((tokDefaultP >> Left <$> parse) <|> (tokCaseP >> Right <$> parse))
    ((tokColonP <|> tokSemiP) >> stmtListParser)

instance Parse (While, WS) where
  parse = tokWhileP >> do
    e <- liftM3 WSCap parse (tokLParenP >> parse <* tokRParenP) parse
    (do -- try parse while-endwhile combination
        wsl <- tokColonP >> parse
        stmts <- stmtListParser
        wsr <- parse
        tokEndwhileP
        ws <- parse
        return (While e (WSCap wsl (Right $ Block stmts) wsr) AltSyntax, ws)
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

--
-- STRING LITERALS
--
-- Since they can contain parsable expressions (like "Hello, $name!") we need
-- to deal with them here.

instance Parse StrLit where
  parse = fmap StrLit $
    -- binary strings
    try (IC.Interend <$> liftM2 (++) (string "b'") (strLitRestParser '\'')) <|>
    liftM2 icGlue (string "b\"") (strLitRestParserExpr '"') <|>

    -- normal strings
    (char '"' >> strLitRestParserExpr '"' >>= return . icGlue "\"") <|>
    (IC.Interend <$> liftM2 (:) (char '\'') (strLitRestParser '\''))

strLitRestParser :: Char -> Parser String
strLitRestParser end = anyChar >>= \ c -> (c:) <$>
  if c == end then return [] else if c == '\\'
    then liftM2 (:) anyChar (strLitRestParser end)
    else strLitRestParser end

-- | Concatenation-like operation for Data.Intercal
icGlue :: String -> IC.Intercal String a -> IC.Intercal String a
icGlue g (IC.Interend xs) = IC.Interend (g ++ xs)
icGlue g (IC.Intercal xs ys ic) = IC.Intercal (g ++ xs) ys ic

-- Parses strings that can contain expressions (like "Hello, $name!" or more
-- complex things like "{$a["{$a}"]}" that actually affect the lexical
-- structure of the program).
strLitRestParserExpr :: Char -> Parser (IC.Intercal String (StrLitExprStyle, RVal))
strLitRestParserExpr end = do
    -- stupid hack: for '$' we don't want to consume the input
    c <- lookAhead anyChar
    if c /= '$' then anyChar >> return () else return ()
    case c of
        '\\' -> liftM2 (\x -> icGlue ['\\',x]) anyChar (strLitRestParserExpr end)
        '$'  -> try parseNormal <|> (anyChar >> icGlue "$" <$> strLitRestParserExpr end)
        '{'  -> parseComplex
        c    -> if c == end
                  then return (IC.Interend [end])
                  else icGlue [c] <$> strLitRestParserExpr end
 where
    parseComplex = do
        c <- lookAhead anyChar
        if c /= '$'
          then icGlue ['{',c] <$> strLitRestParserExpr end
          else do
            (expr, ws) <- parse :: Parser (RVal, WS)
            tokRBraceP
            IC.Intercal "" (SLEComplex, expr) <$> icGlue (unparse ws) <$> strLitRestParserExpr end

    parseNormal = do
        (expr, ws) <- parse :: Parser (RVal, WS)
        IC.Intercal "" (SLENormal, expr) <$> icGlue (unparse ws) <$> strLitRestParserExpr end

backticksParser :: Parser StrLit
backticksParser = char '`' >> StrLit <$> icGlue "`" <$> strLitRestParserExpr '`'

instance Parse NewDoc where
  parse = NewDoc <$> init <$> try mainP
   where
     mainP = do
       tokNewDocP >> wsNoNLParser
       eot_string <- char '\'' >> genIdentifierParser <* char '\''
       newline
       restP eot_string

     restP :: String -> Parser String
     restP s = try (endOfStringP s) <|> liftM2 (++) lineParser (restP s)

     endOfStringP s = do
       string s
       notFollowedBy (satisfy (\ c -> c /= '\n' && c /= ';'))
       return ""

instance Parse HereDoc where
  parse = HereDoc <$> do
    tokHereDocP >> wsNoNLParser
    eot_ident <- genIdentifierParser
             <|> (char '"' >> genIdentifierParser <* char '"')
    newline
    hereDocRestParser eot_ident

hereDocRestParser :: String -> Parser (IC.Intercal String (StrLitExprStyle, RVal))
hereDocRestParser s = try theEnd <|> singleLine
 where
   theEnd = do
     string s <* notFollowedBy (satisfy (\ c -> c /= '\n' && c /= ';'))
     return (IC.Interend "")

   singleLine = icCat <$> strLitRestParserExpr '\n' <*> hereDocRestParser s

   icCat :: IC.Intercal [a] b -> IC.Intercal [a] b -> IC.Intercal [a] b
   icCat (IC.Interend xs) (IC.Interend ys) = IC.Interend (xs ++ ys)
   icCat (IC.Interend xs) (IC.Intercal ys y ic) = IC.Intercal (xs ++ ys) y ic
   icCat (IC.Intercal xs x ic) ic' = IC.Intercal xs x (icCat ic ic')

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

instance Parse (DynConst, WS) where
  parse = liftM2 (,) stmts parse where
    stmts = do
      idents <- many $ try (parseIdent <* tokDubColonP)
      lastIdent <- parseIdent
      return $ DynConst idents lastIdent
     where
      parseIdent :: Parser (WSCap (Either String Var))
      parseIdent = liftM2 (capify) parse identifierOrVarParser
      
      identifierOrVarParser :: Parser ((Either String Var), WS)
      identifierOrVarParser =
        first Right <$> parse
        <|>
        liftM2 (,) (Left <$> (tokStaticP <|> identifierParser)) parse

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
     where
      dynConstOrConstParser :: Parser (Either DynConst Const, WS)
      dynConstOrConstParser =
        try (first Right <$> parse) <|> first Left <$> parse

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
      ws2 <- (tokArrowP >> parse) <|>
             (tokDubColonP >> parse)
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

instance Parse (Const, WS) where
  parse = liftM2 (,) stmts parse where
    stmts = do
      initIdent <- many (try $ liftM3 WSCap parse namespaceParser
        (parse <* tokDubColonP))
      lastIdent <- liftM3 WSCap parse namespaceParser parse
      return $ Const initIdent lastIdent

lRValOrConstParser :: Parser (Either LRVal Const, WS)
lRValOrConstParser = do
  (v, w) <- parse
  case v of
    ValLRVal a -> return (Left a, w)
    ValROnlyVal (ROnlyValConst a) -> return (Right a, w)
    _ -> fail "Expected LRVal or Const but fould a different Val type."

instance Parse (Expr, WS) where
  parse = buildExpressionParser exprParserTable simpleExprParser

simpleExprParser :: Parser (Expr, WS)
simpleExprParser = 
    try (liftM2 (,) (ExprStrLit <$> parse) parse)
  <|>
    try (liftM2 (,) (ExprNewDoc <$> parse) parse)
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
  <|> first (ExprRef) <$> parse
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


anonFuncParser = Func <$>
  parse <*>
  ((tokAmpP >> Just <$> parse) <|> return Nothing) <*>
  (return Nothing) <*>
  parseArgList <*>
  ((tokUseP >> Just <$> parseArgList) <|> return Nothing) <*>
  parse 

  where
    parseArgList = (liftM3 WSCap parse (argListParser parse) parse)

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

instance Parse (Ref, WS) where
  parse = do
    w <- tokAmpP >> parse
    first (Ref w . Right) <$> parse <|> do
      (e, wEnd) <- parse
      case e of
        ExprNew _ _ _ -> return (Ref w (Left e), wEnd)
        _ -> fail "Expecting a Val or ExprNew."

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
