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

module Lang.Php.Ast.Lex where

import Lang.Php.Type.PhpNum
import Lang.Php.Ast.Common
import qualified Data.Set as Set

--
-- Number literals
--
-- Documentation for integers:
-- http://php.net/manual/en/language.types.integer.php
--

data NumLit = NumLit String PhpNum
  deriving (Eq, Show, Typeable, Data)

instance Parse NumLit where
  -- could be tighter
  parse = do
    strNumber <- liftM2 (:) (char '0') (hexNum <|> binNum <|> numRest) <|>
      liftM2 (:) (oneOf ['1'..'9']) numRest <|>
      -- special case .[0-9]+([eE][+-]?[0-9]+)? is a number!
      (lookAhead (char '.') >> numRest)
    return $ NumLit strNumber (PhpNum $ parseNumber strNumber)
    where

    numRest :: Parser String
    numRest = do
      prePt <- octdecNum
      pt <- ptAndRest
      sciNot <- sciNot
      return $ concat [prePt,pt,sciNot]

    -- parses: .[0-9]
    ptAndRest :: Parser String
    ptAndRest = option ""
      (try (liftM2 (:) (char '.') (many . oneOf $ ['0'..'9'])))

    -- parses: .[+-][0-9]
    sciNot :: Parser String
    sciNot = option ""
      (try $ liftM2 (++)
       (liftM2 (:) (oneOf ['e','E']) (tokPlusP <|> tokMinusP <|> return "")) 
       (many1 . oneOf $ ['0'..'9']))

    -- binary numbers (since PHP 5.4)
    -- parses: b[0-1]+
    binNum = liftM2 (:) (char 'b') (many1 . oneOf $ ['0','1'])

    -- As PHP docs says: [..] If an invalid digit is given in an octal integer
    -- (i.e. 8 or 9), the rest of the number is ignored. [...], That's why
    -- those two options are connected.
    -- parses: [0-9]*
    octdecNum = many . oneOf $ ['0'..'9']

    -- hex numbers
    -- parses: x[0-9a-fA-F]+
    hexNum = liftM2 (:) (oneOf ['X','x'])
      (many1 . oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])

    -- Parse number
    parseNumber :: String -> Either Integer Double
    parseNumber strNum = let lowerStrNum = map toLower strNum in
      if '.' `elem` strNum ||
      (('x' `notElem` lowerStrNum) &&
       ('e' `elem` lowerStrNum))
      then Right $ parseFloat lowerStrNum
      else Left $ parseInteger lowerStrNum 0 0

    -- XXX: verify if this trick is sufficent for the PHP
    parseFloat :: String -> Double
    parseFloat x = read . concat $
      ["0", takeWhile ((/=) 'e') x, "0", dropWhile ((/=) 'e') x]

    -- This function assumes that given string is valid string (valid in the
    --  PHP world)
    -- XXX: refactor me, too complicated
    parseInteger :: String -> Integer -> Integer -> Integer
    parseInteger [] _ acc = acc
    parseInteger (x:xs) base acc =
      case x of
        'b' -> parseInteger xs 2 0
        'x' -> parseInteger xs 16 0
        x -> if x == '0' && base == 0
          then parseInteger xs 8 0
          else (case base of
              -- special case: we hit first digit of 10-based number
              0 -> parseInteger xs 10 (toInteger $ digitToInt x)
              -- special case: first non oct digit ends oct numbers
              8 -> if isOctDigit x
                    then parseInteger xs 8
                      (acc * base + (toInteger $ digitToInt x))
                    else acc
              base -> parseInteger xs base
                (acc * base + (toInteger $ digitToInt x))
            )

instance Unparse NumLit where
  unparse (NumLit a _) = a

lineParser :: Parser String
lineParser = liftM2 (++) (many $ satisfy (/= '\n')) ((:[]) <$> newline)

---
--- As PHP Language Reference says:
---
--- Variable names follow the same rules as other labels in PHP. A valid
--- variable name starts with a letter or underscore, followed by any number
--- of letters, numbers, or underscores. As a regular expression, it would be
--- expressed thus: '[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*' 
---
--- Source : http://www.php.net/manual/en/language.variables.basics.php
---

identStartChars :: String
identStartChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['\x7f'..'\xff']

identEndChars :: String
identEndChars = identStartChars ++ ['0'..'9']

genIdentifierParser :: Parser String
genIdentifierParser =
  liftM2 (:) (oneOf identStartChars)
             (many $ oneOf identEndChars)

-- FIXME: ugly hack
-- this parser is used for parsing namespace names
namespaceParser :: Parser String
namespaceParser = try $ do 
  i <- liftM2 (:) (oneOf $ identStartChars ++ ['\\'])
    (many $ oneOf $ identEndChars ++ ['\\'])
  when (map toLower i `Set.member` reservedWords) $
    fail "Found reserved word when expecting identifier."
  return i

identifierParser :: Parser String
identifierParser = try $ do
  i <- genIdentifierParser
  when (map toLower i `Set.member` reservedWords) $
    fail "Found reserved word when expecting identifier."
  return i

-- must be given lowercase
charCI :: Char -> Parser Char
charCI c = satisfy ((== c) . toLower)

-- must be given lowercase
stringCI :: String -> Parser String
stringCI = mapM charCI

-- idk why but we need an explicit specialized type instead of using (string)
-- directly
s :: String -> Parser String
s = string

nc t cs = try $ s t <* notFollowedBy (oneOf cs)

-- ugly, redo this.. maybe have a minimal lexer stage after all?
tokNot = "!"
tokNotP = nc tokNot "="
tokNE = "!="
tokNEP = nc tokNE "="
tokNI = "!=="
tokNIP = try $ s tokNI
tokDollar = "$"
tokDollarP = s tokDollar
tokMod = "%"
tokModP = nc tokMod "="
tokModBy = "%="
tokModByP = try $ s tokModBy
tokAmp = "&"
tokAmpP = nc tokAmp "&="
tokAnd = "&&"
tokAndP = try $ s tokAnd
tokBitAndBy = "&="
tokBitAndByP = try $ s tokBitAndBy
tokLParen = "("
tokLParenP = s tokLParen
tokRParen = ")"
tokRParenP = s tokRParen
tokMul = "*"
tokMulP = nc tokMul "=/"
tokMulBy = "*="
tokMulByP = try $ s tokMulBy
tokPlus = "+"
tokPlusP = nc tokPlus "+="
tokIncr = "++"
tokIncrP = try $ s tokIncr
tokPlusBy = "+="
tokPlusByP = try $ s tokPlusBy
tokComma = ","
tokCommaP = s tokComma
tokMinus = "-"
tokMinusP = nc tokMinus "-=>"
tokDecr = "--"
tokDecrP = try $ s tokDecr
tokMinusBy = "-="
tokMinusByP = try $ s tokMinusBy
tokArrow = "->"
tokArrowP = try $ s tokArrow
tokConcat = "."
tokConcatP = nc tokConcat "="
tokConcatBy = ".="
tokConcatByP = try $ s tokConcatBy
tokDiv = "/"
tokDivP = nc tokDiv "=*/"
tokDivBy = "/="
tokDivByP = try $ s tokDivBy
tokColon = ":"
tokColonP = nc tokColon ":"
tokDubColon = "::"
tokDubColonP = try $ s tokDubColon
tokSemi = ";"
tokSemiP = s tokSemi
tokLT = "<"
tokLTP = nc tokLT "<=>"
tokShiftL = "<<"
tokShiftLP = nc tokShiftL "<="
tokHereDoc = "<<<"
tokHereDocB = "b<<<"
tokHereDocP = (try $ s tokHereDoc) <|> (try $ s tokHereDocB)
tokNewDocP = tokHereDocP
tokShiftLBy = "<<="
tokShiftLByP = try $ s tokShiftLBy
tokLE = "<="
tokLEP = try $ s tokLE
tokNEOld = "<>"
tokNEOldP = try $ s tokNEOld
tokOpenPhp = "<?php"
tokOpenPhpP = try $ s "<?" >> optional (identCI "php")
tokOpenPhpEcho = "<?="
-- no tokOpenPhpEchoP, done manually currently, has weird rules
tokEquals = "="
tokEqualsP = nc tokEquals "=>"
tokEQ = "=="
tokEQP = nc tokEQ "="
tokID = "==="
tokIDP = try $ s tokID
tokDubArrow = "=>"
tokDubArrowP = try $ s tokDubArrow
tokGT = ">"
tokGTP = nc tokGT "=>"
tokGE = ">="
tokGEP = try $ s tokGE
tokShiftR = ">>"
tokShiftRP = nc tokShiftR "="
tokShiftRBy = ">>="
tokShiftRByP = try $ s tokShiftRBy
tokQMark = "?"
tokQMarkP = nc tokQMark ">"
tokClosePhp = "?>"
tokClosePhpP = try $ s tokClosePhp
tokAt = "@"
tokAtP = s tokAt
tokLBracket = "["
tokLBracketP = s tokLBracket
tokRBracket = "]"
tokRBracketP = s tokRBracket
tokXor = "^"
tokXorP = nc tokXor "="
tokXorBy = "^="
tokXorByP = try $ s tokXorBy
tokLBrace = "{"
tokLBraceP = s tokLBrace
tokBitOr = "|"
tokBitOrP = nc tokBitOr "=|"
tokBitOrBy = "|="
tokBitOrByP = try $ s tokBitOrBy
tokOr = "||"
tokOrP = try $ s tokOr
tokRBrace = "}"
tokRBraceP = s tokRBrace
tokBitNot = "~"
tokBitNotP = s tokBitNot

tokAbstract = "abstract"
tokAndWd = "and"
tokArray = "array"
tokAs = "as"
tokBreak = "break"
tokCase = "case"
tokCatch = "catch"
tokClass = "class"
tokClone = "clone"
tokConst = "const"
tokContinue = "continue"
tokDeclare = "declare"
tokDefault = "default"
tokDie = "die"
tokDo = "do"
tokEcho = "echo"
tokElse = "else"
tokElseif = "elseif"
tokEmpty = "empty"
tokEnddeclare = "enddeclare"
tokEndfor = "endfor"
tokEndforeach = "endforeach"
tokEndif = "endif"
tokEndswitch = "endswitch"
tokEndwhile = "endwhile"
tokEval = "eval"
tokExit = "exit"
tokExtends = "extends"
tokFinal = "final"
tokFor = "for"
tokForeach = "foreach"
tokFunction = "function"
tokGlobal = "global"
tokGoto = "goto"
tokIf = "if"
tokImplements = "implements"
tokInclude = "include"
tokIncludeOnce = "include_once"
tokInstanceof = "instanceof"
tokInterface = "interface"
tokIsset = "isset"
tokList = "list"
tokNamespace = "namespace"
tokNew = "new"
tokOrWd = "or"
tokPrint = "print"
tokPrivate = "private"
tokProtected = "protected"
tokPublic = "public"
tokRequire = "require"
tokRequireOnce = "require_once"
tokReturn = "return"
tokStatic = "static"
tokSwitch = "switch"
tokThrow = "throw"
tokTry = "try"
tokUnset = "unset"
tokUse = "use"
tokVar = "var"
tokWhile = "while"
tokXorWd = "xor"

reservedWords :: Set.Set String
reservedWords = Set.fromList [
  tokAbstract,
  tokAndWd,
  tokArray,
  tokAs,
  tokBreak,
  tokCase,
  tokCatch,
  tokClass,
  tokClone,
  tokConst,
  tokContinue,
  tokDeclare,
  tokDefault,
  tokDie,
  tokDo,
  tokEcho,
  tokElse,
  tokElseif,
  tokEmpty,
  tokEnddeclare,
  tokEndfor,
  tokEndforeach,
  tokEndif,
  tokEndswitch,
  tokEndwhile,
  tokEval,
  tokExit,
  tokExtends,
  tokFinal,
  tokFor,
  tokForeach,
  tokFunction,
  tokGlobal,
  tokGoto,
  tokIf,
  tokImplements,
  tokInclude,
  tokIncludeOnce,
  tokInstanceof,
  tokInterface,
  tokIsset,
  tokList,
  tokNamespace,
  tokNew,
  tokOrWd,
  tokPrint,
  tokPrivate,
  tokProtected,
  tokPublic,
  tokRequire,
  tokRequireOnce,
  tokReturn,
  tokStatic,
  tokSwitch,
  tokThrow,
  tokTry,
  tokUnset,
  tokUse,
  tokVar,
  tokWhile,
  tokXorWd]

identCI w = try $ do
  i <- genIdentifierParser
  when (map toLower i /= w) $ fail ""
  return i

identsCI w = try $ do
  i <- genIdentifierParser
  when (map toLower i `notElem` w) $ fail ""
  return i

tokAbstractP = identCI tokAbstract
tokAndWdP = identCI tokAndWd
tokArrayP = identCI tokArray
tokAsP = identCI tokAs
tokBreakP = identCI tokBreak
tokCaseP = identCI tokCase
tokCatchP = identCI tokCatch
tokClassP = identCI tokClass
tokCloneP = identCI tokClone
tokConstP = identCI tokConst
tokContinueP = identCI tokContinue
tokDeclareP = identCI tokDeclare
tokDefaultP = identCI tokDefault
tokDieP = identCI tokDie
tokDoP = identCI tokDo
tokEchoP = identCI tokEcho
tokElseifP = identCI tokElseif
tokElseP = identCI tokElse
tokEmptyP = identCI tokEmpty
tokEnddeclareP = identCI tokEnddeclare
tokEndforeachP = identCI tokEndforeach
tokEndforP = identCI tokEndfor
tokEndifP = identCI tokEndif
tokEndswitchP = identCI tokEndswitch
tokEndwhileP = identCI tokEndwhile
tokEvalP = identCI tokEval
tokExitP = identCI tokExit
tokExtendsP = identCI tokExtends
tokFinalP = identCI tokFinal
tokForP = identCI tokFor
tokForeachP = identCI tokForeach
tokFunctionP = identCI tokFunction
tokGlobalP = identCI tokGlobal
tokGotoP = identCI tokGoto
tokIfP = identCI tokIf
tokImplementsP = identCI tokImplements
tokInstanceofP = identCI tokInstanceof
tokInterfaceP = identCI tokInterface
tokIssetP = identCI tokIsset
tokListP = identCI tokList
tokNamespaceP = identCI tokNamespace
tokNewP = identCI tokNew
tokOrWdP = identCI tokOrWd
tokPrintP = identCI tokPrint
tokPrivateP = identCI tokPrivate
tokProtectedP = identCI tokProtected
tokPublicP = identCI tokPublic
tokReturnP = identCI tokReturn
tokStaticP = identCI tokStatic
tokSwitchP = identCI tokSwitch
tokThrowP = identCI tokThrow
tokTryP = identCI tokTry
tokUnsetP = identCI tokUnset
tokUseP = identCI tokUse
tokVarP = identCI tokVar
tokWhileP = identCI tokWhile
tokXorWdP = identCI tokXorWd

tokCategory = "category"
tokCategoryP = identCI tokCategory
tokChildren = "children"
tokChildrenP = identCI tokChildren
tokAttribute = "attribute"
tokAttributeP = identCI tokAttribute

$(derive makeBinary ''NumLit)
