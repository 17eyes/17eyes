--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

module Lang.Php.Ast.Analysis.Func(allAnalyses) where

import Lang.Php.Ast
import Lang.Php.Ast.Traversal

import qualified Kinds
import qualified Issue

allAnalyses = [funcAnalysis]

data InfoFuncLevel = FuncInfo | FuncWarn | FuncDeny | FuncDeprecated |
    FuncNotPortable

data InfoFunc = InfoFunc {
  infoFuncName  :: String,
  infoFuncLevel :: InfoFuncLevel,
  infoFuncDesc  :: String
}
    
-- XXX: Could be a bit uneffective, should we use Sets here?
-- XXX: This is onle small stub of the desired database (#77)
funcs = [InfoFunc "crc32" FuncWarn cryptoWarn,
  InfoFunc "str_rot13" FuncWarn cryptoWarn,
  InfoFunc "base64_encode" FuncInfo cryptoWarn,
  InfoFunc "base64_decode" FuncInfo cryptoWarn,
  InfoFunc "mt_srand" FuncDeny "XXX: joomla exploit - read more",

  -- DEPRECATED IN 5.3 based on: http://php.net/manual/en/migration53.deprecated.php
  InfoFunc "call_user_method" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "call_user_method_array" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "define_syslog_variables" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "dl" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "ereg" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "ereg_replace" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "eregi" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "eregi_replace" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "set_magic_quotes_runtime" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "session_register" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "session_unregister" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "session_is_registered" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "set_socket_blocking" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "split" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "spliti" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "sql_regcase" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "mysql_db_query" FuncDeprecated (funcDeprecated "5.3"),
  InfoFunc "mysql_escape_string" FuncDeprecated (funcDeprecated "5.3"),

  -- DEPRECATED IN 5.4 based on: http://php.net/manual/en/migration54.deprecated.php
  InfoFunc "mcrypt_generic_end" FuncDeprecated (funcDeprecated "5.4"),
  InfoFunc "mysql_list_dbs" FuncDeprecated (funcDeprecated "5.4")]

cryptoWarn = "Please review if this function is not used in a cryptographic context"

funcDeprecated :: String -> String
funcDeprecated version = "Function used is deprecated by the " ++ version ++
  " of the PHP"

funcAnalysis :: AstAnalysis
funcAnalysis = AstAnalysis () analysis
 where
    analysis :: ROnlyVal -> TraverseState () ROnlyVal

    -- XXX: ROnlyValFunc could be either a() or $a(), for now we're not able to
    --      figure out what's executed on dynamic call (at least until constant
    --      propagation.
    analysis x@(ROnlyValFunc (Right (Const _ funcName)) _ _) =
      case getFuncInfo funcs (wsCapMain funcName) of
        Just (InfoFunc name level desc) ->
          (emitIssue Kinds.funcInfo [unparse x] (name, desc))
          >> return x
        _ -> return x

    analysis x = return x

    -- Get InfoFunc
    getFuncInfo :: [InfoFunc] -> String -> Maybe InfoFunc
    getFuncInfo funcs func = find (\(InfoFunc name _ _) -> func == name) funcs
