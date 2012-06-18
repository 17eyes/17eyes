module Kinds where

-- | All issue kinds should be declared here. You should use either `newKind'
-- or `newKind_' for this purpose. In the future, we may decide to generate
-- this module from some kind of issue kind database.

import Issue

parseError = newKind "Pipeline" "parseError"
  "parse error" Critical Likely id

tabsVsSpaces = newKind "CharAnalysis" "tabsVsSpaces"
  "code mixes tabs with spaces" Style Possible $
  \(chosen, other) ->
       "The code in this file is indented mostly using " ++ chosen ++ " but "
    ++ "still contains some " ++ other ++ ". This may be confusing for some "
    ++ "editors and source control programs."

lineLengthInconsistent = newKind_ "CharAnalysis" "lineLengthInconsistent"
  "file contains long lines" Style Sure $
     "Lines in this file are very long. To improve readability, code line "
  ++ "length is usually limited to some number of characters."

lineLength = newKind "CharAnalysis" "lineLength"
  "line too long" Style Possible $
  \lim ->
     "Almost all lines in this file are shorter than " ++ lim ++ " " ++
     "characters, but this one is longer. It is often desirable to " ++
     "maintain a consistent line length convention."

backwardGoto = newKind_ "Lang.Php.Ast.Analysis.Goto" "detectBackwardGoto"
  "goto instead of structural loop" Style Likely $
     "This goto statement jumps backwards. Although this is correct, such "
  ++ "control flow may sometimes be not very obvious and would be better "
  ++ "expressed as a loop (while or do-while)."

badLabelsMissing = newKind "Lang.Php.Ast.Analysis.Goto" "badLabelsMissing"
  "goto to a missing label" Critical Likely $
  \lab ->
       "This goto statement references a nonexistent label `" ++ lab ++ "'."
    ++ "This will trigger a runtime error when executed."

badLabelsRedundant = newKind "Lang.Php.Ast.Analysis.Goto" "badLabelsRedundant"
  "unused label" Style Sure $
  \lab ->
       "The label `" ++ lab ++ "' was declared but there is no goto statement "
    ++ "jumping to it. Maybe the name of the label was misspelled somewhere."

loopGoto = newKind_ "Lang.Php.Ast.Analysis.Goto" "loopGoto"
  "goto to an inside of a loop" Critical Sure $
     "This goto statement jumps to an inside of a loop. This is prohibited in "
  ++ "PHP and will result in a runtime error."

unreachableCode = newKind_ "Lang.Php.Ast.Analysis.Reachability" "unreachableCode"
  "possibly unreachableCode" Nitpicking Possible $
     "It might be possible that this code will never get executed. This may "
  ++ "be an indicator of an underlying bug."

switchFallthrough = newKind_ "Lang.Php.Ast.Analysis.Reachability" "switchFallthrough"
  "`case' block without `break'" Style Possible $
     "This 'case' block lacks a 'break' statement and will pass control to "
  ++ "the next one. Note that sometimes this is the desired behavior."

switchLastFallthrough = newKind_ "Lang.Php.Ast.Analysis.Reachability" "switchLastFallthrough"
  "last `case' block without `break'" MayHarm Sure $
     "This 'case' block is the last one and contains no `break'. This doesn't "
  ++ "affect the behavior in any way, but if another 'case' is going to be "
  ++ "added at the end, care must be taken to avoid unintentional fallthrough."

styleFinishPhp = newKind_ "Lang.Php.Ast.Analysis.Style" "styleFinishPhp"
  "PHP closing tag at the end of a file" Style Sure
  "This file ends with a closing PHP tag (like \"?>\"). If such tag is followed \
  \by white space at the end of a file this can cause HTTP headers to be sent \
  \sooner than expected."

styleIncludeTopLevel = newKind_ "Lang.Php.Ast.Analysis.Style" "styleIncludeTopLevel"
  "top-level includes should be done with require_once" Style Sure
  "Including files with require_once prevents the script from further execution \
  \if the inclusion fails. It also disallows including the same file multiple \
  \times. This is usually the desired behavior for top-level include \
  \statements."

styleIncludeDeep = newKind_ "Lang.Php.Ast.Analysis.Style" "styleIncludeDeep"
  "non-top-level includes should be done with include_once" Style Sure
  "Including files with include_once disallows including the same file many \
  \times but still allows the script to continue if the inclusion fails. This \
  \is usually the desired behavior of include statements inside functions and \
  \control structures (e.g. those used to load plugins)."

styleStringLiterals = newKind_ "Lang.Php.Ast.Analysis.Style" "styleStringLiterals"
  "simple string literals should be single-quoted" Style Sure
  "If you don't use variables or special escapes inside string literals, it \
  \might be better to put them in single quotes ('...').  This helps avoiding \
  \unintended expansion of variables inside string literals."

styleFunctionCalls = newKind_ "Lang.Php.Ast.Analysis.Style" "styleFunctionCalls"
  "there should be no space before parentheses in function calls" Style Sure
  "The usual convention is to put no whitespace between the function name and \
  \the opening parenthesis in a function call. This helps visually distinguish \
  \function calls from statements like \"if\" or \"while\"."

styleClassFunctionDeclaration = newKind_ "Lang.Php.Ast.Analysis.Style" "styleClassFunctionDeclaration"
  "class and function declarations should have their opening brace on a new line" Style Sure
  "By convention the opening brace after class and function declarations is \
  \usually in the next line. This serves as a visual aid to distinguish these \
  \declarations from statements like \"if\" or \"switch\"."

styleShortTags = newKind_ "Lang.Php.Ast.Analysis.Style" "styleShortTags"
  "short tags are not portable" Style Sure
  "Short tags <? and <?= might be not supported by the PHP interpreter, thus \
  \using short tags on some servers may lead to information leak. It's \
  \recommended to use long tags, since they're supported by all PHP versions."

styleControlStructs = newKind_ "Lang.Php.Ast.Analysis.Style" "styleControlStructs"
  "blocks in the control statements should be sounded by parenthesis" Style Sure
  "Lack of the parenthesis may lead to logic errors when new lines are introduced. \
  \Having them also increases readability of the code. "

styleControlStructsWS = newKind_ "Lang.Php.Ast.Analysis.Style" "styleControlStructsWS"
  "control statements should have one space between the keyword and parenthesis" Style Sure
  "Control statements should have one space between the control keyword and \
  \opening parenthesis, in order to distinguish them from function calls."

styleControlStructsSwitchDefault = newKind_ "Lang.Php.Ast.Analysis.Style"
  "styleControlStructsSwitchDefault"
  "switch should always include default branch" Style Sure
  "Switch should always include default branch otherwise it may lead to logic \
  \bugs connected with lack of a default action."

funcInfo = newKind "Lang.Php.Ast.Analysis.Func" "funcInfo"
  "suspicious function usage" MayHarm Sure
  (\(fname, desc) -> concat[fname,": ",desc])
