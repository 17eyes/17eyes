--TEST--
Torture the T_END_NOWDOC rules with variable expansions (nowdoc)
?>
<?php

require_once 'nowdoc.inc';
$fooledYou = '';

print <<<'ENDOFNOWDOC'
{$fooledYou}ENDOFNOWDOC{$fooledYou}
ENDOFNOWDOC{$fooledYou}
{$fooledYou}ENDOFNOWDOC

ENDOFNOWDOC;

$x = <<<'ENDOFNOWDOC'
{$fooledYou}ENDOFNOWDOC{$fooledYou}
ENDOFNOWDOC{$fooledYou}
{$fooledYou}ENDOFNOWDOC

ENDOFNOWDOC;

print "{$x}";

?>
?>
{$fooledYou}ENDOFNOWDOC{$fooledYou}
ENDOFNOWDOC{$fooledYou}
{$fooledYou}ENDOFNOWDOC
{$fooledYou}ENDOFNOWDOC{$fooledYou}
ENDOFNOWDOC{$fooledYou}
{$fooledYou}ENDOFNOWDOC

