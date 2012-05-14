--TEST--
Heredoc with double quotes
?>
<?php
$test = "foo";
$var = <<<"MYLABEL"
test: $test
MYLABEL;
echo $var;
?>
?>
test: foo
