--TEST--
Closure 029: Testing lambda with instanceof operator
?>
<?php

var_dump(function() { } instanceof closure);
var_dump(function(&$x) { } instanceof closure);
var_dump(@function(&$x) use ($y, $z) { } instanceof closure);

?>
?>
bool(true)
bool(true)
bool(true)
