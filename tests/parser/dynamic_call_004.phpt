--TEST--
Testing dynamic call with undefined variables
?>
<?php 

$a::$b();

?>
?>
Notice: Undefined variable: a in %s on line %d

Fatal error: Class name must be a valid object or a string in %s on line %d
