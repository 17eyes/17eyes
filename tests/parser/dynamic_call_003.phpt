--TEST--
Testing dynamic call with invalid method name
?>
<?php 

$a = new stdClass;
$b = 1;

$a::$b();

?>
?>
Fatal error: Function name must be a string in %s on line %d
