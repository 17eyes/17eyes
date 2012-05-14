--TEST--
Testing dynamic call with invalid value for method name
?>
<?php 

$a = new stdClass;

$a::$a();

?>
?>
Fatal error: Function name must be a string in %s on line %d
