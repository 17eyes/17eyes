--TEST--
Bug #43344.13 (Wrong error message for undefined namespace constant)
?>
<?php
function f($a=array(namespace\bar=>0)) {
	reset($a);
	return key($a);
}
echo f()."\n";
?>
?>
Fatal error: Undefined constant 'bar' in %sbug43344_13.php on line %d
