--TEST--
callback function is a closure
--SKIPIF--
<?php if (!extension_loaded("filter")) die("skip"); ?>
?>
<?php
$callback = function ($var) {
	return $var;
};
$var = "test";
var_dump(filter_var($var, FILTER_CALLBACK, array('options'=> $callback)));
?>
?>	
string(4) "test"
