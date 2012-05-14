--TEST--
Reflection::getClosureScopeClass()
--SKIPIF--
<?php
if (!extension_loaded('reflection') || !defined('PHP_VERSION_ID') || PHP_VERSION_ID < 50399) {
  print 'skip';
}
?>
?> 
<?php
$closure = function($param) { return "this is a closure"; };
$rf = new ReflectionFunction($closure);
var_dump($rf->getClosureScopeClass());

Class A {
	public static function getClosure() {
		return function($param) { return "this is a closure"; };
	}
}

$closure = A::getClosure();
$rf = new ReflectionFunction($closure);
var_dump($rf->getClosureScopeClass());
echo "Done!\n";
?>
NULL
object(ReflectionClass)#%d (1) {
  ["name"]=>
  string(1) "A"
}
Done!
