--TEST--
Testing recursion detection with Closures
?>
<?php

$x = function () use (&$x) {
	$h = function () use ($x) {
		var_dump($x);
		return 1;
	};	
	return $h();
};

var_dump($x());

?>
?>
object(Closure)#%d (1) {
  ["static"]=>
  array(1) {
    ["x"]=>
    *RECURSION*
  }
}
int(1)
