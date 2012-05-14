--TEST--
Closure 033: Dynamic closure property and private function
?>
<?php

class Test {
	public $func;
	function __construct() {
		$this->func = function() {
			echo __METHOD__ . "()\n";
		};
	}
	private function func() {
		echo __METHOD__ . "()\n";
	}
}

$o = new Test;
$f = $o->func;
$f();
$o->func();

?>
=?>=
?>
Test::{closure}()

Fatal error: Call to private method Test::func() from context '' in %sclosure_033.php on line %d
