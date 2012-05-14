--TEST--
Closure 007: Nested lambdas in classes
?>
<?php

class A {
	private $x = 0;

	function getClosureGetter () {
		return function () {
			return function () {
				$this->x++;
			};
		};
	}

	function printX () {
		echo $this->x."\n";
	}
}

$a = new A;
$a->printX();
$getClosure = $a->getClosureGetter();
$a->printX();
$closure = $getClosure();
$a->printX();
$closure();
$a->printX();

echo "Done\n";
?>
?>
0
0
0
1
Done
