--TEST--
Closure 045: Closures created in static methods are static, even without the keyword
?>
<?php

class A {
static function foo() {
	return function () {};
}
}

$a = A::foo();
$a->bindTo(new A);

echo "Done.\n";

?>
Warning: Cannot bind an instance to a static closure in %s on line %d
Done.
