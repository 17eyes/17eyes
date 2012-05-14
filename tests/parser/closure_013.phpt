--TEST--
Closure 013: __invoke() on temporary result
?>
<?php
class Foo {
	function __invoke() {
		echo "Hello World!\n";
	}
}

function foo() {
	return function() {
		echo "Hello World!\n";
	};
}
$test = new Foo;
$test->__invoke();
$test = foo();
$test->__invoke();
$test = foo()->__invoke();
?>
?>
Hello World!
Hello World!
Hello World!
