--TEST--
ZE2 Late Static Binding creating a new class with 'static'
?>
<?php

class TestClass {
	public static function createInstance() {
		return new static();
	}
}

class ChildClass extends TestClass {}

$testClass = TestClass::createInstance();
$childClass = ChildClass::createInstance();

echo get_class($testClass) . "\n";
echo get_class($childClass) . "\n";
?>
?>
?>
TestClass
ChildClass
?>
