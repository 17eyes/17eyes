--TEST--
Dynamic call for static methods
?>
<?php
class A {
    static function foo() { return 'foo'; }
}

$classname       =  'A';
$wrongClassname  =  'B';

echo $classname::foo()."\n";
echo $wrongClassname::foo()."\n";
?>
=?>=
?>
foo

Fatal error: Class 'B' not found in %s043.php on line %d
