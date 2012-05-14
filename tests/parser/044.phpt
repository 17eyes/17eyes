--TEST--
Dynamic call for static methods dynamically named
?>
<?php
class A {
    static function foo() { return 'foo'; }
}
$classname        =  'A';
$wrongClassname   =  'B';

$methodname       =  'foo';

echo $classname::$methodname()."\n";

echo $wrongClassname::$methodname()."\n";
?>
=?>=
?>
foo

Fatal error: Class 'B' not found in %s044.php on line %d
