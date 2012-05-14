--TEST--
Dynamic access of constants
?>
<?php
class A {
    const B = 'foo';
}

$classname       =  'A';
$wrongClassname  =  'B';

echo $classname::B."\n";
echo $wrongClassname::B."\n";
?>
=?>=
?>
foo

Fatal error: Class 'B' not found in %s042.php on line %d
