--TEST--
Closure 002: Lambda with lexical variables (global scope)
?>
<?php

$x = 4;

$lambda1 = function () use ($x) {
	echo "$x\n";
};

$lambda2 = function () use (&$x) {
	echo "$x\n";
};

$lambda1();
$lambda2();
$x++;
$lambda1();
$lambda2();

echo "Done\n";
?>
?>
4
4
4
5
Done
