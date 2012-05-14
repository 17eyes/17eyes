--TEST--
Closure 024: Clone the Closure object
?>
<?php

$a = 1;
$c = function($add) use(&$a) { return $a+$add; };

$cc = clone $c;

echo $c(10)."\n";
echo $cc(10)."\n";

$a++;

echo $c(10)."\n";
echo $cc(10)."\n";

echo "Done.\n";
?>
?>
11
11
12
12
Done.
