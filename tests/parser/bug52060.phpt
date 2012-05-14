--TEST--
Bug #52060 (Memory leak when passing a closure to method_exists())
?>
<?php

$closure = function($a) { echo $a; };

var_dump(method_exists($closure, '__invoke')); // true

?>
?>
bool(true)
