--TEST--
Using lambda with list()
?>
<?php

list($x, $y) = function() { };

var_dump($x, $y);

?>
?>
NULL
NULL
