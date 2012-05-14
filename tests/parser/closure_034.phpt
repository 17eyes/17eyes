--TEST--
Closure 033: Recursive var_dump on closures
?>
<?php

$a = function () use(&$a) {};
var_dump($a);

?>
=?>=
?>
object(Closure)#%d (1) {
  ["static"]=>
  array(1) {
    ["a"]=>
    *RECURSION*
  }
}
=?>=
