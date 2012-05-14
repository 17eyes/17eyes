--TEST--
061: use in global scope
?>
<?php
class A {}
use \A as B;
echo get_class(new B)."\n";
?>
A
