--TEST--
065: Multiple names in use statement
?>
<?php
use X\Y as test, X\Z as test2;

require "ns_065.inc";

test\foo();
test2\foo();
?>
X\Y\foo
X\Z\foo
