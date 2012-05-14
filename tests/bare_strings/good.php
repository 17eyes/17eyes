<?php
$a = array("foo" => "bar");
define("foo", "foo");
echo $a[foo]; // If foo is defined as a constant we shouldn't complain.
