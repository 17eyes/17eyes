<?php
$a = array("foo" => "bar");
echo $a[foo];
// If foo is undefined it becomes a string (deprecated feature). This is 
// somewhat less important since PHP dynamically wanrns about it.
