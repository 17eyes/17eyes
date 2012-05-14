--TEST--
compact()
?>
unicode.script_encoding=UTF-8
unicode.output_encoding=UTF-8
?>
<?php

$çity  = "San Francisco";
$state = "CA";
$event = "SIGGRAPH";

$location_vars = array("c\u0327ity", "state");

$result = compact("event", $location_vars);
var_dump($result);
?>
?>
array(2) {
  ["event"]=>
  string(8) "SIGGRAPH"
  ["state"]=>
  string(2) "CA"
}
