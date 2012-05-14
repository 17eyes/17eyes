--TEST--
Closure 028: Trying to use lambda directly in foreach
?>
<?php

foreach (function(){ return 1; } as $y) { 
	var_dump($y);	
}

print "ok\n";

?>
?>
ok
