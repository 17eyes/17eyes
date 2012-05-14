--TEST--
Bug #43344.10 (Wrong error message for undefined namespace constant)
?>
<?php
echo namespace\bar."\n";
?>
?>
Fatal error: Undefined constant 'bar' in %sbug43344_10.php on line %d
