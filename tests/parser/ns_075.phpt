--TEST--
075: Redefining compile-time constants
--FILE--
<?php
namespace foo;
const NULL = 1;

echo NULL;
?>
Fatal error: Cannot redeclare constant 'NULL' in %sns_075.php on line %d
