--TEST--
077: Unknown compile-time constants in namespace
?>
<?php

function foo($a = array(namespace\unknown => unknown))
{
}

foo();
?>
Fatal error: Undefined constant 'unknown' in %sns_077_%d.php on line %d
