--TEST--
077: Unknown compile-time constants in namespace
--FILE--
<?php
namespace foo;

function foo($a = array(0 => \unknown))
{
}

foo();
?>
Fatal error: Undefined constant 'unknown' in %sns_077_%d.php on line %d
