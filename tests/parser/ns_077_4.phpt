--TEST--
077: Unknown compile-time constants in namespace
--FILE--
<?php
namespace foo;

function foo($a = array(0 => namespace\unknown))
{
}

foo();
?>
Fatal error: Undefined constant 'foo\unknown' in %sns_077_%d.php on line %d
