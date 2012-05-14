--TEST--
066: Name ambiguity (import name & internal class name)
?>
<?php
include __DIR__ . '/ns_027.inc';
use Foo\Bar\Foo as stdClass;

new stdClass();
?>
Foo\Bar\Foo
