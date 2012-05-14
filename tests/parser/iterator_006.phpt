--TEST--
SPL: IteratorIterator and SimpleXMlElement
--SKIPIF--
<?php if (!extension_loaded('simplexml')) print "skip SimpleXML required"; ?>
?>
<?php

$root = simplexml_load_string(b'XML version="1.0"?>
<root>
 <child>Hello</child>
 <child>World</child>
</root>
');

foreach (new IteratorIterator($root->child) as $child) {
	echo $child."\n";
}
?>
=?>=
<?php exit(0); ?>
?>
Hello
World
=?>=
