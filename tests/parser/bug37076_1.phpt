--TEST--
Bug #37076 (SimpleXML ignores .=) (appending to unnamed attribute)
--SKIPIF--
<?php if (!extension_loaded("simplexml")) print "skip"; ?>
?>
<?php
$xml = simplexml_load_string("<root><foo /></root>");
$xml->{""} .= "bar";
print $xml->asXML();
?>
=?>=
?>
Warning: main(): Cannot write or create unnamed element in %s on line %d

Warning: main(): Cannot write or create unnamed element in %s on line %d
XML version="1.0"?>
<root><foo/></root>
=?>=
