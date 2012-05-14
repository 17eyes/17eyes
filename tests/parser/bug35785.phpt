--TEST--
Bug #35785 (SimpleXML memory read error)
--SKIPIF--
<?php if (!extension_loaded("simplexml")) print "skip"; ?>
?>
<?php

$xml = simplexml_load_string("<root></root>");
$xml->bla->posts->name = "FooBar";
echo $xml->asXML();
$xml = simplexml_load_string("<root></root>");
$count = count($xml->bla->posts);
var_dump($count);
$xml->bla->posts[$count]->name = "FooBar";
echo $xml->asXML();
$xml = simplexml_load_string("<root></root>");
$xml->bla->posts[]->name = "FooBar";
echo $xml->asXML();
?>
=?>=
<?php exit(0); __halt_compiler(); ?>
?>
XML version="1.0"?>
<root><bla><posts><name>FooBar</name></posts></bla></root>
int(0)
XML version="1.0"?>
<root><bla><posts><name>FooBar</name></posts></bla></root>
XML version="1.0"?>
<root><bla><posts><name>FooBar</name></posts></bla></root>
=?>=
