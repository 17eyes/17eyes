--TEST--
Bug #41287 (Writing empty tags with Xmlwriter::WriteElement[ns])
--SKIPIF--
<?php 
if (!extension_loaded("xmlwriter")) die("skip"); 
?>
?>
<?php
$xml = new XmlWriter();
$xml->openMemory();
$xml->setIndent(true);
$xml->startDocument();
$xml->startElement('test');
$xml->writeElement('foo', null);
$xml->writeElement('foo2', "");
$xml->writeElement('foo3');
$xml->startElement('bar');
$xml->endElement('bar');
$xml->endElement();
$xml->endElement();
print $xml->flush(true);

print "\n";

$xw = new XMLWriter();
$xw->openMemory();
$xw->setIndent(true);
$xw->startDocument();
$xw->startElementNS('test', 'test', 'urn:x-test:');
$xw->writeElementNS('test', 'foo', null, '');
$xw->writeElementNS(null, 'bar', 'urn:x-test:', '');
$xw->writeElementNS(null, 'bar', 'urn:x-test:', NULL);
$xw->writeElementNS(null, 'bar', 'urn:x-test:');
$xw->writeElementNS(null, 'bar', '', '');
$xw->endElement();
$xw->endDocument();
print $xw->flush(true);
?>
?>
Warning: XMLWriter::endElement() expects exactly 0 parameters, 1 given in %s on line %d
XML version="1.0"?>
<test>
 <foo/>
 <foo2></foo2>
 <foo3/>
 <bar/>
</test>

XML version="1.0"?>
<test:test xmlns:test="urn:x-test:">
 <test:foo></test:foo>
 <bar xmlns="urn:x-test:"></bar>
 <bar xmlns="urn:x-test:"/>
 <bar xmlns="urn:x-test:"/>
 <bar xmlns=""></bar>
</test:test>
