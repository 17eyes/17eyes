--TEST--
DOMDocument::createEntityReference() should create a new entity reference node
--CREDITS--
Knut Urdalen <knut@php.net>
#PHPTestFest2009 Norway 2009-06-09 \o/
--SKIPIF--
<?php
require_once dirname(__FILE__) .'/skipif.inc';
?>
?>
<?php
$dom = new DOMDocument('1.0');
$ref = $dom->createEntityReference('nbsp');
$dom->appendChild($ref);
echo $dom->saveXML();
?>
?>
XML version="1.0"?>
&nbsp;
