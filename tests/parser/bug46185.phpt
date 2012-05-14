--TEST--
Bug #46185 (importNode changes the namespace of an XML element).
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php 
$aDOM = new DOMDocument();
$aDOM->loadXML('XML version="1.0"?>
<ns1:a xmlns:ns1="urn::ns"/>');
$a= $aDOM->firstChild;

$ok = new DOMDocument();
$ok->loadXML('XML version="1.0"?>
<ns1:ok xmlns:ns1="urn::ns" xmlns="urn::REAL"><watch-me xmlns:default="urn::BOGUS"/></ns1:ok>');

$imported= $aDOM->importNode($ok->firstChild, true);
$a->appendChild($imported);

echo $aDOM->saveXML();
?>
?>
XML version="1.0"?>
<ns1:a xmlns:ns1="urn::ns"><ns1:ok xmlns="urn::REAL"><watch-me xmlns:default="urn::BOGUS"/></ns1:ok></ns1:a>
