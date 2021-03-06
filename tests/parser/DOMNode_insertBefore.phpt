--TEST--
Tests DOMNode::insertBefore()
--CREDITS--
Michael Stillwell <mjs@beebo.org>
# TestFest 2008
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php

$dom = new DOMDocument();
$dom->loadXML('<root/>');
echo $dom->saveXML();

$e1 = $dom->createElement("A");
$e2 = $dom->documentElement->appendChild($dom->createElement("B"));

echo "Add new node B\n";
echo $dom->saveXML();

echo "Add new node A before B\n";
$e2->parentNode->insertBefore($e1, $e2);
echo $dom->saveXML();

?>
?>
XML version="1.0"?>
<root/>
Add new node B
XML version="1.0"?>
<root><B/></root>
Add new node A before B
XML version="1.0"?>
<root><A/><B/></root>
