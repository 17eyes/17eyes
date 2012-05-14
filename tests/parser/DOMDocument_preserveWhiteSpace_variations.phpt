--TEST--
DOMDocument::$preserveWhiteSpace - test ability to read and write property
--CREDITS--
Lev Radin <prokurator@gmail.com>
# TestFest 2009 NYPHP
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php

echo "Load document with preserveWhiteSpace on\n";
$doc = new DOMDocument;
$doc->load(dirname(__FILE__)."/book.xml");
echo $doc->saveXML();


echo "\nLoad document with preserveWhiteSpace off\n";
$doc = new DOMDocument;
$doc->preserveWhiteSpace = false;
$doc->load(dirname(__FILE__)."/book.xml");
echo $doc->saveXML();

?>
?>
Load document with preserveWhiteSpace on
XML version="1.0"?>
<books>
 <book>
  <title>The Grapes of Wrath</title>
  <author>John Steinbeck</author>
 </book>
 <book>
  <title>The Pearl</title>
  <author>John Steinbeck</author>
 </book>
</books>

Load document with preserveWhiteSpace off
XML version="1.0"?>
<books><book><title>The Grapes of Wrath</title><author>John Steinbeck</author></book><book><title>The Pearl</title><author>John Steinbeck</author></book></books>
