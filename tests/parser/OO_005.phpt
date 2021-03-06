--TEST--
XMLWriter: libxml2 XML Writer, comments 
--SKIPIF--
<?php 
if (!extension_loaded("xmlwriter")) die("skip"); 
if (!function_exists("xmlwriter_start_comment")) die("skip: libxml2 2.6.7+ required");
?>
?>
<?php 
/* $Id: OO_005.phpt 201860 2005-12-02 02:05:26Z iliaa $ */

$doc_dest = '001.xml';
$xw = new XMLWriter();
$xw->openUri($doc_dest);
$xw->startDocument('1.0', 'UTF-8');
$xw->startElement("tag1");
$xw->startComment();
$xw->text('comment');
$xw->endComment();
$xw->writeComment("comment #2");
$xw->endDocument();

// Force to write and empty the buffer
$output_bytes = $xw->flush(true);
echo file_get_contents($doc_dest);
unset($xw);
unlink('001.xml');
?>
=?>=
?>
XML version="1.0" encoding="UTF-8"?>
<tag1><!--comment--><!--comment #2--></tag1>
=?>=
