--TEST--
Test 6: Transform To Doc
--SKIPIF--
<?php require_once dirname(__FILE__) .'/skipif.inc'; ?>
?>
<?php
echo "Test 6: Transform To Doc";
include("prepare.inc");
$proc->importStylesheet($xsl);
print "\n";
$doc = $proc->transformToDoc($dom);
print $doc->saveXML();
print "\n";


?>
Test 6: Transform To Doc
XML version="1.0" encoding="iso-8859-1"?>
<html><body>bar
a1 b1 c1 <br/> 
a2 c2 <br/> 
�3 b3 c3 <br/> 
</body></html>
