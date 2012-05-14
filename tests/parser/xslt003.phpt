--TEST--
Test 3: Using Parameters
--SKIPIF--
<?php require_once dirname(__FILE__) .'/skipif.inc'; ?>
?>
<?php
echo "Test 3: Using Parameters";
include("prepare.inc");
$proc->importStylesheet($xsl);
$proc->setParameter( "", "foo","hello world");
print "\n";
print $proc->transformToXml($dom);
print "\n";


?>
Test 3: Using Parameters
XML version="1.0" encoding="iso-8859-1"?>
<html><body>hello world
a1 b1 c1 <br/> 
a2 c2 <br/> 
ä3 b3 c3 <br/> 
</body></html>
