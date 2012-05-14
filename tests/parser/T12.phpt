--TEST--
SOAP 1.2: T12 unknownHdr
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php
$HTTP_RAW_POST_DATA = <<<EOF
XML version='1.0' ?>
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope"> 
  <env:Header>
    <test:Unknown xmlns:test="http://example.org/ts-tests"
          env:role="http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver"
          env:mustUnderstand="1">foo</test:Unknown>
  </env:Header>
  <env:Body>
  </env:Body>
</env:Envelope>
EOF;
include "soap12-test.inc";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope"><env:Body><env:Fault><env:Code><env:Value>env:MustUnderstand</env:Value></env:Code><env:Reason><env:Text>Header not understood</env:Text></env:Reason></env:Fault></env:Body></env:Envelope>
