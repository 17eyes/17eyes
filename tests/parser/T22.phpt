--TEST--
SOAP 1.2: T22 echoOk
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php
$HTTP_RAW_POST_DATA = <<<EOF
XML version='1.0' ?>
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope"> 
  <env:Header>
    <test:echoOk xmlns:test="http://example.org/ts-tests" 
          env:mustUnderstand = "1">foo</test:echoOk>
  </env:Header>
  <env:Body>
    <test:echoOk xmlns:test="http://example.org/ts-tests">foo</test:echoOk>
  </env:Body>
</env:Envelope>
EOF;
include "soap12-test.inc";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope" xmlns:ns1="http://example.org/ts-tests"><env:Header><ns1:responseOk>foo</ns1:responseOk></env:Header><env:Body><ns1:responseOk>foo</ns1:responseOk></env:Body></env:Envelope>
ok
