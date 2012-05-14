--TEST--
SOAP 1.2: T77.1 isNil
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php
$HTTP_RAW_POST_DATA = <<<EOF
XML version='1.0' ?>
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope" 
              xmlns:xsd="http://www.w3.org/2001/XMLSchema"
              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <env:Body>
    <test:isNil xmlns:test="http://example.org/ts-tests"
          env:encodingStyle="http://www.w3.org/2003/05/soap-encoding">
      <inputString xsi:nil="1" />
    </test:isNil>
  </env:Body>
</env:Envelope>
EOF;
include "soap12-test.inc";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope" xmlns:ns1="http://example.org/ts-tests" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:enc="http://www.w3.org/2003/05/soap-encoding"><env:Body xmlns:rpc="http://www.w3.org/2003/05/soap-rpc"><ns1:isNilResponse env:encodingStyle="http://www.w3.org/2003/05/soap-encoding"><rpc:result>return</rpc:result><return xsi:type="xsd:boolean">true</return></ns1:isNilResponse></env:Body></env:Envelope>
ok
