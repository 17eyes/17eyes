--TEST--
SOAP Interop Round4 GroupI XSD 006 (php/wsdl): echoBase64
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
detect_unicode=0
soap.wsdl_cache_enabled=0
?>
<?php
$client = new SoapClient(dirname(__FILE__)."/round4_groupI_xsd.wsdl",array("trace"=>1,"exceptions"=>0));
$client->echoBase64(array("inputBase64"=>" �� HELLO "));
echo $client->__getlastrequest();
$HTTP_RAW_POST_DATA = $client->__getlastrequest();
include("round4_groupI_xsd.inc");
echo "ok\n";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/"><SOAP-ENV:Body><ns1:echoBase64><ns1:inputBase64>AID/AEhFTExPAH8=</ns1:inputBase64></ns1:echoBase64></SOAP-ENV:Body></SOAP-ENV:Envelope>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/"><SOAP-ENV:Body><ns1:echoBase64Response><ns1:return>AID/AEhFTExPAH8=</ns1:return></ns1:echoBase64Response></SOAP-ENV:Body></SOAP-ENV:Envelope>
ok
