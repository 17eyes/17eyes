--TEST--
SOAP Interop Round3 GroupD Compound1 003 (php/wsdl): echoDocument
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
soap.wsdl_cache_enabled=0
?>
<?php
$client = new SoapClient(dirname(__FILE__)."/round3_groupD_compound1.wsdl",array("trace"=>1,"exceptions"=>0));
$client->echoDocument((object)array("_"=>"Test Document Here","ID"=>1));
echo $client->__getlastrequest();
$HTTP_RAW_POST_DATA = $client->__getlastrequest();
include("round3_groupD_compound1.inc");
echo "ok\n";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/xsd"><SOAP-ENV:Body><ns1:x_Document ID="1">Test Document Here</ns1:x_Document></SOAP-ENV:Body></SOAP-ENV:Envelope>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/xsd"><SOAP-ENV:Body><ns1:result_Document ID="1">Test Document Here</ns1:result_Document></SOAP-ENV:Body></SOAP-ENV:Envelope>
ok
