--TEST--
SOAP Interop Round2 base 020 (php/wsdl): echoDate
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
soap.wsdl_cache_enabled=0
?>
<?php
$client = new SoapClient(dirname(__FILE__)."/round2_base.wsdl",array("trace"=>1,"exceptions"=>0));
$client->echoDate('2001-05-24T17:31:41Z');
echo $client->__getlastrequest();
$HTTP_RAW_POST_DATA = $client->__getlastrequest();
include("round2_base.inc");
echo "ok\n";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><SOAP-ENV:Body><ns1:echoDate><inputDate xsi:type="xsd:dateTime">2001-05-24T17:31:41Z</inputDate></ns1:echoDate></SOAP-ENV:Body></SOAP-ENV:Envelope>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><SOAP-ENV:Body><ns1:echoDateResponse><outputDate xsi:type="xsd:dateTime">2001-05-24T17:31:41Z</outputDate></ns1:echoDateResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>
ok
