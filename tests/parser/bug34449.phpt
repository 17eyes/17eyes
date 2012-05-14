--TEST--
Bug #34449 (ext/soap: XSD_ANYXML functionality not exposed)
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
<?php
class TestSoapClient extends SoapClient {
  function __doRequest($request, $location, $action, $version, $one_way = 0) {
  	echo "$request\n";
  	exit;
  }
}

$my_xml = "<array><item/><item/><item/></array>";
$client = new TestSoapClient(null, array('location' => 'test://', 'uri' => 'test://'));
$client->AnyFunction(new SoapVar($my_xml, XSD_ANYXML));
?>
?>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="test://" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><SOAP-ENV:Body><ns1:AnyFunction><array><item/><item/><item/></array></ns1:AnyFunction></SOAP-ENV:Body></SOAP-ENV:Envelope>
