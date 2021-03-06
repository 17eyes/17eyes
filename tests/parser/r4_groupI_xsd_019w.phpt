--TEST--
SOAP Interop Round4 GroupI XSD 019 (php/wsdl): echoComplexTypeAsSimpleTypes
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
precision=14
soap.wsdl_cache_enabled=0
?>
<?php
class SOAPComplexType {
    function SOAPComplexType($s, $i, $f) {
        $this->varString = $s;
        $this->varInt = $i;
        $this->varFloat = $f;
    }
}
$struct = new SOAPComplexType('arg',34,325.325);
$client = new SoapClient(dirname(__FILE__)."/round4_groupI_xsd.wsdl",array("trace"=>1,"exceptions"=>0));
$client->echoComplexTypeAsSimpleTypes(array("inputComplexType"=>$struct));
echo $client->__getlastrequest();
$HTTP_RAW_POST_DATA = $client->__getlastrequest();
include("round4_groupI_xsd.inc");
echo "ok\n";
?>
?>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/xsd" xmlns:ns2="http://soapinterop.org/"><SOAP-ENV:Body><ns2:echoComplexTypeAsSimpleTypes><ns2:inputComplexType><ns1:varInt>34</ns1:varInt><ns1:varString>arg</ns1:varString><ns1:varFloat>325.325</ns1:varFloat></ns2:inputComplexType></ns2:echoComplexTypeAsSimpleTypes></SOAP-ENV:Body></SOAP-ENV:Envelope>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://soapinterop.org/"><SOAP-ENV:Body><ns1:echoComplexTypeAsSimpleTypesResponse><ns1:outputString>arg</ns1:outputString><ns1:outputInteger>34</ns1:outputInteger><ns1:outputFloat>325.325</ns1:outputFloat></ns1:echoComplexTypeAsSimpleTypesResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>
ok
