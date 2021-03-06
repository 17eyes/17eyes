--TEST--
Bug #42086 (SoapServer return Procedure '' not present for WSIBasic compliant wsdl)
--SKIPIF--
<?php require_once('skipif.inc'); ?>
?>
soap.wsdl_cache_enabled=0
?>
<?php
$request = <<<EOF
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"><SOAP-ENV:Body><firstFunctionWithoutParam/></SOAP-ENV:Body></SOAP-ENV:Envelope>
EOF;

class firstFunctionWithoutParamResponse {
	public $param;
}

function firstFunctionWithoutParam() {
	$ret = new firstFunctionWithoutParamResponse();
	$ret->param	=	"firstFunctionWithoutParam";
	return $ret;
}
	
$server = new SoapServer(dirname(__FILE__).'/bug42086.wsdl',
	array('features'=>SOAP_SINGLE_ELEMENT_ARRAYS));
$server->addFunction('firstFunctionWithoutParam');
$server->handle($request);
?>
?>
XML version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"><SOAP-ENV:Body><firstFunctionWithoutParamReturn><param>firstFunctionWithoutParam</param></firstFunctionWithoutParamReturn></SOAP-ENV:Body></SOAP-ENV:Envelope>
