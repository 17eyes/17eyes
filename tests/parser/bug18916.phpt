--TEST--
Bug #18916 (xmlrpc_set_type() not working)
--SKIPIF--
<?php if (!extension_loaded("xmlrpc")) print "skip"; ?>
?>
date.timezone="America/Sao_Paulo"
?>
<?php

$params = date("Ymd\TH:i:s", time());
xmlrpc_set_type($params, 'datetime');
echo xmlrpc_encode($params);

?>
?>
XML version="1.0" encoding="utf-8"?>
<params>
<param>
 <value>
  <dateTime.iso8601>%dT%d:%d:%d</dateTime.iso8601>
 </value>
</param>
</params>
