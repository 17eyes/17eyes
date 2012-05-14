--TEST--
php_sapi_name test
--FILE--
<?php

var_dump(php_sapi_name());

?>
%unicode|string%(3) "c%ci"
