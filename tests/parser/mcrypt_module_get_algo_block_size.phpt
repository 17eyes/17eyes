--TEST--
mcrypt_module_get_algo_block_size
--SKIPIF--
<?php if (!extension_loaded("mcrypt")) print "skip"; ?>
--FILE--
<?php
var_dump(mcrypt_module_get_algo_block_size(MCRYPT_RIJNDAEL_256));
var_dump(mcrypt_module_get_algo_block_size(MCRYPT_RIJNDAEL_192));
var_dump(mcrypt_module_get_algo_block_size(MCRYPT_RC2));
var_dump(mcrypt_module_get_algo_block_size(MCRYPT_XTEA));
var_dump(mcrypt_module_get_algo_block_size(MCRYPT_CAST_128));
var_dump(mcrypt_module_get_algo_block_size(MCRYPT_BLOWFISH));
?>
int(32)
int(24)
int(8)
int(8)
int(8)
int(8)
