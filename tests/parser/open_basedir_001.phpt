--TEST--
openbase_dir runtime tightning
--SKIPIF--
<?php
if (substr(PHP_OS, 0, 3) == 'WIN') {
	die('skip.. only for unix');
}
?>
open_basedir=/usr/local
--FILE--
<?php
var_dump(ini_set("open_basedir", "/usr/local/bin"));
var_dump(ini_get("open_basedir"));
var_dump(ini_set("open_basedir", "/usr"));
var_dump(ini_get("open_basedir"));
?>
?>
string(10) "/usr/local"
string(14) "/usr/local/bin"
bool(false)
string(14) "/usr/local/bin"

