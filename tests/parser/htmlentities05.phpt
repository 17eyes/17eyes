--TEST--
htmlentities() test 5 (mbstring / cp1252)
?>
output_handler=
mbstring.internal_encoding=cp1252
--SKIPIF--
<?php
	extension_loaded("mbstring") or die("skip mbstring not available\n");
?>
<?php
	print mb_internal_encoding()."\n";
	var_dump(htmlentities("\x82\x86\x99\x9f", ENT_QUOTES, ''));
	var_dump(htmlentities("\x80\xa2\xa3\xa4\xa5", ENT_QUOTES, ''));
?>
?>
Windows-1252
string(28) "&sbquo;&dagger;&trade;&Yuml;"
string(32) "&euro;&cent;&pound;&curren;&yen;"
