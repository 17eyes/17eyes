--TEST--
Test function readgzfile() reading a plain relative file
--SKIPIF--
<?php
if (!extension_loaded('zlib')) die ('skip zlib extension not available in this build');
?>
?>
<?php
$plaintxt = b<<<EOT
hello world
is a very common test
for all languages

EOT;
$dirname = 'readgzfile_temp';
$filename = $dirname.'/plainfile.txt';
mkdir($dirname);
$h = fopen($filename, 'w');
fwrite($h, $plaintxt);
fclose($h);


var_dump(readgzfile( $filename ) );

unlink($filename);
rmdir($dirname);
?>
=?>=
?>
hello world
is a very common test
for all languages
int(52)
=?>=
