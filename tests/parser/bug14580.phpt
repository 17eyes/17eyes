--TEST--n
Bug #14580 (key() not binary safe)
?>
<?php
	$arr = array (b"foo\0bar" => b"foo\0bar");
	$key = key($arr);
	echo strlen($key), ': ';
	echo urlencode($key), "\n";
?>
?>
7: foo%00bar
