--TEST--
Bug #60477: Segfault after two multipart/form-data POST requestes
--SKIPIF--
<?php
include "skipif.inc"; 
?>
--FILE--
<?php
include "php_cli_server.inc";
php_cli_server_start('echo done, "\n";', TRUE);

list($host, $port) = explode(':', PHP_CLI_SERVER_ADDRESS);
$port = intval($port)?:80;
$output = '';

$fp = fsockopen($host, $port, $errno, $errstr, 0.5);
if (!$fp) {
  die("connect failed");
}

if(fwrite($fp, <<<HEADER
POST /index.php HTTP/1.1
Host: {$host}
Content-Type: multipart/form-data; boundary=---------123456789
Content-Length: 70

---------123456789
Content-Type: application/x-www-form-urlencoded
a=b
HEADER
)) {
	while (!feof($fp)) {
		$output .= fgets($fp);
	}
}

fclose($fp);

$fp = fsockopen($host, $port, $errno, $errstr, 0.5);
if(fwrite($fp, <<<HEADER
POST /main/no-exists.php HTTP/1.1
Host: {$host}
Content-Type: multipart/form-data; boundary=---------123456789
Content-Length: 70

---------123456789
Content-Type: application/x-www-form-urlencoded
a=b
HEADER
)) {
	while (!feof($fp)) {
		$output .= fgets($fp);
	}
}

echo preg_replace("/<style type=\"text\/css\">(.*?)<\/style>/s", "<style type=\"text/css\">AAA</style>", $output), "\n";
fclose($fp);

?>
--EXPECTF--

HTTP/1.1 200 OK
Host: %s
Connection: close
X-Powered-By: %s
Content-type: %s

done
HTTP/1.1 404 Not Found
Host: %s
Connection: close
Content-Type: %s
Content-Length: %d

<html><head><title>404 Not Found</title><style type="text/css">AAA</style>
</head><body><h1 class="h">Not Found</h1><p>The requested resource /main/no-exists.php was not found on this server.</p></body></html>
