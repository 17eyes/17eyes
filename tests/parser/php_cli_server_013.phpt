--TEST--
No router, no script
--SKIPIF--
<?php
include "skipif.inc"; 
?>
--FILE--
<?php
include "php_cli_server.inc";
php_cli_server_start(NULL, TRUE);

list($host, $port) = explode(':', PHP_CLI_SERVER_ADDRESS);
$port = intval($port)?:80;
$output = '';

$fp = fsockopen($host, $port, $errno, $errstr, 0.5);
if (!$fp) {
  die("connect failed");
}


if(fwrite($fp, <<<HEADER
POST / HTTP/1.1
Host: {$host}
Content-Type: application/x-www-form-urlencoded
Content-Length: 3

a=b
HEADER
)) {
	while (!feof($fp)) {
		$output .= fgets($fp);
	}
}

echo preg_replace("/<style type=\"text\/css\">(.*?)<\/style>/s", "<style type=\"text/css\">AAA</style>", $output), "\n";
fclose($fp);


$output = '';
$fp = fsockopen($host, $port, $errno, $errstr, 0.5);
if (!$fp) {
  die("connect failed");
}

if(fwrite($fp, <<<HEADER
GET /main/style.css HTTP/1.1
Host: {$host}


HEADER
)) {
	while (!feof($fp)) {
		$output .= fgets($fp);
	}
}

echo preg_replace("/<style type=\"text\/css\">(.*?)<\/style>/s", "<style type=\"text/css\">AAA</style>", $output), "\n";
fclose($fp);

$output = '';
$fp = fsockopen($host, $port, $errno, $errstr, 0.5);
if (!$fp) {
  die("connect failed");
}

if(fwrite($fp, <<<HEADER
HEAD /main/foo/bar HTTP/1.1
Host: {$host}


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

HTTP/1.1 404 Not Found
Host: %s
Connection: close
Content-Type: text/html; charset=UTF-8
Content-Length: %d

<html><head><title>404 Not Found</title><style type="text/css">AAA</style>
</head><body><h1 class="h">Not Found</h1><p>The requested resource / was not found on this server.</p></body></html>
HTTP/1.1 404 Not Found
Host: %s
Connection: close
Content-Type: text/html; charset=UTF-8
Content-Length: %d

<html><head><title>404 Not Found</title><style type="text/css">AAA</style>
</head><body><h1 class="h">Not Found</h1><p>The requested resource /main/style.css was not found on this server.</p></body></html>
HTTP/1.1 404 Not Found
Host: %s
Connection: close
Content-Type: text/html; charset=UTF-8
Content-Length: %d

<html><head><title>404 Not Found</title><style type="text/css">AAA</style>
</head><body><h1 class="h">Not Found</h1><p>The requested resource /main/foo/bar was not found on this server.</p></body></html>

