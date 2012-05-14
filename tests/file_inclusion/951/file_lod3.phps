<?php
	function checkFile($fName) {
		if (strstr($fName, '\\') >= 0) {
			$fName = str_replace('\\','/', $fName);
		}
		if (!ereg('^[^./][^/]*$', $fName))
			return 'error.inc';
		$DOCUMENT_ROOT = $_SERVER['DOCUMENT_ROOT'];
		$apacheres = apache_lookup_uri($fName);
		$really = realpath($apacheres->filename);
		if(substr($really, 0, strlen($DOCUMENT_ROOT)) == $DOCUMENT_ROOT) {
			if(is_file($really))
				return $really;
		}
		return 'error.inc';
	}
?>
<html>
<head>
<title>Remote File Inclusion</title>
</head>
<body>
	<h1>Remote File Inclusion</h1>
	This is a link for the scanners: <a href="?q=file">here</a><br />
	<h1>Content:</h1>
<?php
	if (isset($_GET['q']))
	{
		$fName =  $_GET['q'] . '.inc';
		include (checkFile($fName));
	}
	else
		echo "Default content...";
?>
</body>
</html>
