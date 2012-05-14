<?
	echo "Executable code<br />";
	echo "<br />";
	for ($i=0;$i<10;++$i)
		echo "Delete file_{$i}<br />";

	echo "<br />I can also execute system command...<br /><br />";

	echo "<pre>";
	system("ls --l");
	echo "</pre>";

	echo "<br />Wanna get the control ?<br /><br />";

	// write some bad script 'h4ck3rs.py'
	system("python h4ck3rs.py");
?>