<?php
	/*
		[POST] q = Marcel Proust' OR 1=1--
		[POST] i = 1 OR 1=1--
	*/
	function typecast($string, $type)
	{
		switch($type)
		{
			case 'integer': return (int)($string);
			case 'string' : return $string;
			default:        return $string;
		}
	}

	// Print a result of a query
	function print_db($result)
	{
		if (!$result)
			return;
		echo "<table><thead><td>ID</td><td>Name</td><td>Author</td></thead><tbody>";
		while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
		{
			echo "<tr><td>". $row['BookID'] ."</td><td>" .  $row['Name']."</td><td>" .  $row['Author']. "</td></tr>\n";
		}
		echo "</tbody></table>";
	}

	// Start the connection to the database
	$db = mysql_connect('localhost', 'media', 'pass');
	if (!$db) {
		die('Could not connect: ' . mysql_error());
	}
	mysql_select_db("media") or die( "Unable to select database");
?>
<html>
<body>
<h1>SQL Injection</h1>
<form action="" method="post">
	Author: <input name="q" type="text" /><br />
	Book ID: <input name="i" type="text" /><br />
	<input name="Submit" type="submit" />
</form>
<br />
<?php
	if (isset($_POST['q']) && strlen($_POST['q']) > 0)
	{
		$q = typecast($_POST['q'], 'string');
		echo "<h2>Result for Author = [$q]<h2>\n";
		$result = mysql_query("SELECT * FROM books WHERE Author = '$q'");
		print_db($result);
	}
	if (isset($_POST['i'])  && strlen($_POST['i']) > 0)
	{
		$i = typecast($_POST['i'], 'integer');
		echo "<h2>Result for BookID = [$i]<h2>\n";
		$result = mysql_query("SELECT * FROM books WHERE BookID = $i");
		print_db($result);
	}
?>
<h2>All the database</h2>
<?php
	$result = mysql_query("SELECT * FROM books WHERE 1");
	print_db($result);
	mysql_close($db);
?>
</body>
</html>