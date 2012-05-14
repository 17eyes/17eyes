<?php
	// Return the salt associated to a username
	function getStoredSalt($username) {
		// always null here because no database for this test case
		return null;
	}

	// Store the salt in the database
	function storeSalt($username, $salt) {
		// always null here because no database for this test case
		return null;
	}

	// My hash function: sha1 + salt
	function my_hash($string, $username = null) {
		$salt = getStoredSalt($username);
		if (!$salt) {
			// generate the salt and store it in the database
			$salt = substr(md5(uniqid(rand(),true)),0,24);
			storeSalt($username, $salt);
		}
		return sha1($salt . $string);
	}

	if (isset($_POST['username']) && isset($_POST['password']))
	{
		$username = htmlentities($_POST['username']);
		$password = my_hash($_POST['password'], $username);
		// write  the cookie
		setcookie("username", $username);
		setcookie("password", $password);
	}
?>
<html>
<head>
<title>Weak Hash Function</title>
</head>
<body>
<h1>Weak Hash Function</h1>
<form action="" method="post">
	<label for="username">Username:</label> <input type="text" name="username" value="<?php echo $_COOKIE['username']; ?>" /><br />
	<label for="password">Password:</label> <input type="password" name="password" /><br />
	<input type="submit"/>
</form>
</body>
</html>