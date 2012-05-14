<?php
	// Write a test case where a cookie contains a hashed value
	
	function my_hash($string) {
		return md5($string);
	}
	
	if (isset($_POST['username']) && isset($_POST['password']))
	{
		$username = htmlentities($_POST['username']);
		$password = my_hash($_POST['password']);
		
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
