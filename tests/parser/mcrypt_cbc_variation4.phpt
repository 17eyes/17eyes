--TEST--
Test mcrypt_cbc() function : usage variation 
--SKIPIF--
<?php 
if (!extension_loaded("mcrypt")) {
	print "skip - mcrypt extension not loaded"; 
}	 
?>
?>
<?php
/* Prototype  : string mcrypt_cbc(string cipher, string key, string data, int mode, string iv)
 * Description: CBC crypt/decrypt data using key key with cipher cipher starting with iv 
 * Source code: ext/mcrypt/mcrypt.c
 * Alias to functions: 
 */

echo "*** Testing mcrypt_cbc() : usage variation ***\n";

// Define error handler
function test_error_handler($err_no, $err_msg, $filename, $linenum, $vars) {
	if (error_reporting() != 0) {
		// report non-silenced errors
		echo "Error: $err_no - $err_msg, $filename($linenum)\n";
	}
}
set_error_handler('test_error_handler');

// Initialise function arguments not being substituted (if any)
$cipher = MCRYPT_TRIPLEDES;
$key = b'string_val';
$data = b'string_val';
$iv = b'01234567';

//get an unset variable
$unset_var = 10;
unset ($unset_var);

// define some classes
class classWithToString
{
	public function __toString() {
		return "Class A object";
	}
}

class classWithoutToString
{
}

// heredoc string
$heredoc = <<<EOT
hello world
EOT;

// get a resource variable
$fp = fopen(__FILE__, "r");

// add arrays
$index_array = array (1, 2, 3);
$assoc_array = array ('one' => 1, 'two' => 2);

//array of values to iterate over
$inputs = array(

      // float data
      'float 10.5' => 10.5,
      'float -10.5' => -10.5,
      'float 12.3456789000e10' => 12.3456789000e10,
      'float -12.3456789000e10' => -12.3456789000e10,
      'float .5' => .5,

      // array data
      'empty array' => array(),
      'int indexed array' => $index_array,
      'associative array' => $assoc_array,
      'nested arrays' => array('foo', $index_array, $assoc_array),

      // null data
      'uppercase NULL' => NULL,
      'lowercase null' => null,

      // boolean data
      'lowercase true' => true,
      'lowercase false' =>false,
      'uppercase TRUE' =>TRUE,
      'uppercase FALSE' =>FALSE,

      // empty data
      'empty string DQ' => "",
      'empty string SQ' => '',

      // string data
      'string DQ' => "string",
      'string SQ' => 'string',
      'mixed case string' => "sTrInG",
      'heredoc' => $heredoc,

      // object data
      'instance of classWithToString' => new classWithToString(),
      'instance of classWithoutToString' => new classWithoutToString(),

      // undefined data
      'undefined var' => @$undefined_var,

      // unset data
      'unset var' => @$unset_var,
      
      // resource variable
      'resource' => $fp      
);

// loop through each element of the array for mode

foreach($inputs as $valueType =>$value) {
      echo "\n--$valueType--\n";
      var_dump(bin2hex(mcrypt_cbc($cipher, $key, $data, $value, $iv)));
};

fclose($fp);

?>
=?>=
?>
*** Testing mcrypt_cbc() : usage variation ***

--float 10.5--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--float -10.5--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--float 12.3456789000e10--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--float -12.3456789000e10--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--float .5--
string(32) "5f781523f696d596e4b809d72197a0cc"

--empty array--
string(32) "5f781523f696d596e4b809d72197a0cc"

--int indexed array--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--associative array--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--nested arrays--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--uppercase NULL--
string(32) "5f781523f696d596e4b809d72197a0cc"

--lowercase null--
string(32) "5f781523f696d596e4b809d72197a0cc"

--lowercase true--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--lowercase false--
string(32) "5f781523f696d596e4b809d72197a0cc"

--uppercase TRUE--
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--uppercase FALSE--
string(32) "5f781523f696d596e4b809d72197a0cc"

--empty string DQ--
string(32) "5f781523f696d596e4b809d72197a0cc"

--empty string SQ--
string(32) "5f781523f696d596e4b809d72197a0cc"

--string DQ--
string(32) "5f781523f696d596e4b809d72197a0cc"

--string SQ--
string(32) "5f781523f696d596e4b809d72197a0cc"

--mixed case string--
string(32) "5f781523f696d596e4b809d72197a0cc"

--heredoc--
string(32) "5f781523f696d596e4b809d72197a0cc"

--instance of classWithToString--
Error: 8 - Object of class classWithToString could not be converted to int, %s(%d)
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--instance of classWithoutToString--
Error: 8 - Object of class classWithoutToString could not be converted to int, %s(%d)
string(32) "983d5edc5f77fe42e2372a0339dc22b0"

--undefined var--
string(32) "5f781523f696d596e4b809d72197a0cc"

--unset var--
string(32) "5f781523f696d596e4b809d72197a0cc"

--resource--
string(%d) %s
=?>=
