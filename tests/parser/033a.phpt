--TEST--
Phar::chmod
--SKIPIF--
<?php if (!extension_loaded("phar")) die("skip"); ?>
?>
phar.readonly=1
phar.require_hash=0
?>
<?php
$fname = dirname(__FILE__) . '/' . basename(__FILE__, '.php') . '.1.phar.php';
$pname = 'phar://hio';
$file = b'<?php include "' . $pname . '/a.php"; __HALT_COMPILER(); ?>';

$files = array();
$files['a.php']   = '<?php echo "This is a\n"; include "'.$pname.'/b.php"; ?>';
include 'files/phar_test.inc';
try {
	$a = new Phar($fname);
	var_dump($a['a.php']->isExecutable());
	$a['a.php']->chmod(0777);
	var_dump($a['a.php']->isExecutable());
	$a['a.php']->chmod(0666);
	var_dump($a['a.php']->isExecutable());
} catch (Exception $e) {
	echo $e->getMessage() . "\n";
}
?>
=?>=
?>
<?php 
unlink(dirname(__FILE__) . '/' . basename(__FILE__, '.clean.php') . '.1.phar.php');
?>
?>
bool(false)
Cannot modify permissions for file "a.php" in phar "%s033a.1.phar.php", write operations are prohibited
=?>=
