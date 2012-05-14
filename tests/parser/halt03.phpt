--TEST--
__HALT_COMPILER() basic test
--FILE--
<?php

if (true) {
	__HALT_COMPILER();
}
?>
Fatal error: __HALT_COMPILER() can only be used from the outermost scope in %shalt03.php on line %d
