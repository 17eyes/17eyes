--TEST--
Bug #48023 (spl_autoload_register didn't addref closures)
?>
<?php
spl_autoload_register(function(){});

new Foo;

?>
=?>=
?>
Fatal error: Class 'Foo' not found in %s on line %d
