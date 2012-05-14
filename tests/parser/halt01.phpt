--TEST--
__HALT_COMPILER() basic test
?>
<?php

print "yo!\n";

__HALT_COMPILER();

none of this should be displayed!
?>
yo!
