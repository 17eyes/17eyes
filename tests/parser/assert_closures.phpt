--TEST--
assert() - basic - accept closures as callback.
?>
assert.active = 1
assert.warning = 1
assert.bail = 0
assert.quiet_eval = 0
?>
<?php
assert_options(ASSERT_CALLBACK, function () { echo "Hello World!\n"; });
assert(0);
?>
?>
Hello World!

Warning: assert(): Assertion failed in %s on line %d
