<?php
$x = 0;

// This will always divide by zero. Include path is a constant expression that
// should be possible to statically determine. Much less obvious than
// warn_include.php.

require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . 'to_include.inc');
