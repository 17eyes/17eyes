--TEST--
empty doc test (nowdoc)
?>
<?php

require_once 'nowdoc.inc';

print <<<'ENDOFNOWDOC'
ENDOFNOWDOC;

$x = <<<'ENDOFNOWDOC'
ENDOFNOWDOC;

print "{$x}";

?>
?>
