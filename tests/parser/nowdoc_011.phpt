--TEST--
Nowdocs CAN be used as static scalars.
?>
<?php

require_once 'nowdoc.inc';

class e {
    
    const E = <<<'THISMUSTNOTERROR'
If you DON'T see this, something's wrong.
THISMUSTNOTERROR;

};

print e::E . "\n";

?>
?>
If you DON'T see this, something's wrong.
