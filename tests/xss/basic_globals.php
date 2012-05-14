<?php

    /* sa: XSS */
    echo $GLOBALS['x'];

    /* sa: - no XSS */
    $y = 'test';
    echo $GLOBALS['y'];

?>
