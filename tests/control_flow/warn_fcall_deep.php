<?php

if (rand() % 2) {
    function divby($x) {
        echo 5 / $x;
    }
} else {
    function divby($x) {
        echo 3 / $x;
    }
}

// This will always cause division by zero but detecting this is a little bit
// more complicated.

divby(0);
