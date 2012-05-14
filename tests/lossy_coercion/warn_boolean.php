<?php

$a = TRUE;
$b = 5;

// $b is converted to boolean; we should warn about it.

if ($a == $b) {
    // Prints: Yes, 1 is equal 5.
    echo "Yes, $a is equal $b.\n";
}
