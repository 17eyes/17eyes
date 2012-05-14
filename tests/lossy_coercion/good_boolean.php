<?php
$a = 5;

// Here $a is loosely converted to boolean like in warn_boolean.php but this is
// a reasonable case and we shouldn't complain about it.

if ($a) {
    echo "$a is true";
}
