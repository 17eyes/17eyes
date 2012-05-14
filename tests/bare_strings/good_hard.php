<?php

function abababab() {
    define('foo', 5);
}

$f = "ab";
$f = $f . $f;
$f = $f . $f;
$f();

// This is why it's hard. Here we should *not* emit a warning, allthough maybe
// a false positive would be acceptable...

echo foo;
