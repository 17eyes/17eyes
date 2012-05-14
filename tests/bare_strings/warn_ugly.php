<?php
$p = array("name" => "Roman", "surname" => "Maliniak");

// On some control flow paths constant "name" is defined.
if (rand() % 4 > 0) {
    define("name", "surname");
}

// Sometimes emits a warning, sometimes doesn't. Statically we could detect it
// every time.
echo "name: " . $p[name];
