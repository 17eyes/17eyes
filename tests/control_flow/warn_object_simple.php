<?php

class FooClass {
    function divby($x) {
        echo 5 / $x;
    }
}

$x = new FooClass;
$x->divby(0);
