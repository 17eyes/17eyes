<?php

abstract class Foo
{
    function call_hello() {
        $this->hello(); // we shouldn't complain about it in an abstract class
    }
}

class Bar extends Foo
{
    function hello() {
        echo "Hello, world!\n";
    }
}

$x = new Bar;
$x->call_hello(); // when invoked on Bar this is valid
