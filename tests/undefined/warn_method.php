<?php

class Foo
{
    function call_bar() {
        $this->bar();
    }
}

$foo = new Foo;
$foo->call_bar();
