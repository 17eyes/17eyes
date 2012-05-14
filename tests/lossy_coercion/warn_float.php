<?php

$a = array(
    0   => "zero",
    0.5 => "half",
    1   => "one",
    2   => "two"
);

// Array indices are ints or strings only so $a[0] is "half". Maybe we should
// warn if there is no explicit cast (like (int)(0.5)).
echo $a[0];
