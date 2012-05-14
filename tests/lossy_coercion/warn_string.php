<?php
$foo = "7 days";
$result = str_split(2, $foo);

// BUG: Should be str_split($foo, 2)
// Here "7 days" is casted to int (becomes 7) and 2 is casted to string. So
// this is equivalent to str_split("2", 7) which is not what we want.
//
// We should detect that (int)("7 days") is lossy.

print_r($result);
