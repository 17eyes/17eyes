--TEST--
Closure 010: Closure calls itself
?>
<?php
$i = 3;
$lambda = function ($lambda) use (&$i) {
    if ($i==0) return;
    echo $i--."\n";
    $lambda($lambda);
};
$lambda($lambda);
echo "$i\n";
?>
?>
3
2
1
0
