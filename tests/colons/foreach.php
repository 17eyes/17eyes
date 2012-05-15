<?php
$a = array(1,2,3,4,5);
foreach($a as &$x): ?>
foo
<?php endforeach; ?>
<?php foreach($a as &$x):
    echo $x;
endforeach;
