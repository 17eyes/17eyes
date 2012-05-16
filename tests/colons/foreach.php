<?php
$a = array(1,2,3,4,5);
foreach($a as &$x): ?>
foo
<?php endforeach; ?>
<?php/*a*/foreach/*a1*/($a as &$x)/*a2*/:/*b*/
    /*c*/echo $x;/*d*/
    /*e*/echo $x;/*f*/
/*g*/endforeach;/*h*/
