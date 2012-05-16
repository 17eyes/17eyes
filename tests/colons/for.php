<?php for($i = 0; $i < 10; $i++): ?>
foo
<?php endfor; ?>
<?php 
/*a*/for/*a1*/($i = 0; $i < 10; $i++)/*a2*/:/*b*/
    /*c*/echo 'bar';/*d*/
    /*e*/echo 'bar';/*f*/
/*g*/endfor/*h1*/;/*h2*/
