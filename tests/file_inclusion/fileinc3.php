<?php

 $array = array('allowed.php');

 if (in_array($_GET['x'], $array)) {
         include($_GET['x']);
 }
