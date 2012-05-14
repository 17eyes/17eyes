--TEST--
Bug #50680 ("eighth" quantifier is not understood)
--INI--
date.timezone=UTC
--FILE--
<?php
var_dump(date("d", strtotime("March 1 eighth day 2009")));
?>
string(2) "09"
