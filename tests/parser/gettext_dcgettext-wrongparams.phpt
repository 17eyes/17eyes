--TEST--
Test if dcgettext() errors when you don't supply the correct params.
--SKIPIF--
<?php
if (!extension_loaded("gettext")) {
    die("skip gettext extension is not loaded.\n");
}
?>
<?php
var_dump(dcgettext('a', 'b'));
?>
Warning: dcgettext() expects exactly 3 parameters, 2 given in %s on line %d
NULL
--CREDITS--
Christian Weiske, cweiske@php.net
PHP Testfest Berlin 2009-05-09
