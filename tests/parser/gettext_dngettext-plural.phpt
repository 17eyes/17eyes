--TEST--
Test if dngettext() returns the correct translations (optionally plural).
--SKIPIF--
<?php
if (!extension_loaded("gettext")) {
    die("skip gettext extension is not loaded.\n");
}
if (!setlocale(LC_ALL, 'en_US.UTF-8')) {
    die("skip en_US.UTF-8 locale not supported.");
}
?>
<?php
chdir(dirname(__FILE__));
setlocale(LC_ALL, 'en_US.UTF-8');
bindtextdomain('dngettextTest', './locale');

var_dump(dngettext('dngettextTest', 'item', 'items', 1));
var_dump(dngettext('dngettextTest', 'item', 'items', 2));
?>
string(7) "Produkt"
string(8) "Produkte"
--CREDITS--
Till Klampaeckel, till@php.net
PHP Testfest Berlin 2009-05-09
