--TEST--
Bug #60192 (SegFault when Collator not constructed properly)
--SKIPIF--
<?php
	if (!extension_loaded('intl')) { die('skip intl extension not available'); }
?>
--FILE--
<?php

class Collator2 extends Collator{
	public function __construct() {
		// ommitting parent::__construct($someLocale);
	}
}

$c = new Collator2();
$c->getLocale(Locale::ACTUAL_LOCALE);
?>

Catchable fatal error: Collator::getLocale(): Object not initialized in %s on line %d
