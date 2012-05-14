--TEST--
DOM cloneNode : Basic Functionality
--SKIPIF--
<?php
require_once('skipif.inc');
?>
--CREDITS--
Simon Hughes <odbc3@hotmail.com>
--FILE--
<?php

$xml = <<< EOXML
<?xml version="1.0" encoding="ISO-8859-1"?>
<courses>
	<course title="one">
		<notes>
			<note>c1n1</note>
			<note>c1n2</note>
		</notes>
	</course>
	<course title="two">
		<notes>
			<note>c2n1</note>
			<note>c2n2</note>
		</notes>
	</course>
</courses>
EOXML;

function dumpcourse($current) {
	$title = ($current->nodeType != XML_TEXT_NODE && $current->hasAttribute('title')) ? $current->getAttribute('title'):"no title"; 
	echo "Course: $title:";var_dump($current);
	echo "~";var_dump($current->textContent);
}

$dom = new DOMDocument();
$dom->loadXML($xml);
$root = $dom->documentElement;

// strip all text nodes from this tree
$children = $root->childNodes;
$len = $children->length;
for ($index = $children->length - 1; $index >=0; $index--) {
	$current = $children->item($index);
	if ($current->nodeType == XML_TEXT_NODE) {
		$noderemoved = $root->removeChild($current);
	}
}

echo "Start cloneNode test\n";
$first_course = $children->item(0);
$cloned_first_course_default = $first_course->cloneNode();
$first_course->setAttribute('title', 'new title1');

$cloned_first_course_true = $first_course->cloneNode(true);
$first_course->setAttribute('title', 'new title2');

$cloned_first_course_false = $first_course->cloneNode(false);
$first_course->setAttribute('title', 'new title3');

$cloned_first_course_default->setAttribute('title', 'new title default');
$cloned_first_course_true->setAttribute('title', 'new title true');
$cloned_first_course_false->setAttribute('title', 'new title false');

$root->appendChild($cloned_first_course_default);
$root->appendChild($cloned_first_course_true);
$root->appendChild($cloned_first_course_false);

$children = $root->childNodes;
for ($index = 0; $index < $children->length; $index++) {
	echo "node $index\n";
	dumpcourse($children->item($index));
}

?>
Start cloneNode test
node 0
Course: new title3:object(DOMElement)#6 (0) {
}
~string(24) "
		
			c1n1
			c1n2
		
	"
node 1
Course: two:object(DOMElement)#3 (0) {
}
~string(24) "
		
			c2n1
			c2n2
		
	"
node 2
Course: new title default:object(DOMElement)#4 (0) {
}
~string(0) ""
node 3
Course: new title true:object(DOMElement)#7 (0) {
}
~string(24) "
		
			c1n1
			c1n2
		
	"
node 4
Course: new title false:object(DOMElement)#8 (0) {
}
~string(0) ""
