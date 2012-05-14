--TEST--
Testing nowdoc in default value for property
?>
<?php 

class foo {
    public $bar = <<<'EOT'
bar
EOT;
}

print "ok!\n";

?>
?>
ok!
