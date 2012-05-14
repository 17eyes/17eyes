<?php

$id = 0;
while (!$id || mysql_error()) {
        $id = rand(1, 10000000);
            mysql_query("INSERT INTO `table` (id) VALUES ('".$id."'");
}
