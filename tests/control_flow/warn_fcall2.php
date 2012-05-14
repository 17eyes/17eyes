<?php

// Sure division by zero, we should warn in this case.
divby(0);

function divby($x) {
    echo 5 / $x;
}
