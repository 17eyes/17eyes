<?php

// chop() takes at most 2 parameters. This causes a runtime warning in the rare
// case when it actually runs. We should be able to detect that statically.

list($x, $y, $z) = array("x", "y", "z");

if (rand() % 64 === 0) {
    chop($x, $y, $z);
}
