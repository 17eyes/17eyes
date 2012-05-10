<?php

// Here the conversion (int)("2") is implicit but without information loss.
// Maybe we shouldn't complain in such cases.
echo "2" * "2";
