--TEST--
Bug #49244 (Floating point NaN cause garbage characters)
?>
<?php

for ($i = 0; $i < 10; $i++) {
	printf("{%f} %1\$f\n", pow(-1.0, 0.3));
	printf(b"{%f} %1\$f\n", pow(-1.0, 0.3));
}

?>
?>
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
{NaN} NaN
