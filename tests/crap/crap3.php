<?php

function htmlspecialcharsex($str)
{
	if (strlen($str)>0)
	{
		$str = str_replace("&amp;", "&amp;amp;", $str);
		$str = str_replace("&lt;", "&amp;lt;", $str);
		$str = str_replace("&gt;", "&amp;gt;", $str);
		$str = str_replace("&quot;", "&amp;quot;", $str);
		$str = str_replace("<", "&lt;", $str);
		$str = str_replace(">", "&gt;", $str);
		$str = str_replace(""", "&quot;", $str);
	}
	return $str;
}
