<?php
if (!defined('Nican'))
	die('Hacking attempt...');

$dbname = '';
$dbuser = '';
$dbpass = '';
$dbhost = '';
	
($link = mysql_connect($dbhost, $dbuser, $dbpass)) or die ("could not connect");
mysql_select_db($dbname) or die('Could not select database');
