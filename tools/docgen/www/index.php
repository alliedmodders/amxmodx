<?php
define('Nican', 1);

include_once('Settings.php');

include_once('Login.php');
include_once('Source.php');
include_once('bbc_parse.php');
include_once('template.php');

$resul =db_query('SELECT * FROM `sm_sminfo`',__FILE__,__LINE__);
while ($line = mysql_fetch_array($resul, MYSQL_NUM)) {
	$context['globalinfo'][$line[0]] = Array ($line[1] , $line[2]);
}

//Check if the user is logged in
CheckLogin();

$context['sm']['haslibary'] = true;

$actions = Array (
	'main' => 'Main',
	'file' => 'ShowFile',
	'gethint' => 'ShowOpts',
	'show' => 'ShowInfo',
	'codehigh' => 'HighLight',
	'previewpost' => 'PreviewPost',
	'post' => 'PostThis',
	'login' => 'LoginWebiste',
	'logout' => 'LogOutWebsite',
);

$context['optheader'] = "";
$context['usetopandbo'] = Array ( true,  true );
$context['topmenu'] = Array();

if(!isset($_GET['action']))
	$action = $actions['main'];
else
	$action = isset($actions[$_GET['action']]) ? $actions[$_GET['action']] : $actions['main'];

$templatefunc = $action . "_template";

$action();


// Check if compressed output is enabled, supported, and not already being done.
if (!headers_sent() && ob_get_length() == 0){
	// If zlib is being used, turn off output compression.
	if (@ini_get('zlib.output_compression') != '1' && @ini_get('output_handler') != 'ob_gzhandler' && @version_compare(PHP_VERSION, '4.2.0') != -1)
		ob_start('ob_gzhandler');
}
if($context['usetopandbo'][0]) Headertemplate();
$templatefunc();
if($context['usetopandbo'][1]) Footertemplate();

//print_r($context);
?>
