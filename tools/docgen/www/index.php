<?php
define('Nican', 1);

require __DIR__ . '/Settings.php';
require __DIR__ . '/Source.php';
require __DIR__ . '/Template.php';

$actions = Array (
	'main' => 'Main',
	'file' => 'ShowFile',
	'gethint' => 'ShowOpts',
	'show' => 'ShowInfo'
);

$context['optheader'] = "";

if(!isset($_GET['action']))
	$action = $actions['main'];
else
	$action = isset($actions[$_GET['action']]) ? $actions[$_GET['action']] : $actions['main'];

$templatefunc = $action . "_template";

$action();
$templatefunc();
