<?php
if (!defined('Nican'))
	die('Hacking attempt...');
	
$context['user']['guest'] = true;
$context['user']['id'] = 0;
$context['user']['ip'] =  getenv("REMOTE_ADDR");
	
//Check if the user is logged in
function CheckLogin(){
	global $context, $dbname, $cookiename;
 
	//First check for the cookie existence
	if (!isset($_COOKIE[$cookiename]))
		return;
	
	//Uncompile the data
	//0 = id, 1 = md5(password . ip)
	$array = @unserialize($_COOKIE[$cookiename]);
	
	if($array === FALSE)
		return;
		
	$array[0] = (int) $array[0];
		
	if($array[0] == 0)
		return;
		
	//Go to BAILOPAN Database to steal some data
	mysql_select_db('am');
	
	$query = db_query('SELECT username, password FROM vb_user WHERE userid = '. $array[0] .'');	
	
	if(mysql_num_rows($query) == 0){
		mysql_free_result($query);
		mysql_select_db($dbname);
		return;
	}
	
	//Sinse userid is PRIMARY, it should only have 1 result, no need for WHILE
	$info = mysql_fetch_array($query, MYSQL_ASSOC);
	
	//print_r($array);
	//print_r($info);
	
	//Omg! it is him! Let him in
	if( md5($info['password'] . $context['user']['ip'] ) == $array[1]){
		$context['user']['guest'] = false;
		$context['user']['id'] = $array[0];
		$context['user']['name'] = $info['username'];		
	}
	
	mysql_free_result($query);
	mysql_select_db($dbname);
}

function LoginUser(){
	global $context, $dbname, $cookieaddr, $cookiename;
	
	if(!isset($_REQUEST['pw']) || !isset($_REQUEST['user']))
		return 'Information not entered.';
	else
	
		
	//'Pw' should be in MD5 and the user column can't be bigger then 100
	if(strlen($_REQUEST['pw']) != 32 || strlen($_REQUEST['user']) > 100)
		return 'Invalid user or password.';
		
	mysql_select_db('am');
	
	$query = db_query('SELECT userid, password, salt FROM vb_user WHERE username = "'. mysql_real_escape_string($_REQUEST['user']) .'"');
	
	if(mysql_num_rows($query) == 0){
		mysql_free_result($query);
		mysql_select_db($dbname);
		return 'Invalid user or password.';
	}
	
	$info = mysql_fetch_array($query, MYSQL_ASSOC);
	
	$pw = md5($_REQUEST['pw'] . $info['salt'] );
	
	//echo $pw . '<br>';
	
	if($pw != $info['password']){
		mysql_free_result($query);
		mysql_select_db($dbname);
		return 'Invalid user or password.';
	}
	
	if(isset($_REQUEST['forever']) && $_REQUEST['forever'] == 1)
		$time = 0;
	else
		$time = time() + 3600 * 24;
	
	
	$data = serialize( Array($info['userid'], md5($pw . $context['user']['ip'])) );
	
	//print_r($data);
	
	//Yay, by now he has login and password correct, let's give him a cookie
	setcookie($cookiename, $data, $time, '/', $cookieaddr, 0 ,1);	
	
	mysql_free_result($query);
	mysql_select_db($dbname);
	
	return 'ok';
}



?>
