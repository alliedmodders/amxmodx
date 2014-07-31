<?php
if (!defined('Nican'))
	die('Hacking attempt...');


function db_query($query){
	$q = mysql_query($query);
	if($q === FALSE){
		echo "\n" . mysql_error() . "\n" . $query . "<hr>";
	}
	return $q;
} 

function Main(){
 	global $context;
	$context['optheader'] = "Main";
	
	$resul = db_query('SELECT id,name,fcount,ccount FROM `sm_smfiles`',__FILE__,__LINE__);
	while ($line = mysql_fetch_array($resul, MYSQL_ASSOC)) {
	    $context['fileinfo'][] = Array(
			'id' => $line['id'],
			'name' => $line['name'],
		);
	}
	
	usort($context['fileinfo'], "SortByName");
	
}

function SortByName($a, $b)
{
    if ($a['name'] == $b['name']) {
        return 0;
    }
    return ($a['name'] < $b['name']) ? -1 : 1;
}


function ShowOpts(){
	global $context;
	
	if(!isset($_GET['id']) || $_GET['id'] == "")
		exit("No Results found.");
	
	if(strlen($_GET['id']) > 30)
		exit("No Results found.");
		
	$context['usetopandbo'] = Array ( false,false);
		
	$query = 'SELECT id,func,inc FROM `sm_smfunctions` WHERE LCASE(func) LIKE \'%'.strtolower(mysql_real_escape_string($_GET['id'])).'%\' OR description LIKE \'%'.mysql_real_escape_string($_GET['id']).' %\' COLLATE latin1_swedish_ci';
	$result = db_query($query,__FILE__,__LINE__);
	
	$context['answers'] = Array();
	
	$context['numresults'] = mysql_num_rows($result);
	
	if($context['numresults'] > 100)
		return;
	
	$files = Array ();
	$i = 0;
	
	if($context['numresults'] > 0){
		while ($line = mysql_fetch_array($result, MYSQL_ASSOC)) {
		    $context['answers'][ $line['inc'] ][ $line['id'] ] = $line['func'];
		    
		    $context['lastone'][ $line['inc'] ] = $line['id'];
		    
			if(!isset( $files [ $line['inc'] ])){
				$files [ $line['inc'] ] = $i;
				$i++;
			}
		}
		
		$fliped = array_flip( $files );
		$query =  db_query('SELECT id,name FROM `sm_smfiles` WHERE id IN ('. implode(",",$fliped) .')',__FILE__,__LINE__);
		
		while ($line = mysql_fetch_array($query, MYSQL_ASSOC)) {
		    $context['files'][ $line['id'] ] = $line['name'];
		}
	}
}

function ShowInfo(){
	global $context;
	
	if(!isset($_GET['id']) || $_GET['id'] == "")
		exit("No Results found.");
	
	$query = 'SELECT func,fullfunc,description,treturn,funcinput,exemple,inc,incname,typeof,onerror,version FROM `sm_smfunctions` WHERE id = '.intval($_GET['id']).' LIMIT 1';
	$result = db_query($query,__FILE__,__LINE__);
	
	
	$context['numresults'] = mysql_num_rows($result);
	
	if($context['numresults'] > 0){
		$context['answers'] = mysql_fetch_array($result, MYSQL_ASSOC);
		$context['topmenu'][] = Array (
			$context['answers']['incname'],
			'index.php?action=file&id='.$context['answers']['inc']		
		); 
		$context['optheader'] = $context['answers']['func'];
		
		
		$result = db_query('SELECT fcount,ccount FROM `sm_smfiles` WHERE id = '.$context['answers']['inc'].' LIMIT 1',__FILE__,__LINE__);
		$context['fileinfo'] = mysql_fetch_array($result, MYSQL_ASSOC);
		
		$result = db_query('SELECT time,poster,body FROM `sm_smposts` WHERE file = '.$context['answers']['inc'].' AND func = '.intval($_GET['id']).'',__FILE__,__LINE__);
		$context['sm']['pcount']  = mysql_num_rows($result);
		if($context['sm']['pcount'] > 0){
			$context['sm']['posts'] = Array();
			while ($line = mysql_fetch_array($result, MYSQL_ASSOC)) {
			    $context['sm']['posts'][] = Array (
					'poster' => $line['poster'],
					'time' => date("F j, Y, g:i a",$line['time']),
					'body' => parse_bbc($line['body']),
				);
			}
		}
	}
}

function ShowFile(){
	global $context;
	
	if(!isset($_GET['id']) || $_GET['id'] == "" || strlen($_GET['id']) > 2)
		exit("No Results found.");
	
	$result = db_query('SELECT name,filename,fcount,ccount FROM `sm_smfiles` WHERE id = '.intval($_GET['id']).' LIMIT 1',__FILE__,__LINE__);
	
	$context['letters'] = Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v');
	$context['numresults'] = mysql_num_rows($result);
	
	if($context['numresults'] > 0){
	 	$info = mysql_fetch_array($result, MYSQL_ASSOC);
	 	
	 	$context['name'] = $info['name'];
		$context['filename'] = $info['filename'];
		$context['fcount'] = $info['fcount'];
		$context['ccount'] = $info['ccount'];
		$context['optheader'] = $info['filename'];
		
		$result = db_query('SELECT time,poster,body FROM `sm_smposts` WHERE file = '.intval($_GET['id']).' AND func = -1',__FILE__,__LINE__);
		$context['sm']['pcount']  = mysql_num_rows($result);
		if($context['sm']['pcount'] > 0){
			$context['sm']['posts'] = Array();
			while ($line = mysql_fetch_array($result, MYSQL_ASSOC)) {
			    $context['sm']['posts'][] = Array (
					'poster' => $line['poster'],
					'time' => date("F j, Y, g:i a",$line['time']),
					'body' => parse_bbc($line['body']),
				);
			}
		}
	
		
		$context['topmenu'][] = Array (
			$context['filename'],
			'index.php?action=file&id='.$_GET['id']		
		);
	 
	 	if(isset($_GET['type']) && $info['fcount'] > 0) return LoadThis(0);
	 	if(isset($_GET['file'])) return LoadThis(2);
	 	if($info['ccount'] > 0) return LoadThis(1);
	 	if($info['fcount'] > 0) return LoadThis(0);
	 	return LoadThis(2);	
	}
	
}

function LoadThis($type){
 	global $context;
 	$context['goon'] = $type;
 
	switch($type){
		case 0:
			$resul = db_query('SELECT id,func,description FROM `sm_smfunctions` WHERE inc = '.intval($_GET['id']).'',__FILE__,__LINE__);
			while ($line = mysql_fetch_array($resul, MYSQL_ASSOC)) {
			    $context['infos'][] = Array(
					'id' => $line['id'],
					'func' => $line['func'],
					'desc' => $line['description'],
				);
			}
			break;
		case 1:
			$resul = db_query('SELECT descrip,`fulltext` FROM `sm_smconst` WHERE fileid = '.intval($_GET['id']).'',__FILE__,__LINE__);
			while ($line = mysql_fetch_array($resul, MYSQL_ASSOC)) {
			    $context['infos'][] = $line;
			}
			break;
		case 2:
			$resul = db_query('SELECT cont FROM `sm_smfilescon` WHERE id = '.intval($_GET['id']).'',__FILE__,__LINE__);
			$context['infos'] = mysql_fetch_array($resul, MYSQL_ASSOC);
			break;		
	}
}

function HighLight(){
	global $context;
	
	$context['goon'] = isset($_GET['goon']);
	
	if($context['goon']){
	 	if(isset($_POST['signature']) && trim($_POST['signature']) != ""){
	 	 	$newinfo =stripslashes($_POST['signature']);
			HighLightThis($newinfo);			
		} elseif (is_uploaded_file($_FILES['uploadedfile']['tmp_name'])) {
		 	switch ($_FILES['uploadedfile']['error']){
	 	 	 	case 1: $context['imagerror'] = 'The uploaded file exceeds max size.'; break;
	 	 	 	case 2: $context['imagerror'] = 'The uploaded file exceeds max size.'; break;
	 	 	 	case 3: $context['imagerror'] = 'The uploaded file was only partially uploaded.'; break;
	 	 	 	case 7: $context['imagerror'] = 'Failed to write file to disk.'; break;
	 	 	 	case 8: $context['imagerror'] = 'File upload stopped by extension.'; break;
			}
			
			if(isset($context['imagerror']))
				return;
				
			$code = file_get_contents($_FILES['uploadedfile']['tmp_name']);
			HighLightThis($code);
			
			$context['topmenu'][] = Array (
				$_FILES['uploadedfile']['name'],
				'index.php?action=codehigh'		
			); 
		} else {
			$context['imagerror'] = 'Could not upload file, or no data found.';
			$context['topmenu'][] = Array (
				'MyCode',
				'index.php?action=codehigh'		
			); 
		}
	}
}

function HighLightThis($code){
	global $context;
 
	$search[0] = Array('&lt;?php&nbsp;','?&gt;');
	$replace[1] = Array('','');
	
	$theresults = Array();
	
	$result = mysql_query('SELECT id,func FROM sm_smfunctions') or die('Query failed: ' . mysql_error());
	while ($line = mysql_fetch_array($result, MYSQL_ASSOC)) {
	 	$theresults[] = Array (
		 	'id' => $line['id'],
			'func' => $line['func'],
			'strlen' => strlen($line['func']),
		);
	}
	
	//I need to sort it and do that stupid thing of $thereplaceid so that one function becomes two, like SetClientListeningFlags and SetClientListening		
	usort($theresults, "CompareSTRLEN");
	
	foreach($theresults as $go){
	 	$thereplaceid = "<!<" . $go['id'] . ">!>";
		$search[0][] = $go['func'];
		$replace[0][] = $thereplaceid;
		
		$search[1][] = $thereplaceid;
		$replace[1][] = '<a href="index.php?action=show&id='.$go['id'].'" onmouseout="hideSMFunc()" onmouseover="showSMfunc('.$go['id'].')">' . $go['func'] . '</a>';
	}
	
	$result = mysql_query('SELECT id,variable FROM sm_smdefine') or die('Query failed: ' . mysql_error());
	while ($line = mysql_fetch_array($result, MYSQL_ASSOC)) {
	 	$search[2][] = $line['variable'];
	 	$replace[2][] = '<a onmouseout="hideSMFunc()" onmouseover="showSMconst('.$line['id'].')">' . $line['variable'] . '</a>';
	}
	
	$str = highlight_string('<?php ' . $code . ' ?>', true);
	$str = str_replace($search[0], $replace[0], $str);
	$str = str_replace($search[1], $replace[1], $str);
	$str = str_replace($search[2], $replace[2], $str);
		
	$context['str'] = $str; //explode("<br />",$str);
}

function PreviewPost(){
	echo parse_bbc(stripslashes($_POST['message']));
	die();
}

function PostThis(){
	global $context,$templatefunc;
	
	if($context['user']['guest'])
		exit("3");
	
	if(!isset($_GET['id']) || $_GET['id'] == '' || strlen($_GET['id']) > 5)
		exit("0");
	
	$typearray = Array ( 'file' , 'func');
	if(!isset($_GET['type']) ||  array_search($_GET['type'], $typearray) === false)
		exit("0");
	
	if(!isset($_POST['message']) || $_POST['message'] == '')
		exit("1");
		
	$time = time();
		
	$query = db_query('SELECT time FROM sm_smposts WHERE ip = "'.$context['user']['ip'].'" ORDER BY time DESC LIMIT 1',__FILE__,__LINE__);
	if(mysql_num_rows($query) > 0){
		$line = mysql_fetch_array($query, MYSQL_NUM);
		if($time < $line[0] + 15)
			exit("2");
	}
		

	switch($_GET['type']){
		case "file":
			$query = db_query('SELECT id FROM sm_smfiles WHERE id = '.intval($_GET['id']).' LIMIT 1',__FILE__,__LINE__);
			if(mysql_num_rows($query) == 0)
				exit("0");

			
			$file = $_GET['id'];
			$func = -1;
			
			$afterfunc = "ShowFile";
		break;
		case "func":
			$query = db_query('SELECT inc FROM sm_smfunctions WHERE id = '.intval($_GET['id']).' LIMIT 1',__FILE__,__LINE__);
			if(mysql_num_rows($query) == 0)
				exit("0");
				
			$line = mysql_fetch_array($query);

			$file = $line[0];
			$func = $_GET['id'];
			
			$afterfunc = "ShowInfo";
		break;
	}

	db_query('INSERT INTO sm_smposts(file,func,time,poster,body,ip) VALUES 
	('.$file.','.$func.','.$time.',"'. $context['user']['name'] .'",\''.mysql_real_escape_string($_POST['message']).'\',"'.$context['user']['ip'].'")',__FILE__,__LINE__);
	
	$afterfunc();
	$templatefunc = $afterfunc . "_template";
}

function LoginWebiste(){
	echo LoginUser();
	exit;
}

function LogOutWebsite(){
 	global $cookieaddr, $scripturl, $cookiename;
	setcookie ($cookiename, '', time() - 100000, '/', $cookieaddr, 0 ,1);
	
	echo 'Logging out...';
	sleep(2);
	echo '<script type="text/javascript">window.location="'.$scripturl.'"</script>';
	exit;
}
?>
