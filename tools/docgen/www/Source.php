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
	
	if(!empty($context['fileinfo']))
	{
		usort($context['fileinfo'], "SortByName");
	}
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
	
	$query = 'SELECT func,fullfunc,description,treturn,funcinput,exemple,inc,incname,typeof,onerror FROM `sm_smfunctions` WHERE id = '.intval($_GET['id']).' LIMIT 1';
	$result = db_query($query,__FILE__,__LINE__);
	
	
	$context['numresults'] = mysql_num_rows($result);
	
	if($context['numresults'] > 0){
		$context['answers'] = mysql_fetch_array($result, MYSQL_ASSOC);
		$context['optheader'] = $context['answers']['func'];
		
		
		$result = db_query('SELECT fcount,ccount FROM `sm_smfiles` WHERE id = '.$context['answers']['inc'].' LIMIT 1',__FILE__,__LINE__);
		$context['fileinfo'] = mysql_fetch_array($result, MYSQL_ASSOC);
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
