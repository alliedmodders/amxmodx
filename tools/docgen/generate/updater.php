<pre><?php

set_time_limit(300);
$dir = __DIR__ . '/include'; // Put DIR here

// Database credentials.
$host = '';
$user = '';
$password = '';
$database = '';

// Output filename.
$filename = '../www/SMfuncs.js';

// Start defines
$funcs = Array();
$filefunclist = Array();
$tableinfo = Array();
$consts = Array();
$thebasedir = dirname(__FILE__);
$javascript = "var SMfunctions=new Array()\nvar SMfiles=new Array()\nvar SMfiledata=new Array()\n";

function GetFunctionType($str){
	switch (strtolower($str)) {
		case "native": return 1; break;
		case "stock": return 2; break;
		case "forward": return 3; break;
	}
	return 0;
}

function PrepareJava($string){
	return addslashes(htmlspecialchars($string));	
}

function DecodeTheFile($file){
	global $funcs, $conts;
	$thelines = file($file);
	$thelines[] = "/**";
	
	$basename = basename($file);
	$linescount = count($thelines) - 1;
	
	$lines = Array();
	$isanotherone = false;
	$commented = false;
	
	foreach ($thelines as $line_num => $lin) {
	 	$line = trim($lin);
	 	
	 	if($line == "")	continue;
	 	
	 	//if(substr($line,0,7) == "#define")
	 	//	AddDefine(substr($line,8));
	 	
	 	if(( ( $line == "/**" || substr( $line, 0, 2 ) == '/*' ) && !$isanotherone) || $linescount == $line_num)
	 	{
	 	 	if(isset($lines[1]))
	 	 	{
		 		$temp = explode(" ",$lines[1][0]);	 		
		 		$type = GetFunctionType($temp[0]);
		 	 	if($type > 0)
		 	 	{
					MakeFunction($lines, implode("", $lines[1]) ,$type,$basename);
				}
				else
				{
					MakeConstant($lines,$basename);
				}
			}
	 	 
			$lines = Array();
			$isanotherone = false;
			$commented = true;	 	 	
	 	}
	
		if($line == "{")
			$isanotherone = true;
		elseif($line{0} == "}")
			$isanotherone = false;
			
		if($commented){
			$lines[0][] = $line;
		} else {
			$lines[1][] = $lin;
		}
		
		if($line == "*/" && !$isanotherone)
		 	$commented = false;	 
	}
	return implode("",$thelines);
}

function MakeConstant($lines,$file){
 	global $consts;
 	
 	$info = Array();
 	$content = Array();
 	
 	if( !empty( $lines[0] ) )
 	{
	 	foreach($lines[0] as $line){
			if(substr($line,0,1) != "*") continue;
			$thesub = trim(substr($line,2));
			if($thesub == "") continue;
			
			$info[] = $thesub;		
		}
 	}
	
	
	$theinfo = str_replace("@section","", implode("\n",$info));
	
	if(strpos($theinfo,"All rights reserved.")){
		$theinfo = "<i>Unclassified</i>";
		$newline = Array();
		
		foreach($lines[1] as $lin){
			$line = trim($lin);
			if(substr($line,0,7) == "#define" && $line{8} != "_")
				$newline[] = $line . "\n";
		}
		if(count($newline) == 0)
			return;
		
		$lines[1] = $newline;		
	}
	
	$consts[] = Array(
		'info' => $theinfo,
		'content' => trim(implode('',$lines[1])),
		'file' => $file,
	);
	
}

function MakeFunction($lines,$function,$type,$file){ 
 	global $funcs;
 	
 	$description = Array();
 	$funcinput = Array();
 	$return = Array();
 	$onerror = Array();
 	$notes = Array();
 	
 	$infostarted = false;
 	$lastone = 0;
 	$depreached = 0;
 	
	foreach($lines[0] as $line){
		if(substr($line,0,1) != "*") continue;
		$thesub = substr($line,2);
		
		if(trim($thesub == "")) continue;
		
		if ($thesub{0} == "@")
			$infostarted = true;
			
		if(!$infostarted){
			$description[] =  $thesub;
		}
		
		if ($lastone === 5 && $infostarted && $thesub{0} != "@")
		{
			$infostarted = false;
		}
		
		if ($infostarted && $thesub{0} != "@")
		{
			switch ($lastone) {
				case 0:
					if(!isset($funcinput[ count($funcinput) - 1 ]))
						echo $file . "\n" . $function . "\n\n";
					else
						$funcinput[ count($funcinput) - 1 ] .= " " . $thesub; 
					break;
				case 1: $return[ count($return) - 1 ] .= " " . $thesub; break;
				case 2: $onerror[ count($onerror) - 1 ]  .= " " . $thesub; break;
				case 3: $notes[ count($notes) - 1 ] .= " " . $thesub; break;
				case 4: $description[ count($description) - 1 ] .= " " . $thesub; break;
			}
		}
		
		if(!$infostarted)
			$continue;
		
		if(substr($thesub, 0, 7) == "@return"){
			$return[] = trim(substr($thesub, 7));
			$lastone = 1;
		} elseif(substr($thesub, 0, 11) == "@deprecated"){
			$depreached = 1;
			$lastone = 5;
		} elseif(substr($thesub, 0, 6) == "@error"){
			$onerror[] = trim(substr($thesub, 6));
			$lastone = 2;
		} elseif(substr($thesub, 0, 9) == "@noreturn"){
			$return[] = 0;
		} elseif(substr($thesub, 0, 5) == "@note"){
		 	$notes[] = substr($thesub, 5);
		 	$lastone = 3;
		} elseif(substr($thesub, 0, 6) == "@param") {
			$funcinput[] = substr($thesub, 6);
			$lastone = 0;				
		} elseif(substr($thesub, 0, 6) == "@brief") {
			$description[] = substr($thesub, 6);
			$lastone = 4;
		}
	}
	
	/*$fullcommand = str_replace(
		Array("Float:","Handle:","Action:","bool:","any:","GroupId:","Function:","ReplySource:","QueryCookie:"),
		Array("","","","","","","","",""),
		$function);*/
	
	$temp = explode("(",$function);
	$func = $temp[0];
	if(strpos($func, " ") !== false){
	 	$func = trim(substr( $func , strpos($func , " ") ));
	}
	
	if(strpos($function, "{") !== false){
	 	$function = trim(substr( $function, 0, strpos($function, "{") ));
	}
		
	$thestrpos = strpos($func,":");
	if($thestrpos !== false){
		$func = substr($func ,$thestrpos + 1);	
	}

	$funcs[] = Array(
		'description' => implode("\n",$description),
	 	'input' => implode("\n",$funcinput),
	 	'function' => $func,
	 	'fullfunc' => trim($function),
	 	'return' => implode("\n",$return),
	 	'onerror' => implode("\n",$onerror),
	 	'notes' => implode("\n",$notes),
	 	'file' => $file,
	 	'typeof' => $type,
	 	'depreached' => $depreached
	); 	
}

function db_query($query){
	$q = mysql_query($query);
	if(!$q){
		echo "\n" . mysql_error() . "\n" . $query . "<hr>";
	}
	return $q;
}

function FindFileID($file){
	global $tableinfo;
	
	if( isset( $tableinfo['sm_smfiles'][ $file ] ) )
	{
		return $tableinfo['sm_smfiles'][ $file ];
	}
	
	return -1;
}


function FindUnusedid($arrayname){
	global $tableinfo;
//	if(!isset($tableinfo[ $arrayname ]))
//		return 0;
	
	$tempinfo = array_flip($tableinfo[ $arrayname ]);
	$i = 0;
	//Hope this works!!
	while( TRUE ){
		if(!isset($tempinfo[ $i ]))
			return $i;
		$i++;
	}
}

$files = Array();

if(!($dir_handle = opendir($dir))){
    exit('Could not open ' . $dir);
}
   
while($file = readdir($dir_handle)){
	if($file == "." || $file == "..") continue;
	
	if(substr($file, -4, 4) == ".inc"){
	 	$filedir = $dir . "/" .  $file;
		
//		if($file == "version.inc"){
//			GetVersion($filedir);
//		} else {
			$files[] = Array(
				'Addr' => $filedir,
				'name' => $file,
//				'fcount' => 0,
//				'ccount' => 0,
				'content' => DecodeTheFile($filedir),
			);
//		}		
	}
}
closedir($dir_handle);

$link = mysql_connect($host, $user, $password)
    or die('Could not connect: ' . mysql_error());
mysql_select_db($database) or die('Could not select database');


db_query("TRUNCATE TABLE `sm_smconst`");
db_query("TRUNCATE TABLE `sm_smfilescon`");
//db_query("TRUNCATE TABLE `sm_smfunctions`");

$tables = Array (
	'sm_smfiles' => Array ('filename','id'),
	'sm_smfunctions' => Array ('func','id'),
);

foreach ($tables as $name => $table){
	$result = db_query('SELECT ' . $table[0] . ',' . $table[1] . ' FROM ' . $name);
	
	$tableinfo[$name] = Array();
	
	while ($line = mysql_fetch_array($result, MYSQL_NUM)) {
		$tableinfo[$name][$line[0]] = $line[1];
	}
	
	mysql_free_result($result);
}

foreach($files as $file){
	$onlyname = str_replace('.inc','',$file['name']);
	
	if(!isset($tableinfo['sm_smfiles'][ $file['name'] ])){
		//0 is a ID, so count will make it just right...
		$tableinfo['sm_smfiles'][ $file['name'] ] = FindUnusedid('sm_smfiles');
		db_query('INSERT INTO sm_smfiles(id,name,filename) VALUES ('. $tableinfo['sm_smfiles'][ $file['name'] ] .',"'.$onlyname.'","'.$file['name'].'")');
	}
	
	db_query('INSERT INTO `sm_smfilescon` VALUES ('. $tableinfo['sm_smfiles'][ $file['name'] ] .',\''.  addslashes( htmlspecialchars($file['content']) ) .'\')');

	$javascript .= 'SMfiles['. $tableinfo['sm_smfiles'][ $file['name'] ] .'] = "'. $onlyname .'"' . "\n";
	
}


foreach($funcs as $go){
 	$gop = str_replace('"', '\"', $go );
 	
 	$fid = FindFileID($gop['file']); 
 	
 	$javacontent = PrepareJava($gop['description'] );

 	if(isset($tableinfo['sm_smfunctions'][ $gop['function'] ])){
		$sql = 'UPDATE sm_smfunctions
			SET fullfunc = "'.$gop['fullfunc'].'",
			description = "'.$javacontent.'",
			`treturn` = "'.$gop['return'].'",
			`onerror` = "'.$gop['onerror'].'",
			`funcinput` = "'.$gop['input'].'",
			inc = '.$fid.',
			incname = "'.$gop['file'].'",
			typeof ='.$gop['typeof'].',
			depreached = '.$gop['depreached'].'
			WHERE id = '. $tableinfo['sm_smfunctions'][ $gop['function'] ] .'';
	} else {
		$tableinfo['sm_smfunctions'][ $gop['function'] ] =  FindUnusedid('sm_smfunctions');
	 	$sql = 'INSERT INTO sm_smfunctions(id,func,fullfunc,description,`treturn`,`onerror`,`funcinput`,inc,incname,typeof) VALUES
		('. $tableinfo['sm_smfunctions'][ $gop['function'] ] .',
		"'.$gop['function'].'",
		"'.$gop['fullfunc'].'",
		"'.$javacontent.'",
		"'.$gop['return'].'",
		"'.$gop['onerror'].'",
		"'.$gop['input'].'",
		'.$fid.',
		"'.$gop['file'].'",
		'.$gop['typeof'].')';
	}
	
	$javascript .= 'SMfunctions['. $tableinfo['sm_smfunctions'][ $gop['function'] ] .'] = Array ("'.$gop['function'].'","'.$javacontent.'");' . "\n";
	
	$filefunclist[$fid][] = $tableinfo['sm_smfunctions'][ $gop['function'] ];
	
	$query = db_query($sql);	
}

foreach($consts as $go){
 	$gop = str_replace('"', '\"', $go );
 	$fid = FindFileID($gop['file']);
 
 	$sql = 'INSERT INTO sm_smconst(fileid,descrip,`fulltext`) VALUES
	('.$fid.',
	"'.htmlspecialchars($gop['info']).'",
	"'.htmlspecialchars($gop['content']).'"
	)';
	
	$query = db_query($sql);	
}

foreach($filefunclist as $id => $go){
	$javascript .= 'SMfiledata['. $id .'] = Array ('. implode(",",$go) .');' . "\n";
}

foreach($tableinfo['sm_smfiles'] as $fid){
	db_query('UPDATE sm_smfiles SET
`fcount` = (SELECT COUNT(*) FROM sm_smfunctions WHERE inc = '.$fid.'),
`ccount` = (SELECT COUNT(*) FROM sm_smconst WHERE fileid = '.$fid.')
WHERE id = '.$fid.'');
}

mysql_close($link);

if (!$handle = fopen($filename, 'w')) {
	echo "Cannot open file ($filename)";
	exit;
}

// Write $somecontent to our opened file.
if (fwrite($handle, $javascript) === FALSE) {
	fclose($handle);
	echo "Cannot write to file ($filename)";
	exit;
}

fclose($handle);
