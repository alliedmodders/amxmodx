<pre><?php

set_time_limit(300);
$downloadnew = FALSE;
$dir = ''; // Put DIR here

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
$version = "";
$thebasedir = dirname(__FILE__);
$defines = Array();
$javascript = "var SMfunctions=new Array()\nvar SMconstant=new Array()\nvar SMfiles=new Array()\nvar SMfiledata=new Array()\n";

function GetFunctionType($str){
	switch (strtolower($str)) {
		case "native": return 1; break;
		case "stock": return 2; break;
		case "forward": return 3; break;
    	case "functag": return 4; break;
	}
	return 0;
}

function PrepareJava($string){
	return addslashes(str_replace("\n","",nl2br($string)));	
}

function DecodeTheFile($file){
	global $funcs, $conts;
	$thelines = file($file);
	$thelines[] = "/**";
	
	$basename = basename($file);
	$linescount = count($thelines) - 1;
	
	$lines = Array();
	$isanotherone = false;
	
	foreach ($thelines as $line_num => $lin) {
	 	$line = trim($lin);
	 	
	 	if($line == "")	continue;
	 	
	 	if(substr($line,0,7) == "#define")
	 		AddDefine(substr($line,8));
	 	
	 	if(($line == "/**" && !$isanotherone) || $linescount == $line_num){
	 	 	if(isset($lines[1])){
		 		$temp = explode(" ",$lines[1][0]);	 		
		 		$type = GetFunctionType($temp[0]);
		 	 	if($type > 0){
					MakeFunction($lines, implode("", $lines[1]) ,$type,$basename);
				}else
					MakeConstant($lines,$basename);
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

function AddDefine($str){
 	if($str{0} == "_")
 		return;
 	global $defines;
 	
 	//Sryl, Preg match is just not working out, too much iregularitires
 	//The * will bug the preg_match
 	/*$str = str_replace("*","",$str);
	$pattern = '/([$A-Z|8|_^]+)\b([^.*]+)([	| |	]\/)/';
	
	if(!preg_match($pattern, $str, $matches,PREG_OFFSET_CAPTURE)){
		echo $str . "\n";
		return;
	}
	
	$new = substr($str , $matches[2][1] + strlen($matches[2][0]) );
	if(strlen($new) > 0){
		$new = str_replace(
			Array ("/**", "<" , "","/"),
			Array ("", "" , "",""),
			$new	
		);
	} 
	*/
	$stage = 0;
	$stageinfo = Array('','','');

	$strlen = strlen($str);
	for($i=0; $i < $strlen; $i++){	 
	 	if(trim($str{$i}) == "" && $stage == 0){
			$stage++;
	 		continue; 		
	 	}
	 	
	 	if($str{$i} == "/"){
			if(substr($str,$i,3) == "/**"){
			 	$stage++;
				$i += 3;
				continue;
			}
		}
		
		if($str{$i} == "*"){
			if(substr($str,$i,2) == '*/'){
			 	break;
			}
		}
		$stageinfo[$stage] .= $str{$i};
	}
	
	foreach($stageinfo as $i => $a)
		$stageinfo[$i] = trim($a);
	
	if($stageinfo[1] != ""){
		$defines[] = Array (
			'variable' => $stageinfo[0],
			'value' => wordwrap(trim($stageinfo[1]) , 35, " " , true ),
			'comment' => isset($stageinfo[2]) ? trim($stageinfo[2]) : "",
		);
	}
	
		
	if($stageinfo[0] == "SOURCEMOD_VERSION"){
		global $version;
		$version = str_replace('"',"",$stageinfo[1]);
	}
}

function MakeConstant($lines,$file){
 	global $consts;
 	
 	$info = Array();
 	$content = Array();
 	
	foreach($lines[0] as $line){
		if(substr($line,0,1) != "*") continue;
		$thesub = trim(substr($line,2));
		if($thesub == "") continue;
		
		$info[] = $thesub;		
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
 	$depreached = 0;
 	
 	$infostarted = false;
 	$lastone = 0;
 	
 	
	foreach($lines[0] as $line){
		if(substr($line,0,1) != "*") continue;
		$thesub = trim(substr($line,2));
		if($thesub == "") continue;
	
		
		if ($thesub{0} == "@")
			$infostarted = true;
			
		if(!$infostarted){
			$description[] =  $thesub;
		}

		if ($infostarted && $thesub{0} != "@"){
			switch ($lastone) {
				case 0:
	//				if(!isset($funcinput[ count($funcinput) - 1 ]))
	//					echo $file . "\n" . $function . "\n\n";
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

function GetFileAddr(){
	$lines = file("http://www.sourcemod.net/builds.php");
	
	foreach ($lines as $line_num => $lin) {
		if(substr($lin, 0, 13) == "<li><a href='"){
		 	$explode = explode("<a href='", $lin);
		 	foreach($explode as $a){
				if(strpos($a, ".zip")){
					$b = explode("'>", $a);
					return $b[0];
				}
			}
		}
	}
}

function DownloadNew(){
 	global $thebasedir;
	copy( GetFileAddr() ,'build.zip');
	
	$zip = new ZipArchive;
	if ($zip->open('build.zip') === TRUE) {
	    $zip->extractTo($thebasedir . "/build/" );
	    $zip->close();
	} else {
		exit( 'Failed to zip' );
	}
}

function full_rmdir( $dir ){
	if ( !is_writable( $dir ) ){
		if ( !@chmod( $dir, 0777 ) ){
                return FALSE;
		}
	}
	$d = dir( $dir );
	while ( FALSE !== ( $entry = $d->read() ) ){
		if ( $entry == '.' || $entry == '..' ){
			continue;
		}
		$entry = $dir . '/' . $entry;
		if ( is_dir( $entry ) ){
			if ( !$this->full_rmdir( $entry ) ){
				return FALSE;
			}
			continue;
		}
		if ( !@unlink( $entry ) ){
			$d->close();
			return FALSE;
		}
	}
	$d->close();
	return rmdir( $dir );
}

function FindFileID($file){
	global $tableinfo;
	foreach($tableinfo['sm_smfiles'] as $fname => $id)
		if($file == $fname)
			return $id;
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


if($downloadnew)
	DownloadNew();
if(strlen(trim($dir)) == 0)
	$dir = $thebasedir . '/build/addons/sourcemod/scripting/include';
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
db_query("TRUNCATE TABLE `sm_smdefine`"); 
db_query("UPDATE sm_smfunctions SET depreached = 2 WHERE typeof <> 4");

//db_query("TRUNCATE TABLE `smfiles`");
//db_query("TRUNCATE TABLE `smfunctions`");

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
	
	db_query('INSERT INTO `sm_smfilescon` VALUES ('. $tableinfo['sm_smfiles'][ $file['name'] ] .',\''.  addslashes( $file['content'] ) .'\')');

	$javascript .= 'SMfiles['. $tableinfo['sm_smfiles'][ $file['name'] ] .'] = "'. $onlyname .'"' . "\n";
	
}


foreach($funcs as $go){
 	$gop = str_replace('"', '\"', $go );
 	
 	$fid = FindFileID($gop['file']); 
 	
 	$javacontent = PrepareJava($gop['description'] );

 	if(isset($tableinfo['sm_smfunctions'][ $gop['function'] ])){
		$sql = 'UPDATE sm_smfunctions
			SET fullfunc = "'.$gop['fullfunc'].'",
			description = "'.$gop['description'].'",
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
	 	$sql = 'INSERT INTO sm_smfunctions(id,func,fullfunc,description,`treturn`,`onerror`,`funcinput`,inc,incname,typeof,version) VALUES
		('. $tableinfo['sm_smfunctions'][ $gop['function'] ] .',
		"'.$gop['function'].'",
		"'.$gop['fullfunc'].'",
		"'.$javacontent.'",
		"'.$gop['return'].'",
		"'.$gop['onerror'].'",
		"'.$gop['input'].'",
		'.$fid.',
		"'.$gop['file'].'",
		'.$gop['typeof'].', 
		\''.$version.'\'
		)';
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
	"'.$gop['info'].'",
	"'.$gop['content'].'"
	)';
	
	$query = db_query($sql);	
}

foreach($filefunclist as $id => $go){
	$javascript .= 'SMfiledata['. $id .'] = Array ('. implode(",",$go) .');' . "\n";
}

foreach($defines as $id => $go){
	$gop = str_replace('"', '\"', $go );
 	$sql = 'INSERT INTO `sm_smdefine` (`id`,`variable` ,`value` ,`comment`) VALUES ('.$id.',"'.$gop['variable'].'", "'.$gop['value'].'", "'.$gop['comment'].'")';
 	$javascript .= 'SMconstant['.$id.'] = Array ("'. PrepareJava($gop['variable']) .'","'. PrepareJava($gop['value']) .'","'. PrepareJava($gop['comment']) .'");' . "\n";
 	db_query($sql);
}

db_query('UPDATE `sm_sminfo` SET infob = "'.$version.'" WHERE master = "version"');

foreach($tableinfo['sm_smfiles'] as $fid){
	db_query('UPDATE sm_smfiles SET
`fcount` = (SELECT COUNT(*) FROM sm_smfunctions WHERE inc = '.$fid.'),
`ccount` = (SELECT COUNT(*) FROM sm_smconst WHERE fileid = '.$fid.')
WHERE id = '.$fid.'');
}

$res = mysql_query("SELECT id FROM sm_smfunctions WHERE depreached = 2");
while (($row = mysql_fetch_array($res)) !== FALSE)
{
	db_query("DELETE FROM sm_smposts WHERE func = " . $row[0]);
	db_query("DELETE FROM sm_smfunctions WHERE id = " . $row[0]);
}

mysql_free_result($res);

mysql_close($link);

//$filename = "SMfuncs.js";
if (!$handle = fopen($filename, 'w')) {
	echo "Cannot open file ($filename)";
	exit;
}

echo $javascript;

    // Write $somecontent to our opened file.
if (fwrite($handle, $javascript) === FALSE) {
	echo "Cannot write to file ($filename)";
	exit;
}

fclose($handle);

?>
