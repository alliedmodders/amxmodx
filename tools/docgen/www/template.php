<?php

function Main_template(){
 	global $context;
 	
 	$search = isset($_GET['query']) ? $_GET['query'] : "";
 	
 	echo '<html><head>
	<title>'.$context['optheader'] .' - AMXX Scripting API Reference</title>
	<script src="script.js"></script>
	<link rel="stylesheet" type="text/css" href="style.css">
	
	</head>
	<body>
	<div id="AdminPopUP" class="LoadingBox" style="display:none;">Loading...</div>
	
	<table width="100%" cellpadding="0" cellspacing="0">
	<tr><td style="border-right-style: groove; background-color: #D0D2D6;width: 12%; height: 42;text-align:centerf">
	<a href="http://amxmodx.net/api/"><img src="imgs/header.gif" alt="AMXX"></a>';
	
	echo '	</td><td style="background-color: WhiteSmoke; font-size: 9pt" rowspan="2" id="MainBody" valign="top">
	<br><br>
	Welcome to the AMX Mod X Scripting API Reference.  <br><br>
	For more information, see the <a href="http://wiki.alliedmods.net/Category:SourceMod_Scripting">SourceMod Scripting Wiki</a>, which contains tutorials on specific topics.
	<br><br>
	<br><br>
	<b>Your browser must support JavaScript to use this tool.</b>
	Enter a search term on the left to look for symbols in the AMX Mod X include files.<br><br>
	Alternately, click a file on the left to browse its functions/symbols or see its contents. <br><br>
	Click "Reset" to get back to the main page, or "Link to page" to link someone to the page you\'re seeing.<br>
	<br>
	</td></tr>
	<tr><td style="border-right-style: groove; border-top: 1px solid black; width: 12%; background-color: #E4E8EB" valign="top">
		<table width="85%" align="center" cellpadding="3" cellspacing="0"><tr>
		<td style="background-color: #A5A5A5" align="center"><a href="http://forums.alliedmods.net/forumdisplay.php?f=52" target="_blank">Forums</a></td><td width="5%"></td>
		<td style="background-color: #A5A5A5" align="center"><a href="http://wiki.alliedmods.net/Category:SourceMod_Scripting" target="_blank">Articles</a></td>
		</tr></table>
	<hr width="95%">
	<div style="padding: 3px;">';
	
	echo 'Search: <input type="text" name="search" value="'.$search.'" size="18" id="txt1" onkeyup="showHint(this.value)">';
	
	
	echo '<div style="text-align: right;width: 85%"><a onclick="ResetSearch()"><small>Reset</small></a></div>
	<div style="font-size: 10pt" id="txtHint">';	
	
	if(!empty($context['fileinfo']))
	{
		asort( $context['fileinfo'] );
		
		foreach( $context['fileinfo'] as $id => $file )
		{
			echo '<div style="margin: 2px;" onclick="SpanArea(\''.$id.'\', \''.$file.'\')">
				<img style="vertical-align: bottom" src="imgs/channel.gif" alt=""> 
				'.$file.'
			</div><font id="'.$file.'" style="display:none">';
			
			$TreeCount = Count( $context['filefunctions'][ $id ] ) - 1;
			$TreeCurrent = 0;
			
			foreach( $context['filefunctions'][ $id ] as $id2 => $function )
			{
				echo '<img style="vertical-align: bottom" src="imgs/tree_';
				
				if( ++$TreeCount === $TreeCount )
				{
					echo 'end';
				}
				else
				{
					echo 'mid';
				}
				
				echo '.gif" alt=""><a onclick="ShowFunction(\'' . $id2 . '\')">' . $function . '</a><br>';
			}
			
			echo '</font>';
		}
	}
	
	echo '</div>
	</div>
	</td></tr>
	
	<tr><td colspan="2" style="border-top-style: groove; background-color: #bebebe; text-align: right">API site created by Nican</td></tr>
	</table>';
		
	echo '<script> 
	MainInformation = document.getElementById("txtHint").innerHTML;';
	
  if(isset($_GET['fastload']))
		echo 'ShowCustomLink("'.GenerateLink(true).'");';
	
	if($search != ""){
     echo 'showHint("'.$search.'");';
  }
		
	echo '</script>';
}

function GenerateLink($backwards = false){
	$link = "";
	foreach($_GET as $name => $info){
		$link .= htmlspecialchars(urlencode($name)) . '=' . htmlspecialchars(urlencode($info)) . '&';
	}
	
	if($backwards)
		$link = str_replace("fastload","action",$link);
	else
		$link = str_replace("action","fastload",$link);
	
	return $link;
}

function PrintUpperTab($fileid, $fcount, $ccount){
	global $context;
	$fileid = (int)$fileid;
	echo '<table cellpadding="0" cellspacing="0" style="height: 25px;"><tr><td style="width: 25px;"></td>
	<td class="UpperTab">';

	if( $ccount > 0)
		echo '<a onclick="ShowCustomLink(\'action=file&id='.$fileid.'\')">Constants</a>';
	else
		echo 'Constants';
	
	echo '</td><td style="width: 10px;"></td><td class="UpperTab">';
	
	if($fcount > 0)
		echo '<a onclick="ShowCustomLink(\'action=file&id='.$fileid.'&type\')">Functions</a>';
	else
		echo 'Functions';
	
	echo '</td><td style="width: 10px;"></td>';
	echo '<td class="UpperTab"><a onclick="ShowCustomLink(\'action=file&id='.$fileid.'&file\')">File</a></td>
	<td style="width: 10px;"></td>
	<td valign="top"><small><a href="index.php?'.GenerateLink().'">Link to page</a></small></td>
	</tr></table>';
}

function ShowOpts_template(){
	global $context;
	
	if($context['numresults'] > 100){
		echo 'More than 100 results found. <br/> Please try something smaller';
		return;
	}
	
	echo '<div id="ts2_layer" style="font-size: 8pt">';
	if($context['numresults'] > 0){
	 	foreach ($context['files'] as $fid => $file){
			echo '<span class="t_c" style="cursor: pointer" onclick="ShowFileInfo('.$fid.')">
			<img style="vertical-align: bottom" src="imgs/channel.gif" alt="#" /> <b>'. $file .'</b></span>';
			
			foreach ($context['answers'][ $fid ] as $id => $func){
				echo '<br/><img style="vertical-align: bottom" src="imgs/tree_' , $context['lastone'][$fid] == $id ? 'end' : 'mid' , '.gif" alt="&#9500;" /><a onclick="ShowFunction('.$id.')" onmouseout="hideSMFunc()" onmouseover="showSMfunc('.$id.')">' . $func . "</a>";
			}
			echo '<br>';
		}
	}
	echo '</div>';
	echo $context['numresults']. ' results found.';

}

//func,fullfunc,description,treturn,funcinput,exemple,inc,typeof

function ShowInfo_template(){
 	global $context;
 
 	if($context['numresults'] > 0){
 		PrintUpperTab( $context['answers']['inc'] , $context['fileinfo']['fcount'], $context['fileinfo']['fcount']);
 
 		echo '<div style="padding: 25px;">';
 		
	 	echo '<h2 style="padding-left: 10px;">' . $context['answers']['func'] . '</h2>
	 	<b>Syntax:</b><div style="padding-left: 25px;" class="smalltext">';
	 	
			highlight_string($context['answers']['fullfunc']);

		 echo '</div><br/><b>Usage: </b><pre style="padding-left: 25px;">' . $context['answers']['funcinput'] . '</pre>';
		 
		 echo '<b>Notes</b>: <pre style="padding-left: 25px;">'. $context['answers']['description'] . '		 
		
		</pre><br/><b>Return: </b><div style="padding-left: 25px;">' , $context['answers']['treturn'] != "0" ? $context['answers']['treturn'] : '<i>No return.</i>' , '</div>';

		if($context['answers']['onerror'] != "")
			echo '<br/><b>On error / Errors: </b><div style="padding-left: 25px;">' , $context['answers']['onerror'] != "" ? $context['answers']['onerror'] : '<i>No error.</i>' , '</div>';
	
		echo '</div>';
	} else
	echo 'No result found on that id.';

}

function ShowFile_template(){
 	global $context;
 
 	if($context['numresults'] > 0){
	 	PrintUpperTab( $_GET['id'] , $context['fcount'], $context['ccount']);
 
 		echo '<div style="padding: 25px;">';
 
		switch($context['goon']){
			case 0:
				echo '<table border="1" class="tpc">';
				foreach($context['infos'] as $i => $ans){
					echo '<tr style="background-color: ',$i%2 ? '#D8D8D8' : '#C8C8C8',';"><td><a onclick="ShowFunction('.$ans['id'].')">' . $ans['func'] . '</a></td><td>' . $ans['desc'] . '</td></tr>';
				}
				echo '</table>';
				break;
			case 1:
				echo '<ul>';
				foreach($context['infos'] as $id => $info){
					echo '<li><a href="#'.$context['letters'][$id].'">' . $info['descrip'] . '</a></li>';
				}
				
				echo '</ul>';
				foreach($context['infos'] as $id => $info){
				echo '<a name="'.$context['letters'][$id].'"></a>
					<b>' . $info['descrip'] . '</b>
					<div style="padding-left: 25px;"><pre>'. $info['fulltext'] .'</pre></div><br/><br/>';
				}
				break;
			case 2:
				echo '<pre>'.$context['infos']['cont'].'</pre>';
				break;		
		}
	 	
	 	echo '</div>';
		 
	} else
	echo 'No result found on that id.';
}
