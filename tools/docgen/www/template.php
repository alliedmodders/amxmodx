<?php

function Headertemplate(){
 	
}

//padding: 10px;
function Main_template(){
 	global $context, $scripturl;
 	
 	$search = isset($_GET['query']) ? $_GET['query'] : "";
 	
 	echo '<html><head>
	<title>'.$context['optheader'] .' - SourceMod Scripting API Reference</title>
	<script src="script.js"></script>
	<script src="SMfuncs.js"></script>
	<script src="md5.js"></script>
	<link rel="stylesheet" type="text/css" href="style.css" />
	
	</head>
	<body>
	<div id="serverbox" class="descript"></div>
	<div id="AdminPopUP" class="LoadingBox" style="display:none;">Loading...</div>
	
	<table width="100%" cellpadding="0" cellspacing="0">
	<tr><td style="border-right-style: groove; background-color: #D0D2D6;width: 12%; height: 42;">
	<a href="'.$scripturl.'"><img src="header.jpg" alt="SourceMod Logo"></a>';
	
	
	if($context['user']['guest']){
		echo '<table width="100%" cellpadding="0" cellspacing="0"><tr><td>Welcome <i>Guest</i>.</td>
		<td style="text-align: right;"><a href="#" onclick="FlipLoginBox()">Login</a></td></tr></table>
		<div style="border: 1px solid black;margin:2px;padding:1px;display:none" id="LoginBox" >
		<form onsubmit="SubmitLoginInfo(); return false;">
		User: <input type="text" name="user" id="user" value="" size="16"/><br/>
		Pass: <input type="password" name="pw" id="pw" value="" size="16"/><br/>
		Remember me: <input type="checkbox" name="forever" id="forever" value="1" />
		<input type="submit" value="Enter"><br/>
		</form></div>';	
	} else{
		echo '<table width="100%" cellpadding="0" cellspacing="0"><tr><td>Hey, <b>'.$context['user']['name'].'</b>.</td>
		<td style="text-align: right;"><a href="'.$scripturl.'?action=logout">Logout</a></td></tr></table>';
	}
		
	echo '	</td><td style="background-color: WhiteSmoke; font-size: 9pt" rowspan="2" id="MainBody" valign="top">
	<br><br>
	Welcome to the SourceMod Scripting API Reference.  <br><br>
	For more information, see the <a href="http://wiki.alliedmods.net/Category:SourceMod_Scripting">SourceMod Scripting Wiki</a>, which contains tutorials on specific topics.
	<br><br>
	<br><br>
	<b>Your browser must support JavaScript to use this tool.</b>
	Enter a search term on the left to look for symbols in the SourceMod include files.  <br><br>
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
	
	foreach($context['fileinfo'] as $file){
		echo '<div style="margin: 2px;" onclick="SpanArea('.$file['id'].')">
			<img style="vertical-align: bottom" src="imgs/channel.gif" alt="#" /> 
			'.$file['name'].'
		</div><font id="'.$file['name'].'"></font>';
	}
	
	echo '</div>
	</div>
	</td></tr>
	
	<tr><td colspan="2" style="border-top-style: groove; background-color: #bebebe; text-align: right">
	API site created by Nican | SourceMod v.<b>'. $context['globalinfo']['version'][1] .'</td></tr>
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
 		
 	 	//echo '<a href="index.php">[ Source Mod ]</a> <a href="index.php?action=file&id='.$context['answers']['inc'].'"> [ '.$context['answers']['incname'].' ]</a>';
	 	echo '<h2 style="padding-left: 10px;">' . $context['answers']['func'] . '</h2>
	 	<b>Syntax:</b><div style="padding-left: 25px;" class="smalltext">';
	 	
			highlight_string($context['answers']['fullfunc']);

		 echo '</div><br/><b>Usage: </b><pre style="padding-left: 25px;">' . $context['answers']['funcinput'] . '</pre>';
		 
		 echo '<b>Notes</b>: <div style="padding-left: 25px;">'. $context['answers']['description'] . '		 
		
		</div><br/><b>Return: </b><div style="padding-left: 25px;">' , $context['answers']['treturn'] != "0" ? $context['answers']['treturn'] : '<i>No return.</i>' , '</div>';

		echo '<br /><b>Version Added:</b><pre style="padding-left: 25px;">' . $context['answers']['version'] . '</pre>';

		if($context['answers']['onerror'] != "")
			echo '<br/><b>On error / Errors: </b><div style="padding-left: 25px;">' , $context['answers']['onerror'] != "" ? $context['answers']['onerror'] : '<i>No error.</i>' , '</div>';
	
		echo '</div><br/><br/>';
		
		PrintPostingSection('func');
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
	 	PrintPostingSection('file');
		 
	} else
	echo 'No result found on that id.';
}

function PrintPostingSection($type){
	global $context;
	
	echo '<table style="border: medium solid #bebebe; width: 98%" cellpadding="0" cellspacing="0" align="center">
		 <tr style="background-color: #bebebe"><td>0 comments</td><td align="right"><a onclick="FlipPostSpan()">Post a new comment</a></td></tr>
		 
		 <tr id="postspan" style="display:none;"><td colspan="2">';
		 
		 if($context['user']['guest']){
			echo '<center><i><h2>Please login before posting</h2></i></center>';
		} else {
		 echo '<form action="index.php?fastload=file&id='.$_GET['id'].'" method="post" accept-charset="ISO-8859-1" name="postmodify" id="postmodify" onsubmit="return false;" enctype="multipart/form-data" style="margin: 0;">
		 
		 <table style="width: 95%;">
		 <tr></td><td><td>
		 	
			<a href="javascript:void(0);" onclick="surroundText(\'[sm]\', \'[/sm]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/sm.png" align="bottom" width="23" height="22" alt="SM code" title="SM code" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<img src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/divider.gif" alt="|" style="margin: 0 3px 0 3px;" />
			<a href="javascript:void(0);" onclick="surroundText(\'[b]\', \'[/b]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/bold.gif" align="bottom" width="23" height="22" alt="Bold" title="Bold" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[i]\', \'[/i]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/italicize.gif" align="bottom" width="23" height="22" alt="Italicized" title="Italicized" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[u]\', \'[/u]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/underline.gif" align="bottom" width="23" height="22" alt="Underline" title="Underline" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[s]\', \'[/s]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/strike.gif" align="bottom" width="23" height="22" alt="Strikethrough" title="Strikethrough" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<img src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/divider.gif" alt="|" style="margin: 0 3px 0 3px;" />
			<a href="javascript:void(0);" onclick="surroundText(\'[pre]\', \'[/pre]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/pre.gif" align="bottom" width="23" height="22" alt="Preformatted Text" title="Preformatted Text" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[left]\', \'[/left]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/left.gif" align="bottom" width="23" height="22" alt="Left Align" title="Left Align" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[center]\', \'[/center]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/center.gif" align="bottom" width="23" height="22" alt="Centered" title="Centered" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[right]\', \'[/right]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/right.gif" align="bottom" width="23" height="22" alt="Right Align" title="Right Align" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<img src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/divider.gif" alt="|" style="margin: 0 3px 0 3px;" />
			<a href="javascript:void(0);" onclick="surroundText(\'[size=10pt]\', \'[/size]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/size.gif" align="bottom" width="23" height="22" alt="Font Size" title="Font Size" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a>
			<a href="javascript:void(0);" onclick="surroundText(\'[font=Verdana]\', \'[/font]\', document.forms.postmodify.message); return false;"><img onmouseover="bbc_highlight(this, true);" onmouseout="if (window.bbc_highlight) bbc_highlight(this, false);" src="http://nican132.com/forum/Themes/halflife_11final/images/bbc/face.gif" align="bottom" width="23" height="22" alt="Font Face" title="Font Face" style="background-image: url(http://nican132.com/forum/Themes/halflife_11final/images/bbc/bbc_bg.gif); margin: 1px 2px 1px 1px;" /></a> 
			<select onchange="surroundText(\'[color=\' + this.options[this.selectedIndex].value.toLowerCase() + \']\', \'[/color]\', document.forms.postmodify.message); this.selectedIndex = 0; document.forms.postmodify.message.focus(document.forms.postmodify.message.caretPos);" style="margin-bottom: 1ex;">
				<option value="" selected="selected">Change Color</option>
				<option value="Black">Black</option>
				<option value="Red">Red</option>
				<option value="Yellow">Yellow</option>
				<option value="Pink">Pink</option>
				<option value="Green">Green</option>
				<option value="Orange">Orange</option>
				<option value="Purple">Purple</option>
				<option value="Blue">Blue</option>
				<option value="Beige">Beige</option>
				<option value="Brown">Brown</option>
				<option value="Teal">Teal</option>
				<option value="Navy">Navy</option>
				<option value="Maroon">Maroon</option>
				<option value="LimeGreen">Lime Green</option>
			</select>			
		
		
		 </td></tr>
		 <tr><td valign="top">Body:<br/><small>All BBC codes are avaliable.</small></td><td><textarea cols="75" rows="12" style="width: 95%; height: 250px;" name="message" tabindex="1"></textarea></td></tr>
		 <tr><td></td><td align="center">
		 	<input type="submit" name="post" value="Post" tabindex="3" onclick="return submitThisOnce(this,\''.$_GET['id'].'\',\''.$type.'\');" accesskey="s" />
			<input type="submit" name="preview" value="Preview" tabindex="4" onclick="return PreviewPost();" accesskey="p" />
		 
		</td></tr>
		 </table>
		 
		 </form>';
		 
		 }
		 
		 echo '</td></tr>
		 <tr><td id="previewspan" style="display:none;" colspan="2"></td></tr>';
		 if($context['sm']['pcount'] > 0){
		 	foreach($context['sm']['posts'] as $post){
				 echo '<tr style="background-color: #D9D9D9"><td style="border-top: medium solid #bebebe;"><b>'. $post['poster'] .'</b></td><td align="right" style="border-top: medium solid #bebebe;"><small>'. $post['time'] .'</small></td></tr>
				 <tr><td colspan="2" style="padding: 5px"><div>'. $post['body'] .'</div></td>';
			 }
		 } else {
			echo '<tr><td colspan="2" align="center" style="border-top: medium solid #bebebe;"> <i> No posts posted </i> </td></tr>';
		 }
		 echo '</table><br/>';
}

function Footertemplate(){
  global $context;
  
  /*echo '<hr>
  <div width="85%" align="right">
    <a href="http://www.nican132.com/forum/index.php"> Nican132.com </a> | SourceMod v.<b>'. $context['globalinfo']['version'][1] .'</b>
  </div>';*/
  
}

function HighLight_template(){
 	global $context;
 
	if($context['goon']){
	 	if(isset($context['imagerror']))
	 		echo 'There was an error:' . $context['imagerror'];
	 	else {
			echo "\n" . '<div id="serverbox" class="descript"></div><script src="SMfuncs.js"></script>' . "\n" . $context['str'];	 
		}
	} else {
		echo '<br/>
		In this section you can upload any file .sp and the website will automaticly highlight the codes in a easy way to understang it.<br/><small>Function still beta.</small><br/><br/>
		
		Upload a file:
		<form action="index.php?action=codehigh&goon" method="post" enctype="multipart/form-data">
		<input name="uploadedfile" type="file" size="80"/><br/>
		<br><h1 style="color:red">OR</h1><br/>
		Enter the code here:<br/>
		<textarea name="signature" rows="12" cols="80"></textarea><br/>
		<input type="submit" value="Submit" />
		</form>';
	}
}
?>
