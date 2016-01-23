opnedtab = -1;
var previewPopupWindow;
MainInformation = "";
var smf_images_url = "http://nican132.com/forum/Themes/halflife_11final/images";
var smf_formSubmitted = false;
var currentSwap = true;

function FlipPostSpan(){
	document.getElementById("postspan").style.display = currentSwap ? "" : "none";
	currentSwap = !currentSwap;
}

currentLoginSwap = true;

function FlipLoginBox(){
	document.getElementById("LoginBox").style.display = currentLoginSwap ? "" : "none";
	currentLoginSwap = !currentLoginSwap;
}

function myMouseMove(e){
	if (!e){
		var e = window.event;
	}
	if (e.pageX){
		myDiv.style.left = (e.pageX + 10) + "px";
		myDiv.style.top = (e.pageY  + 10) + "px";
	}else{
		myDiv.style.left = e.clientX + (document.documentElement.scrollLeft ? document.documentElement.scrollLeft : document.body.scrollLeft) + 20;
		myDiv.style.top = e.clientY + (document.documentElement.scrollTop ? document.documentElement.scrollTop : document.body.scrollTop) + 20;
	}
}

function ResetSearch(){
	document.getElementById("txt1").value="";
	showHint("");
}

function hideSMFunc(){
	myDiv = document.getElementById("serverbox");
	myDiv.style.visibility = "hidden";
	document.onmousemove = "";
}

function LoadPopUP(html){
	myDiv = document.getElementById("serverbox");
	myDiv.innerHTML = html;
	myDiv.style.visibility = "visible";
	document.onmousemove = myMouseMove;
}


function showSMfunc(id){
 	html = '<div style="background-color: #00AAAA"><b>';
 	html += SMfunctions[id][0]; 	
 	html += '</b></div><div style="padding: 2px;">'; 
 	html += SMfunctions[id][1]; 
 	html += '</div>';
 	
 	LoadPopUP(html);
}

function showSMconst(id){
 	html = '<div style="background-color: #00AAAA"><b>';
 	html += SMconstant[id][0]; 	
 	html += '</b></div><div style="padding: 2px;"><i>'; 
 	html += SMconstant[id][1];
 	html += '</i><br/>';
 	html += SMconstant[id][2];
 	html += '</div>';
 	
 	LoadPopUP(html);
}

String.prototype.trim = function () {
    return this.replace(/^\s*/, "").replace(/\s*$/, "");
}

function PrintMain(x){
	html = '<div style="margin: 2px;" onclick="SpanArea('+x+')">';
	html += '<img style="vertical-align: bottom" src="imgs/channel.gif" alt="#" /> ';
	html += SMfiles[x] + '</div><div id="'+ SMfiles[x] +'"></div>';
	return html;
}

var xmlHttp
var BodyHttp

function showHint(str){
    str=str.trim();
	if (str.length==0){ 
		//html = "";
		//for (x in SMfiles){
		//	html += PrintMain(x);
		//}
  		document.getElementById("txtHint").innerHTML= MainInformation;
  		return
  	}
  	
	xmlHttp=GetXmlHttpObject()
	
	if (xmlHttp==null){
  		alert ("Browser does not support HTTP Request")
  		return
  	} 
  	
  	document.getElementById("txtHint").innerHTML="<i>Loading...</i>"
  	
	var url="index.php?action=gethint&id="+str;
	xmlHttp.onreadystatechange=stateChanged 
	xmlHttp.open("GET",url,true)
	xmlHttp.send(null)
} 

function stateChanged(){ 
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete"){ 
 		document.getElementById("txtHint").innerHTML=xmlHttp.responseText 
 	} 
}

function LoadMainPage(url){
	BodyHttp=GetXmlHttpObject()
	
	if (BodyHttp==null){
  		alert ("Browser does not support HTTP Request")
  		return
  	}
  	ShowLoading()
	
	BodyHttp.onreadystatechange=MainStateChanged 
	BodyHttp.open("GET",url,true)
	BodyHttp.send(null)
}

function MainStateChanged(){ 
	if (BodyHttp.readyState==4 || BodyHttp.readyState=="complete"){ 
		HideLoading()
 		document.getElementById("MainBody").innerHTML=BodyHttp.responseText 
 	} 
}

function ShowCustomLink(page){
	LoadMainPage("index.php?" + page);
}

function ShowFunction(id){
	hideSMFunc()
	LoadMainPage("index.php?action=show&id="+id);
}

function ShowFileInfo(id){
	LoadMainPage("index.php?action=file&id="+id);
}
function ShowLoading(){
	ly = document.getElementById("AdminPopUP");
	
	ly.style.zindex = "100";
	ly.style.display = "block";
}

function HideLoading(){
	document.getElementById("AdminPopUP").style.display = "none";
}

function SpanArea(id, hashtml){
	if(opnedtab >= 0){
		document.getElementById( SMfiles[opnedtab] ).innerHTML="";
		if(opnedtab == id){
			opnedtab = -1;
			return
		}
	}
	opnedtab = id;
	ShowFileInfo(id);
	
	if(!SMfiledata[id])
		return;
	
	html = "";
	arycount = SMfiledata[id].length -1
	
	for (x in SMfiledata[id]){
		html += '<img style="vertical-align: bottom" src="imgs/tree_';
		if(x == arycount) html+= 'end'; else html+= 'mid';
		html += '.gif" alt="&#9500;" /><a onclick="ShowFunction('+ SMfiledata[id][x] +')" onmouseout="hideSMFunc()" onmouseover="showSMfunc('+ SMfiledata[id][x] +')">';
		html += SMfunctions[ SMfiledata[id][x] ][0] + "</a><br>";
		
	}
	if(html != "")
		document.getElementById( SMfiles[id] ).innerHTML=html
	
}

function getHTMLDocument(url, callback)
{
	if (!window.XMLHttpRequest)
		return false;

	var myDoc = new XMLHttpRequest();
	if (typeof(callback) != "undefined")
	{
		myDoc.onreadystatechange = function ()
		{
			if (myDoc.readyState != 4)
				return;

			if (myDoc.responseText != null && myDoc.status == 200){
				callback(myDoc.responseText);
			}
		};
	}
	myDoc.open('GET', url, true);
	myDoc.send(null);

	return true;
}

function SubmitLoginInfo(){
  	ShowLoading();
  	
  	var url = "index.php?action=login&user=" + document.getElementById("user").value;
  	url += "&pw=" + hex_md5(document.getElementById("pw").value);
  	if(document.getElementById("forever").checked)
  		url += "&forever=1";
  	else
  		url += "&forever=0";
	
	getHTMLDocument(url, Revivelogin);
	
	return false;
}

function Revivelogin(html){ 
	HideLoading();
 	if(html == "ok"){
 		window.location="index.php"
 		return;
 	}
 		
	alert(html); 		
}

// Send a post form to the server using XMLHttpRequest.
function senHTMLDocument(url, content, callback)
{
	if (!window.XMLHttpRequest)
		return false;

	var sendDoc = new window.XMLHttpRequest();
	if (typeof(callback) != "undefined")
	{
		sendDoc.onreadystatechange = function ()
		{
			if (sendDoc.readyState != 4)
				return;

			if (sendDoc.responseText != null && sendDoc.status == 200)
				callback(sendDoc.responseText);
			else
				callback(false);
		};
	}
	sendDoc.open('POST', url, true);
	if (typeof(sendDoc.setRequestHeader) != "undefined")
		sendDoc.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
	sendDoc.send(content);

	return true;
}

function GetXmlHttpObject(){
	var xmlHttp=null;
	try{
 		// Firefox, Opera 8.0+, Safari
 		xmlHttp=new XMLHttpRequest();
 	}
	catch (e){
	 	// Internet Explorer
	 	try{
	  		xmlHttp=new ActiveXObject("Msxml2.XMLHTTP");
	  	}
 		catch (e){
  			xmlHttp=new ActiveXObject("Microsoft.XMLHTTP");
  		}
 	}
	return xmlHttp;
} 

function surroundText(text1, text2, textarea)
{
	// Can a text range be created?
	if (typeof(textarea.caretPos) != "undefined" && textarea.createTextRange)
	{
		var caretPos = textarea.caretPos, temp_length = caretPos.text.length;

		caretPos.text = caretPos.text.charAt(caretPos.text.length - 1) == ' ' ? text1 + caretPos.text + text2 + ' ' : text1 + caretPos.text + text2;

		if (temp_length == 0)
		{
			caretPos.moveStart("character", -text2.length);
			caretPos.moveEnd("character", -text2.length);
			caretPos.select();
		}
		else
			textarea.focus(caretPos);
	}
	// Mozilla text range wrap.
	else if (typeof(textarea.selectionStart) != "undefined")
	{
		var begin = textarea.value.substr(0, textarea.selectionStart);
		var selection = textarea.value.substr(textarea.selectionStart, textarea.selectionEnd - textarea.selectionStart);
		var end = textarea.value.substr(textarea.selectionEnd);
		var newCursorPos = textarea.selectionStart;
		var scrollPos = textarea.scrollTop;

		textarea.value = begin + text1 + selection + text2 + end;

		if (textarea.setSelectionRange)
		{
			if (selection.length == 0)
				textarea.setSelectionRange(newCursorPos + text1.length, newCursorPos + text1.length);
			else
				textarea.setSelectionRange(newCursorPos, newCursorPos + text1.length + selection.length + text2.length);
			textarea.focus();
		}
		textarea.scrollTop = scrollPos;
	}
	// Just put them on the end, then.
	else
	{
		textarea.value += text1 + text2;
		textarea.focus(textarea.value.length - 1);
	}
}

function textToEntities(text)
{
	var entities = "";
	for (var i = 0; i < text.length; i++)
	{
		var charcode = text.charCodeAt(i);
		if ((charcode >= 48 && charcode <= 57) || (charcode >= 65 && charcode <= 90) || (charcode >= 97 && charcode <= 122))
			entities += text.charAt(i);
		else
			entities += "&#" + charcode + ";";
	}

	return entities;
}

function bbc_highlight(something, mode){
	something.style.backgroundImage = "url(" + smf_images_url + (mode ? "/bbc/bbc_hoverbg.gif)" : "/bbc/bbc_bg.gif)");
}

function PreviewPost(){
	x = new Array();
	var textFields = ["message"];
	
	ShowLoading();
	
	for (i in textFields)
		if (document.forms.postmodify.elements[textFields[i]])
			x[x.length] = textFields[i] + "=" + escape(textToEntities(document.forms.postmodify[textFields[i]].value.replace(/&#/g, "&#38;#"))).replace(/\+/g, "%2B");

	senHTMLDocument("index.php?action=previewpost", x.join("&"), PreviewPostSent);
	
	return false;
}

function PreviewPostSent(html){
	if (previewPopupWindow)
		previewPopupWindow.close();
		
	HideLoading();
	
	thespan = document.getElementById("previewspan");
	
	thespan.style.display = "";
	thespan.innerHTML = "Preview: <br/> <div style=\"padding: 5px\">" + html + "</div>";			
}

function submitThisOnce(form,id,post)
{
	if (typeof(form.form) != "undefined")
		form = form.form;

	for (var i = 0; i < form.length; i++)
		if (typeof(form[i]) != "undefined" && form[i].tagName.toLowerCase() == "textarea")
			form[i].readOnly = true;

	x = new Array();
	var textFields = ["message","poster"];
	
	ShowLoading();
	
	
	for (i in textFields)
		if (document.forms.postmodify.elements[textFields[i]])
			x[x.length] = textFields[i] + "=" + escape(textToEntities(document.forms.postmodify[textFields[i]].value.replace(/&#/g, "&#38;#"))).replace(/\+/g, "%2B");
	
	senHTMLDocument("index.php?action=post&id=" + id + "&type=" + post, x.join("&"), SentPost);
	
	return false;
}

function SentPost(html){
	HideLoading();
	html=html.trim();
	
	if(html == "0")
		return alert("Invalid data.")
	else if (html == "1")
		return alert("No body data.")
	else if (html == "2")
		return alert("Please at least wait 15 secs between each post.");
	else if (html == "3")
		return alert("Please login before posting.");
	else
		document.getElementById("MainBody").innerHTML= html;	
							
}

