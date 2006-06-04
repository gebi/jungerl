%%%-------------------------------------------------------------------
%%% Created : 21 Dec 2005 by Tobbe <tobbe@tornkvist.org>
%%% Descr.  : Collection of useful Javascript code.
%%%-------------------------------------------------------------------
-module(javascript).

-export([restSubmit/0, isNotEmpty/1, isNumber/1, isEmailAddr/1, 
	 validateForm/1, validateForm/2, validateForm/3, 
	 updateImgBox/0, updateImgObj/0, 
	 selectSubmit/0, openHelpWindow/0]).


%%%
%%% Use: [..., {href, "javascript:openHelpWindow('choose_domain');"}, ...]
%%%
openHelpWindow() ->
    "
// Open a Help Window 
function openHelpWindow(action) {
  window.open('help.yaws?action=' + action , 'helpWindow',
	      'resizable,height=400,width=600');
}
". % " 

%%%
%%% Use: [..., {href, "javascript:update_imgbox('box','cowboy.jpg');"}, ...]
%%%
updateImgBox() ->
    "
// Update a (div) box containing one image element
function updateImgBox(Id, Src) {
  var box = document.getElementById(Id);
  var image = new Image();
  image.src = Src;
  if (box.firstChild)
    box.replaceChild(image, box.firstChild);
  else
    box.appendChild(image);  
}
".
 
%%%
%%% Use: [..., {href, "javascript:update_imgobj('box',images[1]);"}, ...]
%%%
updateImgObj() ->
    "
// Update a (div) box containing one image element (for prechaching)
function updateImgObj(Id, Obj) {
  var box = document.getElementById(Id);
  var image = new Image();
  image.src = Obj.src;
  if (box.firstChild)
    box.replaceChild(image, box.firstChild);
  else
    box.appendChild(image);  
}
".
 

%%%
%%% Use: [..., {onchange, "restSubmit('xxx.yaws','date','month','2');"}, ...]
%%%
restSubmit() ->     
    "
// Issue a page request
function restSubmit(page, action, key, value)
{
  var str = page + '?action=' + action + '&key=' + key + '&value=' + value; 
  document.location = str;
}
".


%%%
%%% Use: [..., {onchange, "selectSubmit('xxx.yaws','date','months');"}, ...]
%%%
selectSubmit() ->     
    "
// Issue a page request with a selected menu value.
function selectSubmit(page, action, id)
{
  var e = document.getElementById(id);
  var ix = e.selectedIndex;
  var value = e.options[ix].value;

  if (value != '___') {
    var str = page + '?action=' + action + '&key=' + id + '&value=' + value; 
    document.location = str;
  }                                         
}
".   % "  fool emacs




isNotEmpty(Prompt) when list(Prompt) ->
    "
// validates that the field value string has one or more characters in it
function isNotEmpty(elem, prompt) {
  var str = elem.value;
  if (str == null || str.length == 0) {
    alert('"++Prompt++": ' + prompt);
    elem.style.backgroundColor = \"lightGrey\";
    elem.focus();                                                               
    elem.select();                                                              
    return false;
  } else {
    return true;
  }
}
". 


isNumber(Prompt) when list(Prompt) ->
    "
//validates that the entry is a positive or negative number                     
function isNumber(elem, prompt) {                                                       
  var str = elem.value;                                                   
  var re = /^[-]?\d*\.?\d*$/;                                                 
  str = str.toString();                                                       
  if (!str.match(re)) {                                                       
    alert('"++Prompt++": ' + prompt);
    elem.style.backgroundColor = \"lightGrey\";
    elem.focus();                                                               
    elem.select();                                                              
    return false;                                                           
  } else {                                                                           
    return true;
  }
}
".


%%% FIXME , This regexp doesn't seem to work
isEmailAddr(Prompt) ->
    "
// validates that the entry is formatted as an e-mail address                   
function isEMailAddr(elem, prompt) {                                                    
  var str = elem.value;                                                   
  var re = /^[\w-]+(\.[\w-]+)*@([\w-]+\.)+[a-zA-Z]{2,7}$/;                    
  if (!str.match(re)) {                                                       
    alert('"++Prompt++": ' + prompt);
    elem.style.backgroundColor = \"lightGrey\";
    elem.focus();                                                               
    elem.select();                                                              
    return false;                                                           
  } else {                                                                    
    return true;                                                            
  }                                                                           
}
". % "


%%%
%%% Validate that specified parts of a form are not empty.
%%% Takes a list of {ElementName, Prompt} typles...
%%% ...and as a special case: {AlertMsg, [EitherThis, OrThis]} which
%%% makes it possible to enforce either of two text fields to
%%% be filled in.
%%%
validateForm(L) -> 
    validateForm(L, {"Chose only one of the fields", []}, "true").

validateForm(L, E) when list(L) ->
    validateForm(L, E, "true").

validateForm(L, E, True) when list(L),list(True) ->
    "\n// validate the customer form\n"
	"function validateForm(form) {\n" ++
	produce_if_clauses(L, "  ", E, True) ++
	"  return false;\n"
	"}\n".

produce_if_clauses([{Name, Prompt}|T], Prefix, E, True) ->
    Prefix++"if (isNotEmpty(form."++Name++", '"++Prompt++"')) {\n" ++
	produce_if_clauses(T, "  "++Prefix, E, True) ++
	Prefix++"}\n";
produce_if_clauses([], Prefix, [], True) ->
    Prefix++"return "++True++";\n";
produce_if_clauses([], _Prefix, E, True) ->
    validateOneOf(E, True).




%%%
%%% Validate that specified parts of a form are not empty.
%%% Takes a list of {ElementName, Prompt} typles
%%%
validateOneOf({Alert, [{Name, Prompt}, {Name2, Prompt2}]}, True) ->
    "
// validate that only one of the fields are defined
  var elem1 = form."++Name++";
  var elem2 = form."++Name2++";
  var str1 = elem1.value;
  var str2 = elem2.value;
  if ((str1.length == 0 && str2.length == 0) || (str1.length != 0 && str2.length != 0))  {
    alert('"++Alert++": ' + '"++Prompt++"' + ' och ' + '"++Prompt2++"');
    elem1.style.backgroundColor = \"lightGrey\";
    elem2.style.backgroundColor = \"lightGrey\";
    elem1.focus();                                                               
    elem1.select();                                                              
    return false;
  } else {
    return "++True++";
  }
". 
