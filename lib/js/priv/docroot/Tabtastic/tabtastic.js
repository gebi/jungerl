//*** This code is copyright 2002-2003 by Gavin Kistner and Refinery; www.refinery.com
//*** It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt
//*** Reuse or modification is free provided you abide by the terms of that license.
//*** (Including the first two lines above in your source code satisfies the conditions.)

//***Adds a new class to an object, preserving existing classes
function AddClass(obj,cName){ KillClass(obj,cName); return obj && (obj.className+=(obj.className.length>0?' ':'')+cName); }

//***Removes a particular class from an object, preserving other existing classes.
function KillClass(obj,cName){ return obj && (obj.className=obj.className.replace(new RegExp("^"+cName+"\\b\\s*|\\s*\\b"+cName+"\\b",'g'),'')); }

//***Returns true if the object has the class assigned, false otherwise.
function HasClass(obj,cName){ return (!obj || !obj.className)?false:(new RegExp("\\b"+cName+"\\b")).test(obj.className) }


//*** This code is copyright 2003 by Gavin Kistner, gavin@refinery.com
//*** It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt
//*** Reuse or modification is free provided you abide by the terms of that license.
//*** (Including the first two lines above in your source code satisfies the conditions.)


//***Cross browser attach event function. For 'evt' pass a string value with the leading "on" omitted
//***e.g. AttachEvent(window,'load',MyFunctionNameWithoutParenthesis,false);

function AttachEvent(obj,evt,fnc,useCapture){
	if (!useCapture) useCapture=false;
	if (obj.addEventListener){
		obj.addEventListener(evt,fnc,useCapture);
		return true;
	} else if (obj.attachEvent) return obj.attachEvent("on"+evt,fnc);
	else{
		MyAttachEvent(obj,evt,fnc);
		obj['on'+evt]=function(){ MyFireEvent(obj,evt) };
	}
} 

//The following are for browsers like NS4 or IE5Mac which don't support either
//attachEvent or addEventListener
function MyAttachEvent(obj,evt,fnc){
	if (!obj.myEvents) obj.myEvents={};
	if (!obj.myEvents[evt]) obj.myEvents[evt]=[];
	var evts = obj.myEvents[evt];
	evts[evts.length]=fnc;
}
function MyFireEvent(obj,evt){
	if (!obj || !obj.myEvents || !obj.myEvents[evt]) return;
	var evts = obj.myEvents[evt];
	for (var i=0,len=evts.length;i<len;i++) evts[i]();
}



//*** This library is copyright 2004 by Gavin Kistner, gavin@refinery.com
//*** It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt
//*** Reuse or modification is free provided you abide by the terms of that license.
//*** (Including the first two lines above in your source code mostly satisfies the conditions.)

//*** Tabtastic -- see http://phrogz.net/JS/Tabstatic/index.html
//*** Version 1.0    20040430   Initial release.
//***         1.0.2  20040501   IE5Mac, IE6Win compat.
//***         1.0.3  20040501   Removed IE5Mac/Opera7 compat. (see http://phrogz.net/JS/Tabstatic/index.html#notes)
//***         1.0.4  20040521   Added scroll-back hack to prevent scrolling down to page anchor. Then commented out :)
//***         1.0.5  20051004   Added workaround for bad scrolling behaviour. <tobbe@tornkvist.org>

AttachEvent(window,'load',function(){
	var tocTag='ul',tocClass='tabset_tabs',tabTag='a',contentClass='tabset_content';


	function FindEl(tagName,evt){
		if (!evt && window.event) evt=event;
		if (!evt) return DebugOut("Can't find an event to handle in DLTabSet::SetTab",0);
		var el=evt.currentTarget || evt.srcElement;
		while (el && (!el.tagName || el.tagName.toLowerCase()!=tagName)) el=el.parentNode;
		return el;
	}

	function SetTabActive(tab){
		if (tab.tabTOC.activeTab){
			if (tab.tabTOC.activeTab==tab) return;
			KillClass(tab.tabTOC.activeTab,'active');
			if (tab.tabTOC.activeTab.tabContent) KillClass(tab.tabTOC.activeTab.tabContent,'tabset_content_active');
			//if (tab.tabTOC.activeTab.tabContent) tab.tabTOC.activeTab.tabContent.style.display='';
			if (tab.tabTOC.activeTab.prevTab) KillClass(tab.tabTOC.activeTab.previousTab,'preActive');
			if (tab.tabTOC.activeTab.nextTab) KillClass(tab.tabTOC.activeTab.nextTab,'postActive');
		}
		AddClass(tab.tabTOC.activeTab=tab,'active');
		if (tab.tabContent) AddClass(tab.tabContent,'tabset_content_active');				
		//if (tab.tabContent) tab.tabContent.style.display='block';
		if (tab.prevTab) AddClass(tab.prevTab,'preActive');
		if (tab.nextTab) AddClass(tab.nextTab,'postActive');
	}
	function SetTabFromAnchor(evt){
		//setTimeout('document.body.scrollTop='+document.body.scrollTop,1);
		SetTabActive(FindEl('a',evt).semanticTab);
	}

	
	function Init(){
		window.everyTabThereIsById = {};
		
		var anchorMatch = /([a-z][\w.:-]*)$/i,match;
		var activeTabs = [];
		
		var tocs = document.getElementsByTagName(tocTag);
		for (var i=0,len=tocs.length;i<len;i++){
			var toc = tocs[i];
			if (!HasClass(toc,tocClass)) continue;

			var lastTab;
			var tabs = toc.getElementsByTagName(tabTag);
			for (var j=0,len2=tabs.length;j<len2;j++){
				var tab = tabs[j];
				if (!tab.name || !(match=anchorMatch.exec(tab.name))) continue;
				if (lastTab){
					tab.prevTab=lastTab;
					lastTab.nextTab=tab;
				}
				tab.tabTOC=toc;
				everyTabThereIsById[tab.tabID=match[1]]=tab;
				tab.tabContent = document.getElementById(tab.tabID);
				
				if (HasClass(tab,'active')) activeTabs[activeTabs.length]=tab;
				
				lastTab=tab;
			}
			AddClass(toc.getElementsByTagName('li')[0],'firstchild');
		}

		for (var i=0,len=activeTabs.length;i<len;i++){
			SetTabActive(activeTabs[i]);
		}

		for (var i=0,len=document.links.length;i<len;i++){
			var a = document.links[i];
			if (!(match=anchorMatch.exec(a.name))) continue;
			if (a.semanticTab = everyTabThereIsById[match[1]]) AttachEvent(a,'click',SetTabFromAnchor,false);
		}
		
		if ((match=anchorMatch.exec(location.name)) && (a=everyTabThereIsById[match[1]])) SetTabActive(a);
		
	}
	Init();
},false);