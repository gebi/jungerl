%%% File    : html.erl
%%% Author  : Tony Rogvall <tony@localhost.localdomain>
%%% Description : HTML utility functions
%%% Created :  5 May 2002 by Tony Rogvall <tony@localhost.localdomain>

-module(html).

-export([inline/0, flow/0, block/0]).
-export([is_fontstyle/1,
	 is_phrase/1,
	 is_special/1,
	 is_formctrl/1,
	 is_inline/1,
	 is_heading/1,
	 is_lst/1,
	 is_preformatted/1,
	 is_block/1,
	 is_flow/1,
	 is_pre_exclusion/1,
	 is_head_content/1,
	 is_head_misc/1,
	 is_head/1]).
-export([coreattrs/1,
	 i18n/1,
	 events/1,
	 attrs/1]).
-export([end_tag_optional/1]).
-export([chars/0, value/1]).



inline() -> fun(T) -> is_inline(T) end.
flow()   -> fun(T) -> is_flow(T) end.
block()  -> fun(T) -> is_block(T) end.
    
    

%% ENTITY % fontstyle
is_fontstyle(tt)     -> true;
is_fontstyle(i)      -> true;
is_fontstyle(b)      -> true;
is_fontstyle(big)    -> true;
is_fontstyle(small)  -> true;
is_fontstyle(erl)    -> true; %% EXTENSION
is_fontstyle(_)      -> false.

%% ENTITY % phrase
is_phrase(em)        -> true;
is_phrase(strong)    -> true;
is_phrase(dfn)       -> true;
is_phrase(code)      -> true;
is_phrase(samp)      -> true;
is_phrase(kbd)       -> true;
is_phrase(var)       -> true;
is_phrase(cite)      -> true;
is_phrase(abbr)      -> true;
is_phrase(acronym)   -> true;
is_phrase(erl)       -> true; %% EXTENSION
is_phrase(_)         -> false.

%% ENTITY % special
is_special(a)        -> true;
is_special(img)      -> true;
is_special(applet)   -> true;
is_special(object)   -> true;
is_special(font)     -> true;
is_special(basefont) -> true;
is_special(br)       -> true;
is_special(script)   -> true;
is_special(map)      -> true;
is_special(q)        -> true;
is_special(sub)      -> true;
is_special(sup)      -> true;
is_special(span)     -> true;
is_special(bdo)      -> true;
is_special(iframe)   -> true;
is_special(erl)      -> true; %% EXTENSION
is_special(_)        -> false.

%% ENTITY % formctrl
is_formctrl(input)    -> true;
is_formctrl(select)   -> true;
is_formctrl(textarea) -> true;
is_formctrl(label)    -> true;
is_formctrl(button)   -> true;
is_formctrl(erl)      -> true; %% EXTENSION
is_formctrl(_)        -> false.

%% ENTITY % inline
is_inline(pcdata)       -> true;
is_inline(ilayer)       -> true;
is_inline(Tag) ->
    is_fontstyle(Tag) orelse is_phrase(Tag) orelse
	is_special(Tag) orelse is_formctrl(Tag).

%% ENTITY % heading
is_heading(h1)       -> true;
is_heading(h2)       -> true;
is_heading(h3)       -> true;
is_heading(h4)       -> true;
is_heading(h5)       -> true;
is_heading(h6)       -> true;
is_heading(erl)      -> true; %% EXTENSION
is_heading(_)        -> false.
    
%% ENTITY % list
is_lst(ul)          -> true;
is_lst(ol)          -> true;
is_lst(dir)         -> true;
is_lst(menu)        -> true;
is_lst(erl)         -> true; %% EXTENSION
is_lst(_)           -> false.

%% ENTITY % preformatted
is_preformatted(pre)  -> true;
is_preformatted(erl)  -> true; %% EXTENSION
is_preformatted(_)    -> false.

%% ENTITY % block
is_block(p)          -> true;
is_block(dl)         -> true;
is_block('div')      -> true;
is_block(center)     -> true;
is_block(noscript)   -> true;
is_block(noframes)   -> true;
is_block(blockquote) -> true;
is_block(form)       -> true;
is_block(isindex)    -> true;
is_block(hr)         -> true;
is_block(table)      -> true;
is_block(fieldset)   -> true;
is_block(address)    -> true;
is_block(ilayer)     -> true;
is_block(layer)      -> true;
is_block(nolayer)    -> true;
is_block(Tag) ->
    is_heading(Tag) orelse is_lst(Tag) orelse is_preformatted(Tag).

%% ENTITY % flow
is_flow(Tag) ->
    is_block(Tag) orelse is_inline(Tag).

%% ENTITY % pre.exclusion
is_pre_exclusion(img) -> true;
is_pre_exclusion(object) -> true;
is_pre_exclusion(applet) -> true;
is_pre_exclusion(big) -> true;
is_pre_exclusion(small) -> true;
is_pre_exclusion(sub) -> true;
is_pre_exclusion(sup) -> true;
is_pre_exclusion(font) -> true;
is_pre_exclusion(basefont) -> true;
is_pre_exclusion(erl)  -> true; %% EXTENSION
is_pre_exclusion(_) -> false.
     
%% ENTITY % head.content
is_head_content(title) -> true;
is_head_content(base)  -> true;
is_head_content(erl)   -> true; %% EXTENSION
is_head_content(_) -> false.

%% ENTITY % head.misc
is_head_misc(script) -> true;
is_head_misc(style)  -> true;
is_head_misc(meta)   -> true;
is_head_misc(link)   -> true;
is_head_misc(object) -> true;
is_head_misc(erl)    -> true; %% EXTENSION
is_head_misc(_)      -> false.
    
is_head(Tag) ->
    is_head_content(Tag) orelse is_head_misc(Tag).

end_tag_optional(html) -> true;
end_tag_optional(head) -> true;
end_tag_optional(body) -> true;
end_tag_optional(p)    -> true;
end_tag_optional(li)   -> true;
end_tag_optional(dt)   -> true;
end_tag_optional(dd)   -> true;
end_tag_optional(thead) -> true;
end_tag_optional(tfoot) -> true;
end_tag_optional(tbody) -> true;
end_tag_optional(colgroup) -> true;
end_tag_optional(tr) -> true;
end_tag_optional(th) -> true;
end_tag_optional(td) -> true;
end_tag_optional(option) -> true;
end_tag_optional(_) -> false.

is_type(length)       -> cdata;
is_type(multilength)  -> cdata;
is_type(mulitlengths) -> cdata;
is_type(pixels)       -> cdata;
is_type(ialign)       -> {enum,[top,middle,bottom,left,right]};
is_type(contenttype)  -> cdata;
is_type(contenttypes) -> cdata;
is_type(charset)      -> cdata;
is_type(charsets)     -> cdata;
is_type(languagecode) -> name;
is_type(character)    -> cdata;
is_type(linktypes)    -> cdata;
is_type(mediadesc)    -> cdata;
is_type(uri)          -> cdata;
is_type(datetime)     -> cdata;
is_type(script)       -> cdata;
is_type(stylesheet)   -> cdata;
is_type(frametarget)  -> cdata;
is_type(text)         -> cdata;
is_type(color)        -> cdata; %% #RRGGBB | name
is_type(A) -> cdata. %% add more
    

is_attr(table, Attr) ->
    case Attr of
	summary      -> text;
	width        -> length;
	border       -> pixels;
	frame        -> tframe;
	rules        -> trules;
	cellspacing  -> length;
	cellpadding  -> length;
	bgcolor      -> color;
	datapagesize -> cdata;
	_ -> attrs(Attr)
    end;
is_attr(sub, Attr)  -> attrs(Attr);
is_attr(sup, Attr)  -> attrs(Attr);
is_attr(span, Attr) -> attrs(Attr); %% + reserved ??
is_attr(select,Attr) ->
    case Attr of
	name -> cdata;
	size -> number;
	multiple -> boolean;
	disabled -> boolean;
	tabindex -> number;
	onfocus  -> script;
	onblur   -> script;
	onchange -> script;
	_ -> attrs(Attr)
    end;
is_attr(option,Attr) ->
    case Attr of
	selected -> boolean;
	disabled -> boolean;
	label    -> text;
	value    -> cdata;
	_        -> attrs(Attr)
    end;
is_attr(optgroup,Attr) ->
    case Attr of
	disabled -> boolean;
	label    -> text;
	_        -> attrs(Attr)
    end;
    
is_attr(Tag, Attr) ->
    case is_fontstyle(Tag) of
	false -> case is_phrase(Tag) of
		     true  -> attrs(Attr);
		     false -> false
		 end;
	true -> attrs(Attr)
    end.

	    
bodycolors(bgcolor) -> color;
bodycolors(text)    -> color;
bodycolors(link)    -> color;
bodycolors(vlink)   -> color;
bodycolors(alink)   -> color;
bodycolors(_) -> false.
    

coreattrs(id)    -> id;         %% document-wide unique id
coreattrs(class) -> cdata;      %% space separated list of classes
coreattrs(style) -> stylesheet; %% associated style info
coreattrs(title) -> text;       %% advisory title/amplification
coreattrs(_)     -> false.

i18n(lang) -> languagecode;        %% language code
i18n(dir)  -> {enum,[ltr,rtl]};    %% direction for weak/neutral text
i18n(_)    -> false.

events(onclick)     -> script; %% a pointer button was clicked 
events(ondblclick)  -> script; %% a pointer button was double clicked
events(onmousedown) -> script; %% a pointer button was pressed down
events(onmouseup)   -> script; %% a pointer button was released 
events(onmouseover) -> script; %% a pointer was moved onto
events(onmousemove) -> script; %% a pointer was moved within
events(onmouseout)  -> script; %% a pointer was moved away 
events(onkeypress)  -> script; %% a key was pressed and released
events(onkeydown)   -> script; %% a key was pressed down
events(onkeyup)     -> script; %% a key was released
events(_) -> false.

reserved(datasrc)      -> uri;
reserved(datafld)      -> cdata;
reserved(dataformatas) -> {enum,[plaintext,html]};
reserved(_) -> false.
    
attrs(A) ->
    case coreattrs(A) of
	false ->
	    case i18n(A) of
		false -> 
		    events(A);
		Type -> Type
	    end;
	Type -> Type
    end.


%% HTML char table
chars() ->
    {
	"&#00;", "&#01;", "&#02;", "&#03;", "&#04;", "&#05;",
	"&#06;", "&#07;", "&#08;", "&#09;", "&#0A;", "&#0B;",
	"&#0C;", "&#0D;", "&#0E;", "&#0F;", "&#10;", "&#11;", 
	"&#12;", "&#13;", "&#14;", "&#15;", "&#16;", "&#17;",
	"&#18;", "&#19;", "&#1A;", "&#1B;", "&#1C;", "&#1D;",
	"&#1E;", "&#1F;", 
	" ", "!", "&quot;", "#", "\$", "%", "&amp;", "'", "(", ")", 
	"*", "+", ",", "-", ".", "/",
	"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
	":", ";", "&lt;", "=", "&gt;", "?", "@", 
	"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", 
	"L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", 
	"W", "X", "Y", "Z", 
	"[", "\\", "]", [$^],  %% Emacs mode bailing out
        "_", "`",
	"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
	"l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",
	"w", "x", "y", "z",
	"{", "|", "}", "~",
	"&#7F;", "&#80;", "&#81;", "&#82;", "&#83;", "&#84;", 
	"&#85;", "&#86;", "&#87;", "&#88;", "&#89;", "&#8A;", 
	"&#8B;", "&#8C;", "&#8D;", "&#8E;", "&#8F;", "&#90;",
	"&#91;", "&#92;", "&#93;", "&#94;", "&#95;", "&#96;", 
	"&#97;", "&#98;", "&#99;", "&#9A;", "&#9B;", "&#9C;", 
	"&#9D;", "&#9E;", "&#9F;",
	"&nbsp;", "&iexcl;", "&cent;", "&pound;", "&curren;", "&yen;", "&brvbar;",
	"&sect;", "&uml;", "&copy;", "&ordf;", "&laquo;", "&not;", "&shy;",
	"&reg;", "&macr;", "&deg;", "&plusmn;", "&sup2;", "&sup3;", "&acute;",
	"&micro;", "&para;", "&middot;", "&cedil;", "&sup1;", "&ordm;", "&raquo;",
	"&frac14;", "&frac12;", "&frac34;", "&iquest;", "&Agrave;", "&Aacute;",
	"&Acirc;", "&Atilde;", "&Auml;", "&Aring;", "&AElig;", "&Ccedil;",
	"&Egrave;", "&Eacute;", "&Ecirc;", "&Euml;", "&Igrave;", "&Iacute;",
	"&Icirc;", "&Iuml;", "&ETH;", "&Ntilde;", "&Ograve;", "&Oacute;",
	"&Ocirc;", "&Otilde;", "&Ouml;", "&times;", "&Oslash;", "&Ugrave;",
	"&Uacute;", "&Ucirc;", "&Uuml;", "&Yacute;", "&THORN;", "&szlig;",
	"&agrave;", "&aacute;", "&acirc;", "&atilde;", "&auml;", "&aring;",
	"&aelig;", "&ccedil;", "&egrave;", "&eacute;", "&ecirc;", "&euml;",
	"&igrave;", "&iacute;", "&icirc;", "&iuml;", "&eth;", "&ntilde;",
	"&ograve;", "&oacute;", "&ocirc;", "&otilde;", "&ouml;", "&divide;",
	"&oslash;", "&ugrave;", "&uacute;", "&ucirc;", "&uuml;", "&yacute;",
	"&thorn;", "&yuml;" 
       }.


value(Name) ->
    case Name of
        "lt" -> 60;
	"gt" -> 62;
        "amp" -> 38;
        "quot" -> $\";
        "nbsp" -> 160;
        "iexcl" -> 161;
        "cent" -> 162;
        "pound" -> 163;
        "curren" -> 164;
        "yen" -> 165;
        "brvbar" -> 166;
        "sect" -> 167;
        "uml" -> 168;
        "copy" -> 169;
        "ordf" -> 170;
        "laquo" -> 171;
        "not" -> 172;
        "shy" -> 173;
        "reg" -> 174;
        "macr" -> 175;
        "deg" -> 176;
        "plusmn" -> 177;
        "sup2" -> 178;
        "sup3" -> 179;
        "acute" -> 180;
        "micro" -> 181;
        "para" -> 182;
        "middot" -> 183;
        "cedil" -> 184;
        "sup1" -> 185;
        "ordm" -> 186;
        "raquo" -> 187;
        "frac14" -> 188;
        "frac12" -> 189;
        "frac34" -> 190;
        "iquest" -> 191;
        "Agrave" -> 192;
        "Aacute" -> 193;
        "Acirc" -> 194;
        "Atilde" -> 195;
        "Auml" -> 196;
        "Aring" -> 197;
        "AElig" -> 198;
        "Ccedil" -> 199;
        "Egrave" -> 200;
        "Eacute" -> 201;
        "Ecirc" -> 202;
        "Euml" -> 203;
        "Igrave" -> 204;
        "Iacute" -> 205;
        "Icirc" -> 206;
        "Iuml" -> 207;
        "ETH" -> 208;
        "Ntilde" -> 209;
        "Ograve" -> 210;
        "Oacute" -> 211;
        "Ocirc"-> 212;
        "Otilde" -> 213;
        "Ouml" -> 214;
        "times" -> 215;
        "Oslash" -> 216;
        "Ugrave" -> 217;
        "Uacute" -> 218;
        "Ucirc" -> 219;
        "Uuml" -> 220;
        "Yacute" -> 221;
        "THORN" -> 222;
        "szlig" -> 223;
        "agrave" -> 224;
        "aacute" -> 225;
        "acirc" -> 226;
        "atilde" -> 227;
        "auml" -> 228;
        "aring" -> 229;
        "aelig" -> 230;
        "ccedil" -> 231;
        "egrave" -> 232;
        "eacute" -> 233;
        "ecirc" -> 234;
        "euml" -> 235;
        "igrave" -> 236;
        "iacute" -> 237;
        "icirc" -> 238;
        "iuml" -> 239;
        "eth" -> 240;
        "ntilde" -> 241;
        "ograve" -> 242;
        "oacute" -> 243;
        "ocirc" -> 244;
        "otilde" -> 245;
        "ouml" -> 246;
        "divide" -> 247;
        "oslash" -> 248;
        "ugrave" -> 249;
        "uacute" -> 250;
        "ucirc" -> 251;
        "uuml" -> 252;
        "yacute" -> 253;
        "thorn" -> 254;
        "yuml" -> 255;
        _ -> error
end.

