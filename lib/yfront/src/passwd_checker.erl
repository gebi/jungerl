%%%-------------------------------------------------------------------
%%% Created : 1 Dec 2006 by Tobbe <tobbe@tornkvist.org>
%%% Desc.   : A (pro-active) password checker.
%%%
%%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%%% 
%%% @doc <b>passwd_checker</b> is a pro-active password checker.
%%%       It makes use of a tiny port program that uses cracklib.
%%%
%%%       Download the dictionaries you want to use, e.g see:
%%%
%%%          http://www.cotse.com/tools/wordlists.htm
%%%
%%%       Then rebuild the dictionary, on Gentoo:
%%%
%%%          create-cracklib-dict /usr/share/dict/*
%%%
%%%       On RedHat:
%%%
%%%          mkdict /usr/share/dict/* | packer /usr/lib/cracklib_dict
%%%
%%%       On Ubuntu/Debian, see the man page for cracklib. You'll
%%%       probably need to set the environment variable CRACKLIB_DICTPATH
%%%       to: CRACKLIB_DICTPATH=/var/cache/cracklib/cracklib_dict
%%%
%%%       See also:
%%%
%%%          http://gdub.wordpress.com/2006/08/26/using-cracklib-to-require-stronger-passwords/
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(passwd_checker).

-export([check/1, check/2, format/1]).

-include("../include/passwd_checker.hrl").


cracklib_dict_path() -> "/usr/lib/cracklib_dict". 

%%%
%%% @doc Textual error messages.
%%%
format(?PWD_CHK_TOO_SHORT)  -> "too short";           % Should be gettext'ified when needed...
format(?PWD_CHK_DICTIONARY) -> "too common";
format(_)                   -> "not secure enough".

%%%
%%% @doc Check if the password is good enough.
%%%      By setting the environment variable CRACKLIB_DICTPATH to the
%%%      full path name + filename prefix of the cracklib dictionary 
%%%      database, it will override the default in cracklib_dict_path/0.
%%%
check(Passwd) ->
    case os:getenv("CRACKLIB_DICTPATH") of
	Path when list(Path) ->
	    check(Passwd, Path);
	_ ->
	    check(Passwd, cracklib_dict_path())
    end.

%%%
%%% @doc Check if the password is good enough. 
%%%      Specify the path to the cracklib dictionaries.
%%%
check(Passwd, CrackDictPath) ->
    PrivDir = code:priv_dir(yfront),
    Cmd = PrivDir++"/passwd_checker "++Passwd++" "++CrackDictPath,
    case os:cmd(Cmd) of
	"ok"++_ -> ok;
	Error   -> {error, analyse_string(Error)}
    end.

%%% lame attempt of returning some more (lang. independent) detailed info...
analyse_string(Str) ->
    is_too_short(Str).

is_too_short(Str) ->
    case substr(Str, "too short") of
	true -> ?PWD_CHK_TOO_SHORT;
	_    -> is_dictionary(Str)
    end.

is_dictionary(Str) ->
    case substr(Str, "dictionary") of
	true -> ?PWD_CHK_DICTIONARY;
	_    -> ?PWD_CHK_OTHER
    end.

substr(Str, Sub) ->
    case string:index(Str, Sub) of
	I when I>0 -> true;
	_          -> false
    end.

