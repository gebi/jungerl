%%% File    : zip.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : ZIP file utils
%%% Created : 25 Apr 2003 by Tony Rogvall <tony@bit.hemma.se>

-module(zip).

-include_lib("kernel/include/file.hrl").
-include("../include/zip.hrl").

-import(lists, [reverse/1]).

-compile(export_all).
-export([info/1, 
	 open/2, 
	 close/1,
	 list_dir/2, 
	 read_file_info/2,
	 read_file/2]).


-define(ZIP_READ,  1).
-define(ZIP_WRITE, 2).

%% zip descriptor
-record(zip,
	{
	  fd,
	  mode = 0,
	  dict,
	  access = read,
	  major_device = 0
	 }).


info(File) ->
    case open(File, [read]) of
	{ok,Z} ->
	    ets:foldl(fun({Path,undefined,_Cs},Acc) ->
			      FName = path_to_filename(Path),
			      io:format("~s\n", [FName]), Acc;
			 ({Path,F,_Cs},Acc) ->
			      FName = path_to_filename(Path),
			      Compression =
				  if F#zip_file.usize == 0 -> 0;
				     true ->
					  trunc((F#zip_file.size/F#zip_file.usize)*100)
				  end,
			      io:format("~40s ~8w ~3w% method=~w\n",
					[FName,
					 F#zip_file.usize,
					 Compression,
					 F#zip_file.method
					]),
			      Acc
		      end, [], Z#zip.dict),
	    close(Z);
	Error -> 
	    Error
    end.

%%
%% Open zip file and read directory
%%
open(ZipFile, Mode) ->
    Zip = open_mode(#zip {}, Mode),
    Opts = open_opts(Zip#zip.mode),
    case file:open(ZipFile, Opts) of
	{ok, Fd} ->
	    {ok,Info} = file:read_file_info(ZipFile),
	    Dict = ets:new(zip, []),
	    each_zip_file(Fd, 
			  fun(F, _Acc) ->
				  zip_insert(Dict, F)
			  end, ok),
	    {ok, Zip#zip { fd = Fd, 
			   dict = Dict, 
			   access=Info#file_info.access,
			   major_device = Info#file_info.major_device 
			  }};
	Error ->
	    Error
    end.
    
    
close(Zip) ->	    
    %% flush write changes
    file:close(Zip#zip.fd),
    ets:delete(Zip#zip.dict).

list_dir(Zip, Dir) ->
    case ets:lookup(Zip#zip.dict, filename_to_path(Dir)) of
	[{_,ZF,[]}] ->
	    if ZF#zip_file.size =/= 0 ->
		    {error, enotdir};
	       true ->
		    {ok, []}
	    end;
	[{_,_ZF,Cs}] ->
	    {ok, Cs};
	[] ->
	    {error, enoent}
    end.
    

%% read info (converted to file info)
read_file_info(Zip, File) ->
    case ets:lookup(Zip#zip.dict, filename_to_path(File)) of
	[{_,Info,_}] ->
	    zip_file_info(Zip,Info);
	[] ->
	    {error, enoent}
    end.


%% read a compressed file
read_file(Zip, File) ->
    case ets:lookup(Zip#zip.dict, filename_to_path(File)) of
	[{_,Info,[]}] ->
	    file:position(Zip#zip.fd, Info#zip_file.offset),
	    case read_zip_lfile(Zip#zip.fd) of
		{ok, _LInfo} ->
		    _Decrypt = (Info#zip_file.flags band (1 bsl 0) =/= 0),
		    %% io:format("lfile=~p, size=~p decrypt=~p\n", 
		    %%   [LInfo, Info#zip_file.size, Decrypt]),
		    case file:read(Zip#zip.fd, Info#zip_file.size) of
			{ok, Bin} ->
			    case Info#zip_file.method of
				?Z_METH_UNCOMPRESSED ->
				    {ok, Bin};
				?Z_METH_DEFLATED ->
				    zlib:unzip(Bin);
				_ ->
				    {error, method_not_implemented}
			    end;
			Error -> Error
		    end;
		Error -> Error
	    end;
	[{_,_,[_|_]}] ->
	    {error, eisdir};
	[] ->
	    {error, enoent}
    end.
    


open_opts(Mode) ->
    if Mode band ?ZIP_READ == 0 -> [];
       true -> [read]
    end ++
    if Mode band ?ZIP_WRITE == 0 -> [];
       true -> [write]
    end ++
    [raw, binary].


open_mode(Zip, [read|Mode]) ->
    open_mode(Zip#zip { mode = Zip#zip.mode bor ?ZIP_READ }, Mode);
open_mode(Zip, [write|Mode]) ->
    open_mode(Zip#zip { mode = Zip#zip.mode bor ?ZIP_WRITE }, Mode);
open_mode(Zip, []) ->
    Zip.

%% convert to internal file name
filename_to_path(FileName) ->
    reverse(filename:split(FileName)).

%% convert to external file name
path_to_filename([]) -> ".";
path_to_filename(Path) -> 
    filename:join(reverse(Path)).

%%
%% Insert entry into Zip internal dictionary
%%
%% a/b/c.txt =>
%%     { [],         undefined, ["a"]}
%%     { ["a"],      undefined, ["b"]}
%%     { ["b","a"],  undefined, ["c.txt"]}
%%     { ["c.txt","b","a"], Info, []}
%%
zip_insert(Dict, ZF) ->
    NList = filename:split(ZF#zip_file.fname),
    zip_insert_path(Dict, NList, ZF).

zip_insert_path(Dict, Path, ZF) ->
    zip_insert_path(Dict, Path, ZF, []).

zip_insert_path(Dict, [A|Rest], ZF, PPath) ->
    case ets:lookup(Dict, PPath) of
	[{PPath,Z,Cs}] ->
	    case lists:member(A, Cs) of
		true ->
		    zip_insert_path(Dict, Rest, ZF, [A|PPath]);
		false ->
		    ets:insert(Dict, {PPath,Z,Cs++[A]}),
		    zip_insert_path(Dict, Rest, ZF, [A|PPath])
	    end;
	[] ->
	    ets:insert(Dict, {PPath,undefined,[A]}),
	    zip_insert_path(Dict, Rest, ZF, [A|PPath])
    end;
zip_insert_path(Dict, [], ZF, Path) ->
    case ets:lookup(Dict, Path) of
	[{Path,_Z,Cs}] ->
	    ets:insert(Dict, {Path,ZF,Cs});
	[] ->
	    ets:insert(Dict, {Path,ZF,[]})
    end.

zip_file_info(Zip, ZF) ->
    Type = case lists:last(ZF#zip_file.fname) of
	       $/ ->
		   ZF#zip_file.size == 0,
		   directory;
	       _ ->
		   regular
	   end,
    Info = #file_info { size   = ZF#zip_file.usize,
			type   = Type,
			access = Zip#zip.access,
			mtime  = {msdos_to_date(ZF#zip_file.date),
				  msdos_to_time(ZF#zip_file.time)},
			ctime  = 0,
			mode   = 0,
			links  = 1,
			major_device = Zip#zip.major_device,
			minor_device = 0,
			inode = 0,
			uid   = 0,
			gid   = 0
		       },
    {ok, Info}.

%%
%% Convert to erlang date
%%
msdos_to_date(Date) ->
    { ((Date band 16#FE00) div 16#200) + 1980,
      ((Date band 16#01E0) div 16#20),
      (Date band 16#1F) }.

msdos_to_time(Time) ->
    { (Time band 16#F800) div 16#800,
      (Time band 16#7E0) div 16#20,
      (Time band 16#1F)*2 }.
	       
    
%%
%% Each zip_lfile may be used to scan each file from beginning of file
%%
each_zip_lfile(Fd, Fun, Acc) ->
    case magic(Fd) of
	ok ->
	    file:position(Fd,0),
	    each_zip_lfile_1(Fd, Fun, Acc);
	Error -> Error
    end.

each_zip_lfile_1(Fd, Fun, Acc) ->
    case read_zip_lfile(Fd) of
	{ok, LF} ->
	    Acc1 = Fun(LF, Acc),
	    file:position(Fd, {cur,LF#zip_lfile.size}),
	    if LF#zip_lfile.flags band (1 bsl 3) == 0 ->
		    each_zip_lfile_1(Fd, Fun, Acc1);
	       true ->
		    {error,not_implemented}
	    end;
	{error,bad_magic} ->
	    Acc;
	Error -> Error
    end.

%%
%% Scan each file in the central directory
%%
each_zip_file(Fd, Fun, Acc) ->
    case read_zip_end(Fd) of
	{ok,End} ->
	    file:position(Fd, End#zip_end.offset),
	    each_zip_file_1(Fd, Fun, Acc, End#zip_end.nent);
	Error -> Error
    end.

each_zip_file_1(_Fd, _Fun, Acc, 0) -> Acc;
each_zip_file_1(Fd, Fun, Acc, I) ->
    case read_zip_file(Fd) of
	{ok, F} ->
	    Acc1 = Fun(F, Acc),
	    each_zip_file_1(Fd, Fun, Acc1, I-1);
	Error ->
	    Error
    end.

magic(Fd) ->
    file:position(Fd, 0),
    case file:read(Fd, 4) of
	{ok,<<?ZIP_LFILE_SIG:32/little>>} ->
	    ok;
	{ok,_} ->
	    {error,bad_magic};
	Error ->
	    Error
    end.


read_zip_lfile(Fd) ->
    case file:read(Fd, 30) of
	{ok, <<?ZIP_LFILE_SIG:32/little,
	      Ver:16/little,
	      Flags:16/little, Method:16/little,
	      Time:16/little, Date:16/little, Crc:32/little, 
	      Size:32/little, USize:32/little,
	      FNameLen:16/little, ExtraLen:16/little>>} ->
	    {ok,FName} = readn(Fd,FNameLen),
	    {ok,Extra} = readn(Fd,ExtraLen),
	    {ok,#zip_lfile { sig = ?ZIP_LFILE_SIG,
			     ver = Ver,
			     flags = Flags,
			     method = Method,
			     time = Time,
			     date = Date,
			     crc = Crc,
			     size = Size,
			     usize = USize,
			     fnamelen = FNameLen,
			     extralen = ExtraLen,
			     fname = binary_to_list(FName),
			     extra = Extra }};
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.


read_zip_file(Fd) ->
    case file:read(Fd, 46) of
	{ok, <<?ZIP_FILE_SIG:32/little,
	      Ver:16/little, Verx:16/little,
	      Flags:16/little, Method:16/little,
	      Time:16/little, Date:16/little, Crc:32/little, 
	      Size:32/little, USize:32/little,
	      FNameLen:16/little, ExtraLen:16/little,
	      CommentLen:16/little,
	      Dsk:16/little, IAttr:16/little,
	      Attr:32/little, Offset:32/little>>} ->
	    {ok,FName} = readn(Fd,FNameLen),
	    {ok,Extra} = readn(Fd,ExtraLen),
	    {ok,Comment} = readn(Fd,CommentLen),
	    {ok,#zip_file { sig = ?ZIP_FILE_SIG,
			    ver = Ver,
			    verx = Verx,
			    flags = Flags,
			    method = Method,
			    time = Time,
			    date = Date,
			    crc = Crc,
			    size = Size,
			    usize = USize,
			    fnamelen = FNameLen,
			    extralen = ExtraLen,
			    commentlen = CommentLen,
			    dsk = Dsk,
			    iattr = IAttr,
			    attr = Attr,
			    offset = Offset,
			    fname = binary_to_list(FName),
			    extra = Extra,
			    comment = binary_to_list(Comment) }};
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.

%%
%% Locate and read the end header
%% FIXME: check for zip64 format i.e if the zip64 locator is above!
%%
read_zip_end(Fd) ->
    try_read_zip_end(Fd, 0).

try_read_zip_end(_Fd, I) when I > 512 ->
    {error, bad_eof_record};
try_read_zip_end(Fd, I) ->
    file:position(Fd, {eof,-22-I}),    
    case file:read(Fd, 22) of
	{ok,<<?ZIP_END_SIG:32/little, Rest/binary>>} ->
	    case Rest of
		<<Dsk:16/little,
		 CDsk:16/little,
		 NEnt:16/little,
		 TEnt:16/little,
		 Size:32/little,
		 Offs:32/little,
		 ZLen:16/little>> ->
		    {ok, #zip_end { sig = ?ZIP_END_SIG,
					dsk = Dsk,
					cdsk = CDsk,
					nent  = NEnt,
					tent  = TEnt,
					size  = Size,
					offset  = Offs,
					zlen  = ZLen }};
		_ ->
		    try_read_zip_end(Fd, I+1)
	    end
    end.


readn(_Fd, 0) -> {ok,<<>>};
readn(Fd, N) ->
    case file:read(Fd, N) of
	{ok,Bin} when size(Bin) == N ->
	    {ok,Bin};
	{ok,_} -> {error, too_short};
	Error -> Error
    end.
	    
    
    

