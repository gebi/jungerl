-module(esmb).
%%% --------------------------------------------------------------------
%%% File    : esmb.erl
%%% Created : 10 Dec 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : Implementation of the NetBIOS/SMB protocol.
%%%
%%% $Id$
%%% --------------------------------------------------------------------
-export([called_name/1, calling_name/1, ucase/1, lcase/1, check_dir/3,
	 connect/2, connect/3, connect/4, close/1, user_logon/3, emsg/3,
	 tree_connect/4, list_dir/3, open_file_ro/3, open_file_rw/3, 
	 read_file/3, client/2, client/3, mkdir/3, rmdir/3, 
	 close_file/2, write_file/4, delete_file/3]).
-export([dec_smb/1, tt_name/1]).
-export([zeros/1,p14/1,s16x/1,s21_lm_session_key/1,ex/2,swab/1,
	 challenge_response/2, crtest/0, e/2]).
-include("esmb_lib.hrl").

-define(PORT, 139).


%%% 
%%% Setup a socket, initiate an NBSS session and 
%%% negotiate the protocol dialect.
%%% 
connect(Caller, Called)           -> connect(Caller, Called, []).
connect(Caller, Called, SockOpts) -> connect(Caller, Called, SockOpts, ?PORT).

connect(Caller, Called, SockOpts, Port) ->
    Opts = [binary, {packet, 0}|SockOpts],
    case gen_tcp:connect(lcase(Called), Port, Opts) of
	{ok,S} ->
	    case nbss_session_request(S, Called, Caller) of
		{ok,_} ->
		    {ok, S, negotiate(S)};
		_ ->
		    {error, nbss_session_request}
	    end;
	Else ->
	    Else
    end.

close(S) ->
    gen_tcp:close(S).

client(Path, User)         -> esmb_client:start(Path, User).
client(Path, User, Wgroup) -> esmb_client:start(Path, User, Wgroup).
	    

-define(LT_BUFSIZE(Neg, Bin), 
	(Neg#smb_negotiate_res.max_buffer_size >= size(Bin))).

write_file(S, Neg, InReq, Finfo) ->
    write_file(S, Neg, InReq, Finfo, list_to_binary(Finfo#file_info.data), 0).

write_file(S, Neg, InReq, Finfo, Bin, Written) when ?LT_BUFSIZE(Neg,Bin) ->
    {Req, Pdu} = smb_write_andx_pdu(InReq, Finfo, Bin, Written),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	{ok, Wrote} ->
	    {ok, Wrote + Written};
	_ ->
	    {error, write_file}
    end;
write_file(S, Neg, InReq, Finfo, Bin, Written) ->
    {B1,B2} = split_binary(Bin, Neg#smb_negotiate_res.max_buffer_size),
    case write_file(S, Neg, InReq, Finfo, B1, Written) of
	{ok, Wrote} ->
	    write_file(S, Neg, InReq, Finfo, B2, Wrote);
	_ ->
	    {error, write_file} 
    end.
	

read_file(S, InReq, Finfo) ->
    read_file(S, InReq, Finfo, []).

-define(READ_ENOUGH(F), (F#file_info.size =< F#file_info.data_len)).
-define(MORE_TO_READ(F), (F#file_info.size > F#file_info.data_len)).

read_file(S, InReq, Finfo, Acc) when ?READ_ENOUGH(Finfo) ->
    {B, _} = split_binary(concat_binary(lists:reverse(Acc)), 
			  Finfo#file_info.size),
    {ok, B};
read_file(S, InReq, Finfo, Acc) when ?MORE_TO_READ(Finfo) ->
    {Req, Pdu} = smb_read_andx_pdu(InReq, Finfo),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	{ok, Res, Data} -> 
	    Dlen = Finfo#file_info.data_len + size(Data),
	    read_file(S, InReq, Finfo#file_info{data_len = Dlen}, [Data | Acc]);
	_ ->
	    {error, decoding_read_andx}
    end.
    

open_file_ro(S, InReq, Path) ->
    {Req, Pdu} = smb_open_file_ro_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

open_file_rw(S, InReq, Path) ->
    {Req, Pdu} = smb_open_file_rw_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

close_file(S, InReq) ->
    {Req, Pdu} = smb_close_file_pdu(InReq),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

mkdir(S, InReq, Path) ->
    {Req, Pdu} = smb_open_dir_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

rmdir(S, InReq, Path) ->
    {Req, Pdu} = smb_delete_dir_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).
    
delete_file(S, InReq, Path) ->
    {Req, Pdu} = smb_delete_file_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).
    

list_dir(S, InReq, Path) ->
    {Req, Pdu} = smb_trans2_find_first2_pdu(InReq, Path),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	X when X#find_result.eos == true -> 
	    X#find_result.finfo;
	X -> 
	    list_dir_cont(S, Req, Path, X#find_result.sid, X#find_result.finfo)
    end.

list_dir_cont(S, InReq, Path, Sid, Finfo) ->
    {Req, Pdu} = smb_trans2_find_next2_pdu(InReq, Path, Sid),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	X when X#find_result.eos == true -> 
	    Finfo ++ X#find_result.finfo;
	X -> 
	    list_dir_cont(S, Req, Path, Sid, Finfo ++ X#find_result.finfo)
    end.
    

check_dir(S, InReq, Path) ->
    {Req, Pdu} = smb_check_directory_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

tree_connect(S, Neg, InReq, Path) ->
    {Req, Pdu} = smb_tree_connect_andx_pdu(Neg, InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

negotiate(S) ->
    {Req, Pdu} = smb_negotiate_pdu(),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

user_logon(S, Neg, U) ->
    {Req, Pdu} = smb_session_setup_andx_pdu(Neg, U),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).


print_res(What, U, Res) ->
    io:format("~s~n"
	      "  User    = ~s~n"
	      "  Eclass  = ~p~n"
	      "  Ecode   = ~p~n"
	      "  Uid     = ~p~n"
	      "  Tid     = ~p~n"
	      "  Fid     = ~p~n"
	      "  Fsize   = ~p~n",
	      [What,
	       U#user.name,
	       Res#smbpdu.eclass,
	       Res#smbpdu.ecode,
	       Res#smbpdu.uid,
	       Res#smbpdu.tid,
	       Res#smbpdu.fid,
	       Res#smbpdu.file_size
	      ]).


%%% --------------------------------------------------------------------
%%% SMB decode routines
%%% --------------------------------------------------------------------

-define(IS(Req,Cmd), (Req#smbpdu.cmd == Cmd) ).
-define(SUB_CMD(Req, SubCmd), (Req#smbpdu.sub_cmd == SubCmd) ).

decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_NEGOTIATE) ->
    Res = dec_smb(Req, ResPdu),
    <<Di:16/little, B/binary>> = Res#smbpdu.wp,
    %%io:format("Dialect Index = ~w~n",[Di]),
    case Di of
	?PCNET_1_0 ->
	    #smb_negotiate_res{dialect_index = Di};
	?LANMAN_1_0 ->
	    crypto:start(),
	    lanman_neg_resp(B, Res#smbpdu.bf, 
			    #smb_negotiate_res{dialect_index = Di});
	_ ->
	    exit(nyi)
    end;
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_SESSION_SETUP_ANDX) ->
    dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_TREE_CONNECT_ANDX) ->
    dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_CHECK_DIRECTORY) ->
    dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_DELETE_DIRECTORY) ->
    dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_DELETE) ->
    dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_CLOSE) ->
    dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_NT_CREATE_ANDX) ->
    dec_nt_create_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_READ_ANDX) ->
    dec_read_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_WRITE_ANDX) ->
    dec_write_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) 
  when  ?IS(Req, ?SMB_COM_TRANSACTION2), 
	?SUB_CMD(Req, ?SMB_TRANS2_FIND_FIRST2) ->
    dec_trans2_find_x2(Req, ResPdu, ?SMB_TRANS2_FIND_FIRST2);
decode_smb_response(Req, {ok, _, ResPdu}) 
  when  ?IS(Req, ?SMB_COM_TRANSACTION2), 
	?SUB_CMD(Req, ?SMB_TRANS2_FIND_NEXT2) ->
    dec_trans2_find_x2(Req, ResPdu, ?SMB_TRANS2_FIND_NEXT2).


lanman_neg_resp(<<SecurityMode:16/little,
		  MaxBufferSize:16/little,
		  _:2/binary,            % Max pending multiplexed requests
		  _:2/binary,            % Max VCs between client and server
		  _:2/binary,            % Raw modes supported
		  _:4/binary,            % Unique token identifying this session
		  _:2/binary,            % Current time at server
		  _:2/binary,            % Current date at server
		  _:2/binary,            % Current time zone at server
		  EncKeyLen:16/little,   % Encryption key length
		  _:2/binary>>,          % reserved
		  B,
		Neg) ->
    <<EncKey:EncKeyLen/binary, _/binary>> = B,
    Neg#smb_negotiate_res{security_mode   = SecurityMode,
			  max_buffer_size = MaxBufferSize,
			  encryption_key  = EncKey}.
    


%%% ---

dec_write_andx(Req, Pdu) -> 
    Res = dec_smb(Req, Pdu),   
    <<?NoAndxCmd,              
    0,                         % reserved
    _:2/binary,                % offset to next command WC
    Count:16/little,           % Number of bytes written
    _:2/binary,                % Remaining (reserved)
    _:4/binary,                % reserved
    _/binary>> = Res#smbpdu.wp,
    {ok, Count}.
    

%%% ---

dec_read_andx(Req, Pdu) -> 
    Res = dec_smb(Req, Pdu),   
    <<?NoAndxCmd,              
    0,                         % reserved
    _:2/binary,                % offset to next command WC
    _:2/binary,                % Remaining (reserved) must be -1
    DcompactMode:16/little,    % ???
    0:16/little,               % reserved
    DataLength:16/little,      % Number of data bytes
    DataOffset:16/little,      % Offset (from header start) to data
    _:2/binary,                % High 16 bits of number of data bytes if
                               % CAP_LARGE_READEX; else zero
    _/binary>> = Res#smbpdu.wp,
    <<_:DataOffset/binary,Data:DataLength/binary>> = Pdu,
    {ok, Res, Data}.
    

%%% ---

dec_nt_create_andx(Req, Pdu) -> 
    Res = dec_smb(Req, Pdu),
    <<?NoAndxCmd,              
    0,                         % reserved
    _:2/binary,                % offset to next command WC
    OpLockLevel,               % The oplock level granted
    Fid:16/little,             % The file ID
    CreateAction:32/little,    % The action taken (1 = file opened)
    CreationTime:64/little,    % When file was created
    LastAccessTime:64/little,  % When file was accessed
    LastWriteTime:64/little,   % When file was last written
    ChangeTime:64/little,      % When file was last changed
    _:4/binary,                % The file attributes
    _:8/binary,                % Allocation size
    EOF:64/little,             % The file size 
    FileType:16/little,        % 0 = Disk file or directory
    _:2/binary,                % State of IPC device (e.g pipe)
    Directory,                 % Boolean (0 = Not a directory)
    _/binary>> = Res#smbpdu.wp,
    %% NB: The TIME comes in units of 100 nano seconds !!
    Gsec = (CreationTime div 10000000) + ?GREG_SEC_0_TO_1601,
    GDT = calendar:gregorian_seconds_to_datetime(Gsec),
    %%io:format("CREATION TIME: ~w~n", [GDT]),
    Asec = (LastAccessTime div 10000000) + ?GREG_SEC_0_TO_1601,
    ADT = calendar:gregorian_seconds_to_datetime(Asec),
    %%io:format("LAST ACCESS TIME: ~w~n", [ADT]),
    Wsec = (LastWriteTime div 10000000) + ?GREG_SEC_0_TO_1601,
    WDT = calendar:gregorian_seconds_to_datetime(Wsec),
    %%io:format("LAST WRITE TIME: ~w~n", [WDT]),
    Csec = (ChangeTime div 10000000) + ?GREG_SEC_0_TO_1601,
    CDT = calendar:gregorian_seconds_to_datetime(Csec),
    %%io:format("LAST CHANGE TIME: ~w~n", [CDT]),
    Res#smbpdu{fid       = Fid,
	       file_size = EOF}.
    
%%% ---

%%% Detta suger häst...
-define(RESPONSE_DATA(SubCmd,    
		      Pad,
		      Sid,
		      SearchCount,
		      EndOfSearch,
		      EaErrorOffset,
		      LastNameOffset,
		      Buffer),
	if (SubCmd == ?SMB_TRANS2_FIND_FIRST2) ->
		<<Pad,
		Sid:16/little,            % Search handle
		SearchCount:16/little,    % # of entries returned
		EndOfSearch:16/little,    % Was last entry returned ?
		EaErrorOffset:16/little,  % Offset into EA list if EA error
		LastNameOffset:16/little, % Offset into Data holding the filename
		%%                          of the last entry, if server needs it 
		%%                          to resume search; else 0
		_/binary>> = Buffer;
	   true -> 
		%% SubCmd == ?SMB_TRANS2_FIND_NEXT2
		Sid = 0,
		<<Pad,
		SearchCount:16/little,    % # of entries returned
		EndOfSearch:16/little,    % Was last entry returned ?
		EaErrorOffset:16/little,  % Offset into EA list if EA error
		LastNameOffset:16/little, % Offset into Data holding the filename
		%%                          of the last entry, if server needs it 
		%%                          to resume search; else 0
		_/binary>> = Buffer
	end).
		
dec_trans2_find_x2(Req, Pdu, SubCmd) ->
    Res = dec_smb(Req, Pdu),
    <<TotParamCount:16/little,
    TotDataCount:16/little,
    0:16/little,
    ParamCount:16/little,
    ParamOffset:16/little,
    ParamDisplacement:16/little,
    DataCount:16/little,
    DataOffset:16/little,
    DataDisplacement:16/little,
    _/binary>> = Res#smbpdu.wp,
    %%
    SMBheaderLen = ?SMB_HEADER_LEN,
    Offset = DataOffset - (SMBheaderLen + (Res#smbpdu.wc * 2) + ParamCount),
    ?RESPONSE_DATA(SubCmd,    
		   Pad,
		   Sid,
		   SearchCount,
		   EndOfSearch,
		   EaErrorOffset,
		   LastNameOffset,
		   Res#smbpdu.bf),
    <<_:DataOffset/unit:8, Data/binary>> = Pdu,
    %%io:format("dec_trans2_find_first2: ~n",[]),
    %%io:format("  Data offset   = ~p~n",[DataOffset]),
    %%io:format("  Search handle = ~p~n",[Sid]),
    %%io:format("  No.of entries = ~p~n",[SearchCount]),
    %%io:format("  EOF search ?  = ~p~n",[EndOfSearch]),
    %%io:format("  LastNameOset  = ~p~n",[LastNameOffset]),
    Finfo = dec_info_standard(Data, SearchCount),
    #find_result{sid = Sid, eos = to_bool(EndOfSearch), finfo = Finfo}.

%%% ---

dec_info_standard(Data, Max) ->
    dec_info_standard(Data, Max, 1).

dec_info_standard(<<_:4/binary, DT:12/binary, Size:32/little,
		  _:4/binary, Attr:16/little, Len, 
		  Rest0/binary>>, Max, I) when I<Max->
    <<Filename:Len/binary, _Null, Rest/binary>> = Rest0,
    F = #file_info{name = Filename,
		   size = Size,
		   attr = Attr,
		   date_time = dec_dt_info_std(DT)},
    [F | dec_info_standard(Rest, Max, I+1)];
dec_info_standard(<<Rkey:4/binary, DT:12/binary, Size:32/little,
		  _:4/binary, Attr:16/little, Len, 
		  Rest0/binary>>, Max, Max) ->
    <<Filename:Len/binary, _Null, Rest/binary>> = Rest0,
    F = #file_info{name = Filename,
		   size = Size,
		   attr = Attr,
		   date_time = dec_dt_info_std(DT),
		   resume_key = Rkey},
    [F];
dec_info_standard(Data, Max, I) ->
    io:format("ERROR: Missing file info I=~p~n",[I]),
    sleep(2),
    io:format("FLUSH: ~p~n",[receive X -> X after 0 -> false end]),
    [].

%%%
%%% DOS - Date/Time format
%%% Year has a range 0-119 which represents 1980-2099
%%% Twoseconds has a range 0-29 representing two seconds increment
%%%
dec_dt_info_std(<<CreationDate:2/binary, CreationTime:2/binary,
		  LastAccessDate:2/binary, LastAccessTime:2/binary,
		  LastWriteDate:2/binary, LastWriteTime:2/binary>>) ->
    <<Yc:7, Mc:4, Dc:5>> = swap(CreationDate),
    <<Hc:5, Ic:6, Tc:5>> = swap(CreationTime),
    <<Ya:7, Ma:4, Da:5>> = swap(LastAccessDate),
    <<Ha:5, Ia:6, Ta:5>> = swap(LastAccessTime),
    <<Yw:7, Mw:4, Dw:5>> = swap(LastWriteDate),
    <<Hw:5, Iw:6, Tw:5>> = swap(LastWriteTime),
    #dt{creation_date    = {Yc + 1980, Mc, Dc},
	creation_time    = {Hc, Ic, Tc * 2},
	last_access_date = {Ya + 1980, Ma, Da},
	last_access_time = {Ha, Ia, Ta * 2},
	last_write_date  = {Yw + 1980, Mw, Dw},
	last_write_time  = {Hw, Iw, Tw * 2}}.

swap(<<X,Y>>) -> <<Y,X>>.
		
%%% ---    


dec_smb(Pdu) ->
    dec_smb(#smbpdu{}, Pdu).

dec_smb(Req, 
	<<16#FF, $S, $M, $B,          % smb-header
	Cmd,
	Eclass, 
	_,                            % zero (not used)
	Ecode:16/little,    
	Flags, 					%
	Flags2:16/little,
	_:12/unit:8,                  % Pad (12 bytes)
	Tid:16/little, 
	Pid:16/little, 
	Uid:16/little,
	Mid:16/little,
	Wc,
	Rest/binary>>) ->
    <<Wp:Wc/binary-unit:16, Bc:16/little, Bf/binary>> = Rest,
    Req#smbpdu{cmd    = Cmd,
	       eclass = Eclass,
	       ecode  = Ecode,
	       flags  = Flags,
	       flags2 = Flags2,
	       tid    = Tid,
	       pid    = Pid,
	       uid    = Uid,
	       mid    = Mid,
	       wc     = Wc,
	       wp     = Wp,
	       bc     = Bc,
	       bf     = Bf}.
    
	
%%% --------------------------------------------------------------------
%%% SMB encode routines
%%% --------------------------------------------------------------------

%%% FIXME : better test when new dialects are added
-define(CORE_PROTOCOL(Neg), (Neg#smb_negotiate_res.dialect_index == 0)).
-define(PRE_DOS_LANMAN_2_1(Neg), (Neg#smb_negotiate_res.dialect_index =< 1)).


smb_close_file_pdu(InReq) ->
    {Wc,Wp} = wp_close_file(InReq#smbpdu.fid),
    Rec = #smbpdu{cmd = ?SMB_CLOSE,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc = Wc,
		  wp = Wp},
    %%io:format("close_file: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("close_file: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_close_file(Fid) ->
    {3,
     <<Fid:16/little,
     0:32/little>>}.     % Time of last write (set by local system)


%%% ---

smb_delete_file_pdu(InReq, Fname) ->
    {Wc,Wp} = wp_delete_file(),
    Bf = bf_delete_file(Fname),
    Rec = #smbpdu{cmd = ?SMB_DELETE,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    %%io:format("delete_file: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("delete_file: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_delete_file() ->
    {1,
     <<0:16/little>>}.   % SearchAttributes

bf_delete_file(Fname) ->
    list_to_binary([?BUF_FMT_ASCII,   % Buffer format
		    Fname,[0]]).      % Filename 

%%% ---

smb_delete_dir_pdu(InReq, Dir) ->
    Bf = bf_delete_directory(Dir),
    Rec = #smbpdu{cmd = ?SMB_DELETE_DIRECTORY,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  bc  = size(Bf),
		  bf  = Bf},
    %%io:format("delete_directory: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("delete_directory: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

bf_delete_directory(Dir) ->
    list_to_binary([?BUF_FMT_ASCII,   % Buffer format
		    Dir,[0]]).        % Dir path

%%% ---

smb_write_andx_pdu(InReq, Finfo, Data, Written) ->
    DataLen = size(Data),
    {Wc,Wp} = wp_write_andx(InReq#smbpdu.fid, Written, DataLen),
    Rec = #smbpdu{cmd = ?SMB_WRITE_ANDX,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc = Wc,
		  wp = Wp,
		  bc = DataLen,
		  bf = Data},  
    %%io:format("write_andx: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("write_andx: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_write_andx(Fid, Written, Dlen) ->
    Remaining = 0,
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount 
    %%             = ?SMB_HEADER_LEN + 1 + 12*2 + 2
    %%             = ?SMB_HEADER_LEN + 27
    Offset = ?SMB_HEADER_LEN + 27,
    FileOffset = Written,
    {12,                        % (ThisLen)
     <<?NoAndxCmd,              
     0,                         % reserved
     0:16/little,               % offset to next command WC
     Fid:16/little,             % File handle
     FileOffset:32/little,      % Offset in file to begin write
     0:32/little,               % reserved
     0:16/little,               % Write mode: 0 = write through
     Remaining:16/little,       % Bytes remaining to satisfy request
     0:16/little,               % DataLengthHigh, zero if not CAP_LARGE_WRITEX
     Dlen:16/little,            % Number of data bytes in buffer (>=0)
     Offset:16/little>>}.       % Offset to data bytes


%%% ---

smb_read_andx_pdu(InReq, Finfo) ->
    {Wc,Wp} = wp_read_andx(InReq#smbpdu.fid, Finfo#file_info.data_len),
    Rec = #smbpdu{cmd = ?SMB_READ_ANDX,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc = Wc,
		  wp = Wp},
    %%io:format("read_andx: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("read_andx: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_read_andx(Fid, Offset) ->
    {10,
     <<?NoAndxCmd,              
     0,                         % reserved
     0:16/little,               % offset to next command WC
     Fid:16/little,             % File handle
     Offset:32/little,          % Offset in file to begin read
     4096:16/little,            % Max number of bytes to return
     4096:16/little,            % Reserved for obsolescent requests
     0:32/little,               % High 16 buts of MaxXount if CAP_LARGE READX;
                                % else must be zero
     0:16/little>>}.            % Remaining, obsolescent requests

%%% ---

-record(candx, {
	  type = file,     % file | dir
	  mode = ro}).     % ro | rw
	  

smb_open_file_ro_pdu(InReq, Path) ->
    smb_nt_create_andx_pdu(InReq, Path, #candx{type=file,mode=ro}).

smb_open_file_rw_pdu(InReq, Path) ->
    smb_nt_create_andx_pdu(InReq, Path, #candx{type=file,mode=rw}).

smb_open_dir_pdu(InReq, Path) ->
    smb_nt_create_andx_pdu(InReq, Path, #candx{type=dir}).


smb_nt_create_andx_pdu(InReq, Path, Opts) ->
    {Wc,Wp} = wp_nt_create_andx(length(Path), Opts),
    Bf = bf_nt_create_andx(Path),
    Rec = #smbpdu{cmd = ?SMB_NT_CREATE_ANDX,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    %%io:format("nt_create_andx: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("nt_create_andx: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_nt_create_andx(NameLen, Opts) ->
    OpLock = oplock(Opts),
    AccessMask = access_mask(Opts),
    FileAttrs = file_attributes(Opts),
    ShareAccess = share_access(Opts),
    CreateOptions = create_options(Opts),
    CreateDisp = create_dispositions(Opts),
    {24,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      0,                         % reserved
      NameLen:16/little,         % Length of Name[] in bytes
      OpLock:32/little,          % Oplock Flags
      0:32/little,               % RootDirectoryFid
      AccessMask:32/little,        % Desired access
      0:32/little,               % Allocaton size MSW
      0:32/little,               %                LSW (64 bit)
      FileAttrs:32/little,       % File attributes
      ShareAccess:32/little,     % Type of share access
      CreateDisp:32/little,      % Create disposition
      CreateOptions:32/little,   % Create options
      ?SECURITY_IDENTIFICATION:32/little, % Security QOS info (?)
      0>>}.                      % Security tracking mode flag (?)


bf_nt_create_andx(Name) ->
    list_to_binary([Name, [0]]).% filename


oplock(_) -> ?NO_OPLOCK.

access_mask(X) when X#candx.mode == ro -> ?AM_READ;
access_mask(X) when X#candx.mode == rw -> ?AM_READ bor ?AM_WRITE.

file_attributes(X) when X#candx.type == file -> ?FILE_ATTR_NORMAL;
file_attributes(X) when X#candx.type == dir  -> ?FILE_ATTR_DIR.

share_access(X) when X#candx.type == file -> ?FILE_SHARE_READ;
share_access(X) when X#candx.type == dir  -> ?FILE_SHARE_RW.

create_dispositions(X) when X#candx.type == file,
			    X#candx.mode == rw   -> ?FILE_OPEN_IF;
create_dispositions(X) when X#candx.type == file -> ?FILE_OPEN;
create_dispositions(X) when X#candx.type == dir  -> ?FILE_CREATE.

create_options(X) when X#candx.type == file -> ?FILE_OPEN_OPTIONS;
create_options(X) when X#candx.type == dir  -> ?DIR_OPEN_OPTIONS.


%%% ---

smb_trans2_find_next2_pdu(InReq, Path, Sid) ->
    {Wc,Wp} = wp_trans2_find_x2(?SMB_TRANS2_FIND_NEXT2, length(Path)),
    Bf = bf_trans2_find_next2(Path, Sid),
    Rec = #smbpdu{cmd = ?SMB_COM_TRANSACTION2,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc  = Wc,
		  wp  = Wp,
		  bc  = size(Bf),
		  bf  = Bf,
		  sub_cmd = ?SMB_TRANS2_FIND_NEXT2},
    %%io:format("trans2_find_next2: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("trans2_find_next2: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

bf_trans2_find_next2(Path, Sid) ->
    %% Flags ::= DoNotClose,CloseAtEndOfSearch,ReturnResumeKey,ContinueSearch,NoBackupIntent
    %% ContinueSearch,Resume,CloseAtEOS,
    Flags = <<16#000e:16/little>>,
    list_to_binary([0,           % Must be null
		    0,0,         % Pad to SHORT or LONG
		    %% --- Start of Parameter Block (12 bytes) ---
		    <<Sid:16/little>>, % Search attribute or SID
		    <<512:16/little>>, % Max # of entries returned
		    <<?SMB_INFO_STANDARD:16/little>>, % What info to return in the result
		    <<0:32/little>>, % Resume key
		    Flags,       % Flags
		    Path,[0]     % Search pattern
		    ]).

%%% ---

smb_trans2_find_first2_pdu(InReq, Path) ->
    {Wc,Wp} = wp_trans2_find_x2(?SMB_TRANS2_FIND_FIRST2, length(Path)),
    Bf = bf_trans2_find_first2(Path),
    Rec = #smbpdu{cmd = ?SMB_COM_TRANSACTION2,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  wc  = Wc,
		  wp  = Wp,
		  bc  = size(Bf),
		  bf  = Bf,
		  sub_cmd = ?SMB_TRANS2_FIND_FIRST2},
    %%io:format("trans2_find_first2: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("trans2_find_first2: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.


wp_trans2_find_x2(SubCmd, PathLen) ->
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount + Pad
    %%             = ?SMB_HEADER_LEN + 30 + 1 + 2 + 3
    %%             = ?SMB_HEADER_LEN + 36
    %% ParamLen    = ParameterBytesLen + PathLen + NullByte
    %%             = 12 + PathLen + 1
    %% DataOffset  = ParamOffset + ParamLen + NullByte
    %%             = ParamOffset + ParamLen
    ParamOffset = ?SMB_HEADER_LEN + 36,
    ParamLen    = 13 + PathLen,
    DataOffset  = ParamOffset + ParamLen,
    ParamCount  = DataOffset - ParamOffset,
    {15,                         % WordCount = 14 + SetupCount
     <<ParamCount:16/little,     % Total parameter bytes sent
      0:16/little,               % Total data bytes sent
      10:16/little,              % Max parameter bytes to return
      1024:16/little,            % Max data bytes to return
      0,                         % Max setup words to return
      0,                         % reserved
      0:16/little,               % Flags
      0:32/little,               % timeout , 0 = return immediately
      0:16/little,               % reserved2
      ParamLen:16/little,        % Parameter bytes sent this buffer
      ParamOffset:16/little,     % Offset (from header start) to parameters
      0:16/little,               % Data bytes sent this buffer
      DataOffset:16/little,      % Offset (from header start) to data
      1,                         % Count of setup words
      0,                         % reserved3 (pad above to word boundary)
      SubCmd:16/little           % Setup words (# = Setup word count)
      >>}.

bf_trans2_find_first2(Path) ->
    %% Sa ::= Archive,Directory,Volume,System,Hidden,ReadOnly
    %% Include: Hidden, System and Directory in search result
    Sa = <<16#0016:16/little>>,
    %% Flags ::= DoNotClose,CloseAtEndOfSearch,ReturnResumeKey,ContinueSearch,NoBackupIntent
    %% Resume,CloseAtEOS
    Flags = <<16#0006:16/little>>,
    list_to_binary([0,           % Must be null
		    0,0,         % Pad to SHORT or LONG
		    %% --- Start of Parameter Block (12 bytes) ---
		    Sa,          % Search attribute
		    <<512:16/little>>, % Max # of entries returned
		    Flags,       % Flags
		    <<?SMB_INFO_STANDARD:16/little>>, % What info to return in the result
		    <<0:32/little>>, % Storage type
		    Path,[0]     % Search pattern
		    ]).

%%% ---

smb_check_directory_pdu(InReq, Path) ->
    Bf = bf_check_directory(Path),
    Rec = #smbpdu{cmd = ?SMB_CHECK_DIRECTORY,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  tid = InReq#smbpdu.tid,
		  bc  = size(Bf),
		  bf  = Bf},
    %%io:format("check_directory: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("check_directory: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

bf_check_directory(Path) ->
    list_to_binary([?BUF_FMT_ASCII,   % Buffer format
		    Path,[0]]).       % Dir path

%%% ---
    
smb_tree_connect_andx_pdu(Neg, InReq, Path) ->
    {Wc,Wp} = wp_tree_connect_andx(Neg),
    Bf = bf_tree_connect_andx(Neg, Path),
    Rec = #smbpdu{cmd = ?SMB_TREE_CONNECT_ANDX,
		  pid = InReq#smbpdu.pid,
		  uid = InReq#smbpdu.uid,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    %%io:format("tree_connect_andx: WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("tree_connect_andx: ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_tree_connect_andx(Neg)  when ?PRE_DOS_LANMAN_2_1(Neg) ->
    {4,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      0:16/little,               % Flags
      1:16/little>>}.            % PasswordLength (incl. NULL)

bf_tree_connect_andx(Neg, Path) when ?PRE_DOS_LANMAN_2_1(Neg) ->
    list_to_binary([0,           % Password
		    Path, [0],   % filesystem
		    "A:",[0]]).  % service
    

%%% ---

smb_session_setup_andx_pdu(Neg, U) ->
    {Passwd, PwLen} = enc_passwd(Neg, U#user.pw),
    {Wc,Wp} = wp_session_setup_andx(Neg, U, PwLen),
    Bf = bf_session_setup_andx(Neg, U, Passwd),
    Rec = #smbpdu{cmd = ?SMB_SESSION_SETUP_ANDX,
		  pid = mypid(),
		  mid = 1,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    %%io:format("WordCount = ~p~n",[Rec#smbpdu.wc]),
    %%io:format("ByteCount = ~p~n",[Rec#smbpdu.bc]),
    {Rec, enc_smb(Rec)}.

wp_session_setup_andx(Neg, U, PwLen) ->
    {10,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      ?MaxBufferSize:16/little,
      ?MaxMpxCount:16/little,   
      ?VcNumber:16/little,
      0:32/little,               % session key
      PwLen:16/little,
      0:32/little>>}.            % reserved
    
bf_session_setup_andx(Neg,U, Passwd) ->
    list_to_binary([Passwd,
		    U#user.name,[0],
		    U#user.primary_domain,[0],
		    U#user.native_os,[0],
		    U#user.native_lanman,[0]]).

enc_passwd(Neg, Passwd) when ?CORE_PROTOCOL(Neg) ->
    {Passwd, length(Passwd)};
enc_passwd(Neg, Passwd) when ?PRE_DOS_LANMAN_2_1(Neg) ->
    EncKey = Neg#smb_negotiate_res.encryption_key,
    EncPasswd = challenge_response(Passwd, EncKey),
    {EncPasswd, size(EncPasswd)}.

%%%
%%% TEST: smbclient //korp/tobbe -U tobbe -m LANMAN1
%%%
%%% EncKey = 1cb2c4dc19d52588
%%%
%%% Passwd = qwe123
%%%
%%% Response = b6c89e28077ada40648149220da0ca5c9f5aa481a3f88467
%%%

crtest() ->
    EncKey = <<16#1c,16#b2,16#c4,16#dc,16#19,16#d5,16#25,16#88>>,
    Resp = challenge_response(<<"qwe123">>, EncKey),
    {Resp == test_response(),
     Resp,
     test_response()}.

test_response() ->
    <<16#b6,16#c8,16#9e,16#28,16#07,16#7a,16#da,16#40,16#64,
      16#81,16#49,16#22,16#0d,16#a0,16#ca,16#5c,16#9f,16#5a,
       16#a4,16#81,16#a3,16#f8,16#84,16#67>>.
    

challenge_response(Passwd, Challenge) when binary(Passwd) -> 
    %%io:format("s21_lm_session_key(Passwd)=~p~n",[s21_lm_session_key(Passwd)]),
    ex(s21_lm_session_key(Passwd), Challenge);
challenge_response(Passwd, Challenge) when list(Passwd) -> 
    challenge_response(list_to_binary(Passwd), Challenge).

%%    ex(s21_nt_session_key(Passwd), Challenge).

ex(<<K0:7/binary,K1:7/binary>>, Data) when size(Data) == 8 ->
    concat_binary([e(K0, Data),
		   e(K1, Data)]);
ex(<<K0:7/binary,K1:7/binary,K2:7/binary>>, Data) when size(Data) == 8 ->
    concat_binary([e(K0, Data),
		   e(K1, Data),
		   e(K2, Data)]);
ex(K, D)  ->
    io:format("<FATAL ERROR>: K=~p~nD=~p~n",[K,D]),
    exit("fatal_error").

e(K,D) -> 
    %%io:format("crypto:des_cbc_encrypt(~p, 0, ~p) ~n", [K,D]),
    B = crypto:des_cbc_encrypt(s2k(K), null_vector(), D).

null_vector() -> <<0,0,0,0,0,0,0,0>>.

s21_lm_session_key(Passwd) -> 
    S16X  = s16x(Passwd),
    Zero5 = zeros(5),
    <<S16X/binary, Zero5/binary>>.

s21_nt_session_key(Passwd) -> 
    md4:start_link(),
    {ok, S16}  = md4:digest(Passwd),
    Zero5 = zeros(5),
    <<S16/binary, Zero5/binary>>.

%%%
%%% What does this function do ??
%%% According to the CIFS spec we should do
%%% bit reverse on each byte. But instead we
%%% do this...
%%%
%%% See libsmb/smbdes.c str_to_key(Str,Key)
%%%
s2k(<<S0,S1,S2,S3,S4,S5,S6>>) ->
    K0 = S0 bsr 1,
    K1 = ((S0 band 16#01) bsl 6) bor (S1 bsr 2),
    K2 = ((S1 band 16#03) bsl 5) bor (S2 bsr 3),
    K3 = ((S2 band 16#07) bsl 4) bor (S3 bsr 4),
    K4 = ((S3 band 16#0F) bsl 3) bor (S4 bsr 5),
    K5 = ((S4 band 16#1F) bsl 2) bor (S5 bsr 6),
    K6 = ((S5 band 16#3F) bsl 1) bor (S6 bsr 7),
    K7 = S6 band 16#7F,
    list_to_binary([X bsl 1 || X <- [K0,K1,K2,K3,K4,K5,K6,K7]]);
s2k(<<B0:7/binary,B1:7/binary>>) ->
    concat_binary([s2k(B0),s2k(B1)]).
    

s16x(Passwd) -> 
    %%io:format("====== ~p b(~p)~n", [p14(Passwd), swab(p14(Passwd))]),
    %%io:format("------ ~p b(~p)~n", [p14(Passwd), s2k(p14(Passwd))]),
    ex(p14(Passwd), n8()).

p14(Passwd) when size(Passwd) =< 14 ->
    Upasswd = list_to_binary(ucase(binary_to_list(Passwd))),
    Zeros = zeros(14 - size(Passwd)),
    <<Upasswd/binary, Zeros/binary>>.

n8() -> <<16#4b,16#47,16#53,16#21,16#40,16#23,16#24,16#25>>.

zeros(N) ->
    list_to_binary(zerosN(N)).

zerosN(0)          -> [];
zerosN(N) when N>0 -> [0 | zerosN(N-1)].

%%%
%%% Return a sequence of bytes where each byte
%%% has reversed its bit pattern.
%%%
swab(B) when binary(B) -> 
    list_to_binary(swab(binary_to_list(B)));
swab(L) when list(L)   -> 
    F = fun(X) -> bit_rev(X) end,
    lists:map(F, L).

bit_rev(N) when N < 256, N >= 0 ->
    <<B0:1,B1:1,B2:1,B3:1,B4:1,B5:1,B6:1,B7:1>> = <<N>> ,
    <<Rev>> = <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1>> ,
    Rev.

%%% ---

smb_negotiate_pdu() ->
    Bf = dialects(),
    Rec = #smbpdu{cmd = ?SMB_NEGOTIATE,
		  pid = mypid(),
		  mid = 1,
		  bc  = size(Bf),
		  bf  = Bf},
    {Rec, enc_smb(Rec)}.

enc_smb(Pdu) ->
    Cmd    = Pdu#smbpdu.cmd,
    Eclass = Pdu#smbpdu.eclass,
    Ecode  = Pdu#smbpdu.ecode,
    Flags  = Pdu#smbpdu.flags,
    Flags2 = Pdu#smbpdu.flags2,
    Tid    = Pdu#smbpdu.tid,
    Pid    = Pdu#smbpdu.pid,
    Uid    = Pdu#smbpdu.uid,
    Mid    = Pdu#smbpdu.mid,
    Wc     = Pdu#smbpdu.wc,
    Wp     = Pdu#smbpdu.wp,
    Bc     = Pdu#smbpdu.bc,
    Bf     = Pdu#smbpdu.bf,
    <<16#FF, $S, $M, $B,          % smb-header
     Cmd,
     Eclass, 
     0,                           % zero (not used)
     Ecode:16/little,    
     Flags, 
     Flags2:16/little,
     0:12/unit:8,                 % Pad (12 bytes)
     Tid:16/little, 
     Pid:16/little, 
     Uid:16/little,
     Mid:16/little,
     Wc, 
     Wp/binary,
     Bc:16/little, 
     Bf/binary>>. 


%%% Until we get DES support we can't negotiate any
%%% other dialect version !!
dialects() ->
    %%<<?BUF_FMT_DIALECT, <<"PC NETWORK PROGRAM 1.0">>/binary, 0>>.
    <<?BUF_FMT_DIALECT, <<"PC NETWORK PROGRAM 1.0">>/binary, 0,
      ?BUF_FMT_DIALECT, <<"LANMAN1.0">>/binary, 0>>.

    
mypid() ->
    [_,Pid,_] = string:tokens(pid_to_list(self()),"."),
    list_to_integer(Pid).


%%% --------------------------------------------------------------------
%%% NetBIOS code
%%% --------------------------------------------------------------------

nbss_session_request(S, Called, Calling) ->
    send_recv(S, nbss_session_request_pdu(Called, Calling)).

nbss_session_service(S, SMB_pdu) ->
    %%io:format("SMB_pdu: ~p~n",[SMB_pdu]),
    send_recv(S, nbss_session_service_pdu(SMB_pdu)).

send_recv(S, Packet) ->
    gen_tcp:send(S,[Packet]),
    recv(S).

recv(S) ->
    receive 
	{tcp,S,Bin} ->
	    case dec_msg(Bin) of
		{ok, ?SESSION_KEEP_ALIVE} ->
		    io:format("recv: got KEEP_ALIVE~n", []),
		    recv(S);
		Else ->
		    Else
	    end
    end.

dec_msg(<<?POSITIVE_SESSION_RESPONSE,Flags,Length:16>>) ->
    %%io:format("Got: Positive-Session-Response~n",[]),
    {ok, ?POSITIVE_SESSION_RESPONSE};
dec_msg(<<?SESSION_SERVICE, _, Length:16, SMB_pdu/binary>>) ->
    %%io:format("Got: Sessio-Service msg, len=~w, size=~w~n",
    %%[Length, size(SMB_pdu)]),
    {ok, ?SESSION_SERVICE, get_more(Length, size(SMB_pdu), [SMB_pdu])};
dec_msg(<<?SESSION_KEEP_ALIVE, _/binary>>) ->
    {ok, ?SESSION_KEEP_ALIVE};
dec_msg(Bin) ->
    io:format("Got: ~p~n",[Bin]),
    {error, Bin}.

get_more(Expected, Got, Bins) when Got < Expected ->
    receive 
	{tcp,_,Bin} ->
	    get_more(Expected, Got + size(Bin), [Bin | Bins])
    end;
get_more(_, _, Bins) ->
    concat_binary(lists:reverse(Bins)).


nbss_session_request_pdu(Called, Calling) ->
    CalledName = called_name(Called),
    CallingName = calling_name(Calling) ,
    Length = size(CalledName) + size(CallingName),
    <<?SESSION_REQUEST, 0, Length:16, CalledName/binary, CallingName/binary>>.

nbss_session_service_pdu(SMB_pdu) when binary(SMB_pdu) ->
    Length = size(SMB_pdu),
    <<?SESSION_SERVICE, 0, Length:16, SMB_pdu/binary>>.


%%% The NetBIOS naming convention allows for 16 character in a 
%%% NetBIOS name. Microsoft, however, limits NetBIOS names to 15
%%% characters and uses the 16th character as a NetBIOS suffix in
%%% order to identify functionality installed on the registered device.

-define(NETBIOS_NAME_LEN, 15).
-define(NETBIOS_SX_WORKSTATION,   16#00).  % Workstation service
-define(NETBIOS_SX_FILESERVER,    16#20).  % File server service

called_name(Name) when length(Name) =< ?NETBIOS_NAME_LEN -> 
    nb_name(Name, ?NETBIOS_SX_FILESERVER).

calling_name(Name) when length(Name) =< ?NETBIOS_NAME_LEN -> 
    nb_name(Name, ?NETBIOS_SX_WORKSTATION).

nb_name(Name, Sx) ->
    Len = 32,
    list_to_binary([Len | l1enc(Name, Sx, 0)]).

%%% test routine
tt_name(Name) ->
    l1enc(Name, $A, 0).

-define(SPACE, 16#20).

l1enc([H|T], Sx, N) when N < ?NETBIOS_NAME_LEN ->
    [l1msn(H),l1lsn(H)|l1enc(T, Sx, N+1)];
l1enc([], Sx, N) when N < ?NETBIOS_NAME_LEN ->
    [l1msn(?SPACE),l1lsn(?SPACE)|l1enc([], Sx, N+1)];
l1enc([], Sx, ?NETBIOS_NAME_LEN) ->
    [l1msn(Sx),l1lsn(Sx),0].

%%% Level 1 encoding, get most significant nibble
l1msn(B) -> (B bsr 4) + $A.

%%% Level 1 encoding, get least significant nibble
l1lsn(B) -> (B band 16#0F) + $A.



ucase([C|Cs]) when C>=$a,C=<$z -> [C-32|ucase(Cs)]; % a-z
ucase([C|Cs])                  -> [C|ucase(Cs)];
ucase([])                      -> [].

lcase([C|Cs]) when C>=$A,C=<$Z -> [C+32|lcase(Cs)]; % A-Z
lcase([C|Cs])                  -> [C|lcase(Cs)];
lcase([])                      -> [].

to_bool(0) -> false;
to_bool(_) -> true.

hexprint(L) ->
    F = fun(H, Acc) ->
		%%io:format("~c~.16B",[Acc,H]),
		io:format("~c~s",[Acc,i2x(H)]),
		$,
	end,
    lists:foldl(F, $[, L),
    io:format("]~n",[]).

i2x(I) when I > 15 -> [x(I div 16),x(I rem 16)];
i2x(I)	           -> [$0,x(I)].

x(X) when X>=0,X=<9 -> X + $0;
x(10) -> $a;
x(11) -> $b;
x(12) -> $c;
x(13) -> $d;
x(14) -> $e;
x(15) -> $f.


sleep(Sec) ->
    receive after Sec*1000 -> true end.

%%%
%%% Error messages
%%%
emsg(Eclass, Ecode, DefaultEmsg) ->
    case catch emsg(Eclass, Ecode) of
	{'EXIT', _} -> DefaultEmsg;
	Emsg        -> Emsg
    end.

%%% See p.118 in CIFS/1.0 doc.
emsg(?ERRDOS, ?ERRbadfunc)  -> "Invalid function";
emsg(?ERRDOS, ?ERRbadfile)  -> "File not found";
emsg(?ERRDOS, ?ERRbadpath)  -> "Directory invalid";
emsg(?ERRDOS, ?ERRnofids)   -> "Too many open files";
emsg(?ERRDOS, ?ERRnoaccess) -> "Access denied".
