%%%----------------------------------------------------------------------
%%% File    : nfs_server.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Extensible NFS v2 (RFC 1094) server core
%%% Created : 22 Jun 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(nfs_server).
-author('luke@bluetail.com').

-behaviour(gen_server).

-include("nfs.hrl").

%% External exports
-export([start_link/0, add_mountpoint/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MOUNTD_PORT, 22050).		% arbitrary
-define(NFS_PORT, 22049).		% normal port + 20000

%% NFS identifies files by a "file handle", which is a fixed-length
%% opaque binary. This program's file handles look like this:
%%
%%   <<FileID:32, FilesystemID:32, _Junk/binary>>
%%
%% We have bi-directional mappings to identify which erlang module
%% implements each file system, and which term represents each file
%% id.

%% These tables are mappings. fh_id_tab maps file handles onto
%% identifying terms, etc.
-define(fh_id_tab, nfs_fh_id).
-define(id_fh_tab, nfs_id_fh).
-define(mod_fsid_tab, nfs_mod_fsid).
-define(fsid_mod_tab, nfs_fsid_mod).
-define(misc_tab, nfs_misc).

%% fattr modes
-define(MODE_DIR,     8#0040000).
-define(MODE_CHAR,    8#0020000).
-define(MODE_BLOCK,   8#0060000).
-define(MODE_REGULAR, 8#0100000).
-define(MODE_SYMLINK, 8#0120000).
-define(MODE_SOCKET,  8#0140000).
-define(MODE_SETUID,  8#0004000).
-define(MODE_SETGID,  8#0002000).
-define(MODE_SV_SWAP, 8#0001000).	% "Save swapped text even after use."
-define(MODE_UR,      8#0000400).
-define(MODE_UW,      8#0000200).
-define(MODE_UX,      8#0000100).
-define(MODE_GR,      8#0000040).
-define(MODE_GW,      8#0000020).
-define(MODE_GX,      8#0000010).
-define(MODE_OR,      8#0000004).
-define(MODE_OW,      8#0000002).
-define(MODE_OX,      8#0000001).

-record(state, {mountpoints		% dict: path -> module
	       }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, nfs_server}, nfs_server, [], []).

add_mountpoint(Path, Module) ->
    gen_server:call(?MODULE, {add_mountpoint, Path, Module}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    start_mountd(),
    start_nfsd(),
    init_tabs(),
    {ok, #state{mountpoints=dict:new()}}.

start_mountd() ->
    {ok, Pid} = rpc_server:start_link({local, nfs_mountd},
				      [{udp, any, ?MOUNTD_PORT, false, []}],
				      ?MOUNTPROG,
				      mountprog,
				      ?MOUNTVERS,
				      ?MOUNTVERS,
				      nfs_svc,
				      do_init).

start_nfsd() ->
    {ok, Pid} = rpc_server:start_link({local, nfs_rpc_nfsd},
				      [{udp, any, ?NFS_PORT, false, []}],
				      ?NFS_PROGRAM,
				      nfs_program,
				      ?NFS_VERSION,
				      ?NFS_VERSION,
				      nfs_svc,
				      []).

init_tabs() ->
    ets:new(?fh_id_tab, [named_table, public, set]),
    ets:new(?id_fh_tab, [named_table, public, set]),
    ets:new(?fsid_mod_tab, [named_table, public, set]),
    ets:new(?mod_fsid_tab, [named_table, public, set]),
    ets:new(?misc_tab, [named_table, public, set]),
    ets:insert(?misc_tab, {next_fileid, 1}),
    ets:insert(?misc_tab, {fh_suffix, make_suffix()}),
    ets:insert(?misc_tab, {next_fsid, 1}),
    ok.

make_suffix() ->
    SufBits = (32 - 8) * 8,
    {A,B,C} = now(),
    S0 = A,
    S1 = (S0 * 1000000) + B,
    S2 = (S1 * 1000000) + C,
    B0 = <<S2:SufBits/integer>> .

handle_call({add_mountpoint, Path, Module}, From, State) ->
    MP0 = State#state.mountpoints,
    MP1 = dict:store(Path, Module, MP0),
    {reply, ok, State#state{mountpoints=MP1}};

%% NFS/RPC callbacks

%% ----------------------------------------------------------------------
%% MOUNTPROC_MNT
%% ----------------------------------------------------------------------
handle_call({mountproc_mnt_1, PathBin, Client}, From, State) ->
    Path = binary_to_list(PathBin),
    case dict:find(Path, State#state.mountpoints) of
	{ok, Mod} ->
	    case catch apply(Mod, root, []) of
		{'EXIT', Rsn} ->
		    io:format("Error in root/0: ~p~n", [Rsn]),
		    {reply, {1, void}, State};
		Root ->
		    {ok, RootFH} = id2fh(Root, Mod),
		    {reply, {0, RootFH}, State}
	    end;
	error ->
	    {reply, {1, void}, State}
    end;

%% ----------------------------------------------------------------------
%% NFSPROC_GETATTR
%% ----------------------------------------------------------------------

handle_call({nfsproc_getattr_2, FH, Client}, From, State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		case fattr(FH, Mod) of
		    {ok, FA} ->
			{'NFS_OK', FA};
		    {error, Error} ->
			{error(Error), void}
		end;
	    error ->
		{'NFSERR_STALE', void}
	end,
    {reply, R, State};

%% ----------------------------------------------------------------------
%% NFSPROC_READDIR
%% ----------------------------------------------------------------------

handle_call({nfsproc_readdir_2, {FH, <<Cookie:32/integer>>, Count}, Client},
	    From,
	    State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		case catch apply(Mod, dirlist, [ID]) of
		    {'EXIT', Rsn} ->
			io:format("Error in readdir: ~p~n", [Rsn]),
			{error(io), void};
		    {error, ErrCode} ->
			{error(ErrCode), void};
		    {ok, Names} ->
			Entries = entries(Mod, ID, Names, Cookie),
			{'NFS_OK', {Entries, true}}
		end;
	    error ->
		{'NFSERR_STALE', void}
	end,
    {reply, R, State};

%% ----------------------------------------------------------------------
%% NFSPROC_LOOKUP
%% ----------------------------------------------------------------------

handle_call({nfsproc_lookup_2, {DirFH, NameBin}, C}, From, State) ->
    R = case fh2id(DirFH) of
	    {ok, DirID} ->
		Name = binary_to_list(NameBin),
		Mod = fh2mod(DirFH),
		case catch apply(Mod, lookup, [DirID, Name]) of
		    {'EXIT', Rsn} ->
			io:format("Error in lookup: ~p~n", [Rsn]);
		    {error, Error} ->
			{error(Error), void};
		    {ok, ChildID} ->
			{ok, ChildFH} = id2fh(ChildID, Mod),
			case fattr(ChildFH, Mod) of
			    {ok, FA} ->
				{'NFS_OK', {ChildFH, FA}};
			    {error, Error} ->
				{error(Error), void}
			end
		end;
	    error ->
		{'NFSERR_STALE', void}
	end,
    {reply, R, State};

%% ----------------------------------------------------------------------
%% NFSPROC_READ
%% ----------------------------------------------------------------------

handle_call({nfsproc_read_2, {FH, Offset, Count, _}, C}, From, State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		case catch apply(Mod, read, [ID]) of
		    {ok, IOList} ->
			case fattr(FH, Mod) of
			    {ok, FA} ->
				{'NFS_OK', {FA, list_to_binary([IOList])}};
			    {error, Error} ->
				{error(Error), void}
			end;
		    {error, Reason} ->
			{error(Reason), void};
		    {'EXIT', Rsn} ->
			io:format("Error in read: ~p~n", [Rsn]),
			{error(io), void}
		end;
	    error ->
		{'NFSERR_STALE', void}
	end,
    {reply, R, State};

%% ----------------------------------------------------------------------
%% NFSPROC_READ
%% ----------------------------------------------------------------------

handle_call({nfsproc_statfs_2, FH, C}, From, State) ->
    R = case fh2id(FH) of
	    {ok, ID} ->
		Mod = fh2mod(FH),
		case catch apply(Mod, statfs, [ID]) of
		    {ok, Res = {Tsize, Bsize, Blocks, Bfree, Bavail}} ->
			{'NFS_OK', Res};
		    {error, Reason} ->
			{error(Reason), void};
		    Other ->
			io:format("Bad return from ~p:statfs/1: ~p~n",
				  [Mod, Other])
		end;
	    error ->
		{'NFSERR_STALE', void}
	end,
    {reply, R, State};

handle_call(Request, From, State) ->
    io:format("Undefined callback: ~p~n", [Request]),
    Reply = {error, nocallback},
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% getattr
%% ----------------------------------------------------------------------

fattr(FH, Mod) ->
    {ok, ID} = fh2id(FH),
    case catch apply(Mod, getattr, [ID]) of
	{dir, Access} ->
	    {ok, make_fattr([{type, 'NFDIR'},
			     {mode, ?MODE_DIR bor access(Access)},
			     {fsid, mod2fsid(Mod)},
			     {fileid, fh2fileid(FH)},
			     {ctime, {0,0}},
			     {mtime, {0,0}},
			     {atime, {0,0}}])};
	{file, Access, Timestamp, Size} ->
	    {ok, make_fattr([{type, 'NFREG'},
			     {mode, ?MODE_REGULAR bor access(Access)},
			     {fsid, mod2fsid(Mod)},
			     {fileid, fh2fileid(FH)},
			     {size, Size},
			     {ctime, Timestamp},
			     {mtime, Timestamp},
			     {atime, Timestamp}])};
	{'EXIT', Rsn} ->
	    {error, io}
    end.
    
entries(Mod, ID, [], N) ->
    void;
entries(Mod, ID, [H|T], N) ->
    case catch apply(Mod, lookup, [ID, H]) of
	{ok, CID} ->
	    {id2fileid(ID, Mod),	% fileid
	     H,				% name
	     <<N:32/integer>>,		% cookie
	     entries(Mod, ID, T, N+1)	% nextentry
	    };
	{error, Error} ->
	    %% just skip this one
	    entries(Mod, ID, T, N+1)
    end.

%% id2fh(ID, FSID | TemplateFH)
%% Returns: {ok, FH}
id2fh(ID, TemplateFH) when binary(TemplateFH) ->
    id2fh(ID, fh2fsid(TemplateFH));
id2fh(ID, FSID) ->
    case ets:lookup(?id_fh_tab, ID) of
	[{_, FH}] ->
	    {ok, FH};
	[] ->
	    {ok, new_fh(ID, FSID)}
    end.

id2fileid(ID, Mod) ->
    {ok, FH} = id2fh(ID, Mod),
    fh2fileid(FH).

%% Returns: {ok, ID} | not_found
fh2id(FH) ->
    case ets:lookup(?fh_id_tab, FH) of
	[{_, ID}] ->
	    {ok, ID};
	[] ->
	    error
    end.

fh2fileid(<<FileID:32/integer, _/binary>>) ->
    FileID.

new_fh(ID, Mod) ->
    FSID = mod2fsid(Mod),
    [{fh_suffix, Suf}] = ets:lookup(?misc_tab, fh_suffix),
    [{next_fileid, N}] = ets:lookup(?misc_tab, next_fileid),
    ets:update_counter(?misc_tab, next_fileid, 1),
    FH = <<N:32/integer, FSID:32/integer, Suf/binary>> ,
    ets:insert(?id_fh_tab, {ID, FH}),
    ets:insert(?fh_id_tab, {FH, ID}),
    FH.

fh2mod(FH) ->
    <<_FileID:32/integer, FSID:32/integer, _Pad/binary>> = FH,
    fsid2mod(FSID).

fh2fsid(<<_:32/integer, FSID:32/integer, _/binary>>) ->
    FSID.

mod2fsid(Mod) ->
    case ets:lookup(?mod_fsid_tab, Mod) of
	[{_, FSID}] ->
	    FSID;
	[] ->
	    new_fsid(Mod)
    end.

new_fsid(Mod) ->
    [{next_fsid, N}] = ets:lookup(?misc_tab, next_fsid),
    ets:update_counter(?misc_tab, next_fsid, 1),
    ets:insert(?fsid_mod_tab, {N, Mod}),
    ets:insert(?mod_fsid_tab, {Mod, N}),
    N.

fsid2mod(FSID) ->
    [{_, Mod}] = ets:lookup(?fsid_mod_tab, FSID),
    Mod.

%% ----------------------------------------------------------------------
%% File attributes
%% ----------------------------------------------------------------------
%% Make an fattr (file attributes) struct. Opts is a dictionary of
%% values we're interested in setting (see fattr_spec/0 below for
%% available options).
make_fattr(Opts) ->
    L = make_fattr_list(fattr_spec(), Opts),
    list_to_tuple(L).

make_fattr_list([], Opts) ->
    [];
make_fattr_list([{Tag, Default}|T], Opts) ->
    Value = case lists:keysearch(Tag, 1, Opts) of
		{value, {_, V}} -> V;
		false           -> Default
	    end,
    [Value|make_fattr_list(T, Opts)];
make_fattr_list([Tag|T], Opts) ->
    Value = case lists:keysearch(Tag, 1, Opts) of
		{value, {_, V}} -> V;
		false           -> exit({fattr, undefined, Tag})
	    end,
    [Value|make_fattr_list(T, Opts)].

%% List of file attributes, some which have defaults.
fattr_spec() ->
    [type, mode, {nlink, 1}, {uid, 0}, {gid, 0}, {size, 0}, {blocksize, 1024},
     {rdev, 0}, {blocks, 1}, fsid, fileid, atime, mtime, ctime].

access(r)   -> ?MODE_UR bor ?MODE_GR bor ?MODE_OR;
access(w)   -> ?MODE_UW bor ?MODE_GW bor ?MODE_OW;
access(x)   -> ?MODE_UX bor ?MODE_GX bor ?MODE_OX;
access(rw)  -> access(r) bor access(w);
access(rwx) -> access(rw) bor access(x);
access(rx)  -> access(r) bor access(x);
access(wx)  -> access(w) bor access(x).

error(noent) -> 'NFSERR_NOENT';
error(io)    -> 'NFSERR_IO'.
