%%% File    : ssh_xfer.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SSH File transfer protocol
%%% Created : 23 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(ssh_xfer).

-compile(export_all).

-include("../include/ssh.hrl").
-include("../include/ssh_xfer.hrl").
-import(lists, [foldl/3, reverse/1]).

-define(is_set(F, Bits),
	((F) band (Bits)) == (F)).

attach(CM) ->
    case ssh_cm:attach(CM) of
	{ok,CMPid} ->  open_xfer(CMPid);
	Error ->  Error
    end.

connect(Host, Port, Auth) ->
    case ssh_cm:start_link(undefined, Host, Port, Auth) of
	{ok, CM} -> open_xfer(CM);
	Error -> Error
    end.

open_xfer(CM) ->
    case ssh_cm:session_open(CM, 32768, 64536) of
	{ok, Channel} ->
	    case ssh_cm:subsystem(CM, Channel, "sftp") of
		ok ->
		    case init(CM, Channel) of
			{ok, {Vsn,Ext}} ->
			    {ok, #ssh_xfer { vsn = Vsn,
					     ext = Ext,
					     cm  = CM,
					     channel = Channel }};
			Error ->
			    Error
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.



init(CM, Channel) ->
    case request(CM, Channel, ?SSH_FXP_INIT, <<?UINT32(5)>>) of
	{ok, <<?SSH_FXP_VERSION, ?UINT32(Version), Ext/binary>>} ->
	    {ok, {Version, decode_ext(Ext)}};
	Error ->
	    Error
    end.

open(XF, ReqID, FileName, Access, Flags, Attrs) ->
    Vsn = XF#ssh_xfer.vsn,
    FileName1 = list_to_binary(FileName),
    MBits = if Vsn >= 5 -> 
		    M = encode_ace_mask(Access),
		    ?uint32(M);
	       true ->
		    (<<>>)
	    end,
    F = encode_open_flags(Flags),
    frequest(XF, ReqID, ?SSH_FXP_OPEN, 
	     [?uint32(ReqID),
	      ?string(FileName1),
	      MBits,
	      ?uint32(F),
	      encode_ATTR(Vsn,Attrs)]).    
    
opendir(XF, ReqID, DirName) ->
    DirName1 = list_to_binary(DirName),
    frequest(XF, ReqID, ?SSH_FXP_OPENDIR, 
	     [?uint32(ReqID),
	      ?string(DirName1)]).


close(XF, ReqID, Handle) ->
    frequest(XF, ReqID, ?SSH_FXP_CLOSE,
	     [?uint32(ReqID),
	      ?binary(Handle)]).

read(XF, ReqID, Handle, Offset, Length) ->
    frequest(XF, ReqID, ?SSH_FXP_READ,
	     [?uint32(ReqID),
	      ?binary(Handle),
	      ?uint64(Offset),
	      ?uint32(Length)]).

readdir(XF, ReqID, Handle) ->
    frequest(XF, ReqID, ?SSH_FXP_READDIR,
	     [?uint32(ReqID),
	      ?binary(Handle)]).    

write(XF, ReqID, Handle, Offset, Data) ->
    Data1 = if binary(Data) -> Data;
	       list(Data) -> list_to_binary(Data)
	    end,
    frequest(XF, ReqID, ?SSH_FXP_WRITE,
	     [?uint32(ReqID),
	      ?binary(Handle),
	      ?uint64(Offset),
	      ?binary(Data1)]).

%% Remove a file
remove(XF, ReqID, File) ->
    File1 = list_to_binary(File),
    frequest(XF, ReqID, ?SSH_FXP_REMOVE, 
	     [?uint32(ReqID),
	      ?string(File1)]).

%% Rename a file/directory
rename(XF, ReqID, Old, New, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    OldPath = list_to_binary(Old),
    NewPath = list_to_binary(New),
    FlagBits
	= if Vsn >= 5 ->
		  F0 = encode_rename_flags(Flags),
		  ?uint32(F0);
	     true ->
		  (<<>>)
	  end,
    frequest(XF, ReqID, ?SSH_FXP_RENAME, 
	     [?uint32(ReqID),
	      ?string(OldPath),
	      ?string(NewPath),
	      FlagBits]).



%% Create directory
mkdir(XF, ReqID, Path, Attrs) ->
    Path1 = list_to_binary(Path),
    frequest(XF, ReqID, ?SSH_FXP_MKDIR, 
	     [?uint32(ReqID),
	      ?string(Path1),
	      encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).

%% Remove a directory
rmdir(XF, ReqID, Dir) ->
    Dir1 = list_to_binary(Dir),
    frequest(XF, ReqID, ?SSH_FXP_REMOVE, 
	     [?uint32(ReqID),
	      ?string(Dir1)]).

%% Stat file
stat(XF, ReqID, Path, Flags) ->
    Path1 = list_to_binary(Path),
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    frequest(XF, ReqID, ?SSH_FXP_STAT, 
	     [?uint32(ReqID),
	      ?string(Path1),
	      AttrFlags]).


%% Stat file - follow symbolic links
lstat(XF, ReqID, Path, Flags) ->
    Path1 = list_to_binary(Path),
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    frequest(XF, ReqID, ?SSH_FXP_LSTAT, 
	     [?uint32(ReqID),
	      ?string(Path1),
	      AttrFlags]).

%% Stat open file
fstat(XF, ReqID, Handle, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    frequest(XF, ReqID, ?SSH_FXP_FSTAT, 
	     [?uint32(ReqID),
	      ?binary(Handle),
	      AttrFlags]).

%% Modify file attributes
setstat(XF, ReqID, Path, Attrs) ->
    Path1 = list_to_binary(Path),
    frequest(XF, ReqID, ?SSH_FXP_SETSTAT, 
	     [?uint32(ReqID),
	      ?string(Path1),
	      encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).


%% Modify file attributes
fsetstat(XF, ReqID, Handle, Attrs) ->
    frequest(XF,  ReqID, ?SSH_FXP_FSETSTAT, 
	     [?uint32(ReqID),
	      ?binary(Handle),
	      encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).
    
%% Read a symbolic link
readlink(XF, ReqID, Path) ->
    Path1 = list_to_binary(Path),
    frequest(XF, ReqID, ?SSH_FXP_READLINK, 
	     [?uint32(ReqID),
	      ?binary(Path)]).


%% Create a symbolic link    
symlink(XF, ReqID, LinkPath, TargetPath) ->
    LinkPath1 = list_to_binary(LinkPath),
    TargetPath1 = list_to_binary(TargetPath),
    frequest(XF, ReqID, ?SSH_FXP_SYMLINK, 
	     [?uint32(ReqID),
	      ?binary(LinkPath1),
	      ?binary(TargetPath1)]).

%% Convert a path into a 'canonical' form
realpath(XF, ReqID, Path) ->
    Path1 = list_to_binary(Path),
    frequest(XF, ReqID, ?SSH_FXP_REALPATH,     
	     [?uint32(ReqID),
	      ?binary(Path1)]).

extended(XF, ReqID, Request, Data) ->
    frequest(XF, ReqID, ?SSH_FXP_EXTENDED,
	     [?uint32(ReqID),
	      ?string(Request),
	      Data/binary]).



frequest(XF, ReqID, Op, Arg) ->
    case request(XF#ssh_xfer.cm, XF#ssh_xfer.channel, Op, Arg) of
	{ok, Reply} ->
	    freply(XF, Reply);
	Error ->
	    Error
    end.

freply(XF, << ?SSH_FXP_STATUS, ?UINT32(ReqID), ?UINT32(Status), 
	    ?UINT32(ELen), Err:ELen/binary,
	    ?UINT32(LLen), Lang:LLen/binary,
	    Reply/binary >> ) ->
    Stat = decode_status(Status),
    {status, ReqID, {Stat,binary_to_list(Err),binary_to_list(Lang),
		     Reply}};
freply(XF, <<?SSH_FXP_HANDLE, ?UINT32(ReqID),
	    ?UINT32(HLen), Handle:HLen/binary>>) ->
    {handle, ReqID, Handle};
freply(XF, <<?SSH_FXP_DATA, ?UINT32(ReqID),
	    ?UINT32(DLen), Data:DLen/binary>>) ->
    {data, ReqID, Data};
freply(XF, <<?SSH_FXP_NAME, ?UINT32(ReqID),
	    ?UINT32(Count), AData/binary>>) ->
    {name, ReqID, decode_names(XF#ssh_xfer.vsn, Count, AData)};
freply(XF, <<?SSH_FXP_ATTRS, ?UINT32(ReqID),
	    AData/binary>>) ->
    {A, _} = decode_ATTR(XF#ssh_xfer.vsn, AData),
    {attrs, ReqID, A};
freply(XF, <<?SSH_FXP_EXTENDED_REPLY, ?UINT32(ReqID),
	    RData>>) ->
    {extended_reply, ReqID, RData}.


decode_status(Status) ->
    case Status of
	?SSH_FX_OK -> ok;
	?SSH_FX_EOF -> eof;
	?SSH_FX_NO_SUCH_FILE -> no_such_file;
	?SSH_FX_PERMISSION_DENIED -> permission_denied;
	?SSH_FX_FAILURE -> failure;
	?SSH_FX_BAD_MESSAGE -> bad_message;
	?SSH_FX_NO_CONNECTION -> no_connection;
	?SSH_FX_CONNECTION_LOST -> connection_lost;
	?SSH_FX_OP_UNSUPPORTED -> op_unsupported;
	?SSH_FX_INVALID_HANDLE -> invalid_handle;
	?SSH_FX_NO_SUCH_PATH -> no_such_path;
	?SSH_FX_FILE_ALREADY_EXISTS -> file_already_exists;
	?SSH_FX_WRITE_PROTECT -> write_protect;
	?SSH_FX_NO_MEDIA -> no_media;
	?SSH_FX_NO_SPACE_ON_FILESYSTEM -> no_space_on_filesystem;
	?SSH_FX_QUOTA_EXCEEDED -> quota_exceeded;
	?SSH_FX_UNKNOWN_PRINCIPLE -> unknown_principle;
	?SSH_FX_LOCK_CONFlICT -> lock_conflict
    end.

	


decode_ext(<<?UINT32(NameLen), Name:NameLen/binary,
	    ?UINT32(DataLen), Data:DataLen/binary,
	    Tail/binary>>) ->
    [{binary_to_list(Name), binary_to_list(Data)}
     | decode_ext(Tail)];
decode_ext(<<>>) ->
    [].


request(CM, Channel, Op, Data) ->
    Data1 = if binary(Data) -> Data;
	       list(Data) -> list_to_binary(Data)
	    end,
    Size = 1+size(Data1),
    ssh_cm:send(CM, Channel, <<?UINT32(Size), Op, Data1/binary>>),
    reply0(CM, Channel).

reply0(CM, Channel) ->
    case get(rbuf) of
	undefined ->
	    reply(CM, Channel, <<>>);
	Buf ->
	    erase(rbuf),
	    reply(CM, Channel, Buf)
    end.
    
reply(CM, Channel, RData = <<?UINT32(Len),Reply/binary>>) ->
    Sz = size(Reply),
    if Len == Sz ->
	    {ok, Reply};
       Len < Sz ->
	    <<Reply1:Len/binary, RBuf/binary>> = Reply,
	    put(rbuf, RBuf);
       Len > Sz ->
	    reply_more(CM, Channel, RData)
    end;
reply(CM, Channel, RData) ->
    reply_more(CM, Channel, RData).


reply_more(CM, Channel, RData) ->
    receive
	{ssh_cm, CM, {data, Channel,Type,RData2}} ->
	    ssh_cm:adjust_window(CM, Channel, size(RData2)),
	    if Type == 0 ->
		    reply(CM, Channel, <<RData/binary, RData2/binary>>);
	       true ->
		    io:format("STDERR: ~s\n", [binary_to_list(RData2)]),
		    reply_more(CM, Channel, RData)
	    end;
	{ssh_cm, CM, {exit_signal,Channel,SIG,Err,Lang}} ->
	    ssh_cm:close(CM, Channel),
	    {error, Err};
	{ssh_cm, CM, {exit_status,Channel,Status}} ->
	    ssh_cm:close(CM, Channel),
	    eof;
	{ssh_cm, CM, {eof, Channel}} ->
	    eof;
	{ssh_cm, CM, {closed, Channel}} ->
	    {error, closed}
    end.

%%
%% Encode rename flags
%%
encode_rename_flags(Flags) ->
    encode_bits(
      fun(overwrite) -> ?SSH_FXP_RENAME_OVERWRITE;
	 (atomic) -> ?SSH_FXP_RENAME_ATOMIC;
	 (native) -> ?SSH_FXP_RENAME_NATIVE
      end, Flags).

decode_rename_flags(F) ->
    decode_bits(F,
		[{?SSH_FXP_RENAME_OVERWRITE, overwrite},
		 {?SSH_FXP_RENAME_ATOMIC, atomic},
		 {?SSH_FXP_RENAME_NATIVE, native}]).
    

encode_open_flags(Flags) ->
    encode_bits(
      fun (read) -> ?SSH_FXF_READ;
	  (write) -> ?SSH_FXF_WRITE;
	  (append) -> ?SSH_FXF_APPEND;
	  (creat) -> ?SSH_FXF_CREAT;
	  (trunc)  -> ?SSH_FXF_TRUNC;
	  (excl)   -> ?SSH_FXF_EXCL;
	  (create_new) -> ?SSH_FXF_CREATE_NEW;
	  (create_truncate) -> ?SSH_FXF_CREATE_TRUNCATE;
	  (open_existing) -> ?SSH_FXF_OPEN_EXISTING;
	  (open_or_create) -> ?SSH_FXF_OPEN_OR_CREATE;
	  (truncate_existing) -> ?SSH_FXF_TRUNCATE_EXISTING;
	  (append_data) -> ?SSH_FXF_ACCESS_APPEND_DATA;
	  (append_data_atomic) -> ?SSH_FXF_ACCESS_APPEND_DATA_ATOMIC;
	  (text_mode) -> ?SSH_FXF_ACCESS_TEXT_MODE;
	  (read_lock) -> ?SSH_FXF_ACCESS_READ_LOCK;
	  (write_lock) -> ?SSH_FXF_ACCESS_WRITE_LOCK;
	  (delete_lock) -> ?SSH_FXF_ACCESS_DELETE_LOCK
      end, Flags).

encode_ace_mask(Access) ->
    encode_bits(
      fun(read_data) -> ?ACE4_READ_DATA;
	 (list_directory) -> ?ACE4_LIST_DIRECTORY;
	 (write_data) -> ?ACE4_WRITE_DATA;
	 (add_file) -> ?ACE4_ADD_FILE;
	 (append_data) -> ?ACE4_APPEND_DATA;
	 (add_subdirectory) -> ?ACE4_ADD_SUBDIRECTORY;
	 (read_named_attrs) -> ?ACE4_READ_NAMED_ATTRS;
	 (write_named_attrs) -> ?ACE4_WRITE_NAMED_ATTRS;
	 (execute) -> ?ACE4_EXECUTE;
	 (delete_child) -> ?ACE4_DELETE_CHILD;
	 (read_attributes) -> ?ACE4_READ_ATTRIBUTES;
	 (write_attributes) -> ?ACE4_WRITE_ATTRIBUTES;
	 (delete) -> ?ACE4_DELETE;
	 (read_acl) -> ?ACE4_READ_ACL;
	 (write_acl) -> ?ACE4_WRITE_ACL;
	 (write_owner) -> ?ACE4_WRITE_OWNER;
	 (synchronize) -> ?ACE4_SYNCHRONIZE
      end, Access).

decode_ace_mask(F) ->
    decode_bits(F,
		[
		 {?ACE4_READ_DATA, read_data},
		 {?ACE4_LIST_DIRECTORY, list_directory},
		 {?ACE4_WRITE_DATA, write_data},
		 {?ACE4_ADD_FILE, add_file},
		 {?ACE4_APPEND_DATA, append_data},
		 {?ACE4_ADD_SUBDIRECTORY, add_subdirectory},
		 {?ACE4_READ_NAMED_ATTRS, read_named_attrs},
		 {?ACE4_WRITE_NAMED_ATTRS, write_named_attrs},
		 {?ACE4_EXECUTE, execute},
		 {?ACE4_DELETE_CHILD, delete_child},
		 {?ACE4_READ_ATTRIBUTES, read_attributes},
		 {?ACE4_WRITE_ATTRIBUTES, write_attributes},
		 {?ACE4_DELETE, delete},
		 {?ACE4_READ_ACL, read_acl},
		 {?ACE4_WRITE_ACL, write_acl},
		 {?ACE4_WRITE_OWNER, write_owner},
		 {?ACE4_SYNCHRONIZE, synchronize}
		]).


encode_ace_type(Type) ->
    case Type of
	access_allowed -> ?ACE4_ACCESS_ALLOWED_ACE_TYPE;
	access_denied  -> ?ACE4_ACCESS_DENIED_ACE_TYPE;
	system_audit   -> ?ACE4_SYSTEM_AUDIT_ACE_TYPE;
	system_alarm   -> ?ACE4_SYSTEM_ALARM_ACE_TYPE
    end.

decode_ace_type(F) ->
    case F of
	?ACE4_ACCESS_ALLOWED_ACE_TYPE -> access_allowed;
	?ACE4_ACCESS_DENIED_ACE_TYPE -> access_denied;
	?ACE4_SYSTEM_AUDIT_ACE_TYPE -> system_audit;
	?ACE4_SYSTEM_ALARM_ACE_TYPE -> system_alarm
    end.

encode_ace_flag(Flag) ->
    encode_bits(
      fun(file_inherit) -> ?ACE4_FILE_INHERIT_ACE;
	 (directory_inherit) -> ?ACE4_DIRECTORY_INHERIT_ACE;
	 (no_propagte_inherit) -> ?ACE4_NO_PROPAGATE_INHERIT_ACE;
	 (inherit_only) -> ?ACE4_INHERIT_ONLY_ACE;
	 (successful_access) -> ?ACE4_SUCCESSFUL_ACCESS_ACE_FLAG;
	 (failed_access) -> ?ACE4_FAILED_ACCESS_ACE_FLAG;
	 (identifier_group) -> ?ACE4_IDENTIFIER_GROUP
      end, Flag).

decode_ace_flag(F) ->
    decode_bits(F,
		[
		 {?ACE4_FILE_INHERIT_ACE, file_inherit},
		 {?ACE4_DIRECTORY_INHERIT_ACE, directory_inherit},
		 {?ACE4_NO_PROPAGATE_INHERIT_ACE, no_propagte_inherit},
		 {?ACE4_INHERIT_ONLY_ACE, inherit_only},
		 {?ACE4_SUCCESSFUL_ACCESS_ACE_FLAG, successful_access},
		 {?ACE4_FAILED_ACCESS_ACE_FLAG, failed_access},
		 {?ACE4_IDENTIFIER_GROUP, identifier_group}
		]).

encode_attr_flags(Vsn, all) ->
    encode_attr_flags(Vsn,
		      [size, uidgid, permissions,
		       acmodtime, accesstime, createtime,
		       modifytime, acl, ownergroup, subsecond_times,
		       bits, extended]);
encode_attr_flags(Vsn, Flags) ->
    encode_bits(
      fun(size) -> ?SSH_FILEXFER_ATTR_SIZE;
	 (uidgid) when Vsn ==3 -> ?SSH_FILEXFER_ATTR_UIDGID;
	 (permissions) -> ?SSH_FILEXFER_ATTR_PERMISSIONS;
	 (acmodtime) when Vsn == 3 -> ?SSH_FILEXFER_ATTR_ACMODTIME;
	 (accesstime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_ACCESSTIME;
	 (createtime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_CREATETIME;
	 (modifytime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_MODIFYTIME;
	 (acl) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_ACL;
	 (ownergroup) when  Vsn >= 5 -> ?SSH_FILEXFER_ATTR_OWNERGROUP;
	 (subsecond_times) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_SUBSECOND_TIMES;
	 (bits) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_BITS;
	 (extended) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_EXTENDED
      end, Flags).

encode_file_type(Type) ->
    case Type of
	regular -> ?SSH_FILEXFER_TYPE_REGULAR;
	directory -> ?SSH_FILEXFER_TYPE_DIRECTORY;
	symlink -> ?SSH_FILEXFER_TYPE_SYMLINK;
	special -> ?SSH_FILEXFER_TYPE_SPECIAL;
	unknown -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	socket -> ?SSH_FILEXFER_TYPE_SOCKET;
	char_device -> ?SSH_FILEXFER_TYPE_CHAR_DEVICE;
	block_device -> ?SSH_FILEXFER_TYPE_BLOCK_DEVICE;
	fifo -> ?SSH_FILEXFER_TYPE_FIFO;
	undefined -> ?SSH_FILEXFER_TYPE_UNKNOWN
    end.

decode_file_type(Type) ->
    case Type of
	?SSH_FILEXFER_TYPE_REGULAR -> regular;
	?SSH_FILEXFER_TYPE_DIRECTORY -> directory;
	?SSH_FILEXFER_TYPE_SYMLINK -> symlink;
	?SSH_FILEXFER_TYPE_SPECIAL -> special;
	?SSH_FILEXFER_TYPE_UNKNOWN -> unknown;
	?SSH_FILEXFER_TYPE_SOCKET -> socket;
	?SSH_FILEXFER_TYPE_CHAR_DEVICE -> char_device;
	?SSH_FILEXFER_TYPE_BLOCK_DEVICE -> block_device;
	?SSH_FILEXFER_TYPE_FIFO -> fifo
    end.

encode_attrib_bits(Bits) ->
    encode_bits(
      fun(readonly) -> ?SSH_FILEXFER_ATTR_FLAGS_READONLY;
	 (system) -> ?SSH_FILEXFER_ATTR_FLAGS_SYSTEM;
	 (hidden) -> ?SSH_FILEXFER_ATTR_FLAGS_HIDDEN;
	 (case_insensitive) -> ?SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE;
	 (arcive) -> ?SSH_FILEXFER_ATTR_FLAGS_ARCHIVE;
	 (encrypted) -> ?SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED;
	 (compressed) -> ?SSH_FILEXFER_ATTR_FLAGS_COMPRESSED;
	 (sparse) -> ?SSH_FILEXFER_ATTR_FLAGS_SPARSE;
	 (append_only) -> ?SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY;
	 (immutable) -> ?SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE;
	 (sync) -> ?SSH_FILEXFER_ATTR_FLAGS_SYNC
      end, Bits).

decode_attrib_bits(F) ->
    decode_bits(F,
		[{?SSH_FILEXFER_ATTR_FLAGS_READONLY, readonly},
		 {?SSH_FILEXFER_ATTR_FLAGS_SYSTEM, system},
		 {?SSH_FILEXFER_ATTR_FLAGS_HIDDEN, hidden},
		 {?SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE, case_insensitive},
		 {?SSH_FILEXFER_ATTR_FLAGS_ARCHIVE, arcive},
		 {?SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED, encrypted},
		 {?SSH_FILEXFER_ATTR_FLAGS_COMPRESSED, compressed},
		 {?SSH_FILEXFER_ATTR_FLAGS_SPARSE, sparse},
		 {?SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY, append_only},
		 {?SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE, immutable},
		 {?SSH_FILEXFER_ATTR_FLAGS_SYNC, sync}]).


%% 
%% Encode file attributes
%% 
encode_ATTR(Vsn, A) ->
    {Flags,As} =
	encode_As(Vsn, 
		  [{size, A#ssh_xfer_attr.size},
		   {ownergroup, A#ssh_xfer_attr.owner},
		   {ownergroup, A#ssh_xfer_attr.group},
		   {permissions, A#ssh_xfer_attr.permissions},
		   {acmodtime, A#ssh_xfer_attr.atime},
		   {acmodtime, A#ssh_xfer_attr.mtime},
		   {accesstime,  A#ssh_xfer_attr.atime},
		   {subsecond_times, A#ssh_xfer_attr.atime_nseconds},
		   {createtime,  A#ssh_xfer_attr.createtime},
		   {subsecond_times, A#ssh_xfer_attr.createtime_nseconds},
		   {modifytime,  A#ssh_xfer_attr.mtime},
		   {subsecond_times, A#ssh_xfer_attr.mtime_nseconds},
		   {acl, A#ssh_xfer_attr.acl},
		   {bits, A#ssh_xfer_attr.attrib_bits},
		   {extended, A#ssh_xfer_attr.extensions}],
		  0, []),
    Type = encode_file_type(A#ssh_xfer_attr.type),
    list_to_binary([?uint32(Flags),
		    if Vsn >= 5 ->
			    ?byte(Type);
		       true ->
			    (<<>>)
		    end, As]).


encode_As(Vsn, [{AName, undefined}|As], Flags, Acc) ->
    encode_As(Vsn, As, Flags, Acc);
encode_As(Vsn, [{AName, X}|As], Flags, Acc) ->
    case AName of
	size ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_SIZE,
		      [?uint64(X) | Acc]);
	ownergroup when Vsn==3 ->
	     encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_UIDGID,
		       [?uint32(X) | Acc]);
	ownergroup when Vsn>=5 ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_OWNERGROUP,
		      [?string(X) | Acc]);
	permissions ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_PERMISSIONS,
		      [?uint32(X) | Acc]);
	acmodtime when Vsn==3 ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_ACMODTIME,
		      [?uint32(X) | Acc]);
	accesstime when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_ACCESSTIME,
		      [?uint64(X) | Acc]);
	createtime when Vsn>=5->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_CREATETIME,
		      [?uint64(X) | Acc]);
	modifytime when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_MODIFYTIME,
		      [?uint64(X) | Acc]);
	subsecond_times when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,
		      [?uint64(X) | Acc]);
	acl when Vsn >=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_ACL,
		      [encode_acl(X) | Acc]);
	bits when Vsn>=5 ->
	    F = encode_attrib_bits(X),
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_BITS,
		      [?uint32(F) | Acc]);
	extended ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_EXTENDED,
		      [encode_extensions(X) | Acc]);
	true ->
	    encode_As(Vsn, As, Flags, Acc)
    end;
encode_As(Vsn, [], Flags, Acc) ->
    {Flags, reverse(Acc)}.


decode_ATTR(Vsn, <<?UINT32(Flags), Tail/binary>>) ->
    {Type,Tail2} =
	if Vsn == 3 ->
		{?SSH_FILEXFER_TYPE_UNKNOWN, Tail};
	   Vsn >= 5 ->
		<<?BYTE(T), TL/binary>> = Tail,
		{T, TL}
	end,
    decode_As(Vsn, 
	      [{size, #ssh_xfer_attr.size},
	       {ownergroup, #ssh_xfer_attr.owner},
	       {ownergroup, #ssh_xfer_attr.group},
	       {permissions, #ssh_xfer_attr.permissions},
	       {acmodtime, #ssh_xfer_attr.atime},
	       {acmodtime, #ssh_xfer_attr.mtime},
	       {accesstime,  #ssh_xfer_attr.atime},
	       {subsecond_times, #ssh_xfer_attr.atime_nseconds},
	       {createtime,  #ssh_xfer_attr.createtime},
	       {subsecond_times, #ssh_xfer_attr.createtime_nseconds},
	       {modifytime,  #ssh_xfer_attr.mtime},
	       {subsecond_times, #ssh_xfer_attr.mtime_nseconds},
	       {acl, #ssh_xfer_attr.acl},
	       {bits, #ssh_xfer_attr.attrib_bits},
	       {extended, #ssh_xfer_attr.extensions}],
	      #ssh_xfer_attr { type = decode_file_type(Type) },
	      Flags,
	      Tail2).

decode_As(Vsn, [{AName, AField}|As], R, Flags, Tail) ->
    case AName of
	size when ?is_set(?SSH_FILEXFER_ATTR_SIZE, Flags) ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	ownergroup when ?is_set(?SSH_FILEXFER_ATTR_UIDGID, Flags),Vsn==3 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	ownergroup when ?is_set(?SSH_FILEXFER_ATTR_OWNERGROUP, Flags),Vsn>=5 ->
	    <<?UINT32(Len), Bin:Len/binary, Tail2/binary>> = Tail,
	    X = binary_to_list(Bin),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);

	permissions when ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,Flags),Vsn>=5->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);

	permissions when ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,Flags),Vsn==3->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    R1 = setelement(AField, R, X),
	    Type = case X band ?S_IFMT of
		       ?S_IFDIR -> directory;
		       ?S_IFCHR -> char_device;
		       ?S_IFBLK -> block_device;
		       ?S_IFIFO -> fifi;
		       ?S_IFREG -> regular;
		       ?S_IFSOCK -> socket;
		       ?S_IFLNK -> symlink;
		       _ -> unknown
		   end,
	    decode_As(Vsn, As, R1#ssh_xfer_attr { type=Type}, Flags, Tail2);

	acmodtime when ?is_set(?SSH_FILEXFER_ATTR_ACMODTIME,Flags),Vsn==3 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	accesstime when ?is_set(?SSH_FILEXFER_ATTR_ACCESSTIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	modifytime when ?is_set(?SSH_FILEXFER_ATTR_MODIFYTIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	createtime when ?is_set(?SSH_FILEXFER_ATTR_CREATETIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	subsecond_times when ?is_set(?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,Flags),Vsn>=5 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	acl when ?is_set(?SSH_FILEXFER_ATTR_ACL, Flags), Vsn>=5 ->
	    {X,Tail2} = decode_acl(Tail),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	bits when ?is_set(?SSH_FILEXFER_ATTR_BITS, Flags), Vsn >=5 ->
	    <<?UINT32(Y), Tail2/binary>> = Tail,
	    X = decode_attrib_bits(Y),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	extended when ?is_set(?SSH_FILEXFER_ATTR_EXTENDED, Flags) ->
	    {X,Tail2} = decode_extended(Tail),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	_ ->
	    decode_As(Vsn, As, R, Flags, Tail)
    end;
decode_As(Vsn, [], R, _, Tail) ->
    {R, Tail}.


	

decode_names(Vsn, 0, Data) ->
    [];
decode_names(Vsn, I, <<?UINT32(Len), FileName:Len/binary, 
		      ?UINT32(LLen), LongName:LLen/binary,
		      Tail/binary>>) when Vsn == 3 ->
    {A, Tail2} = decode_ATTR(Vsn, Tail),
    [{binary_to_list(FileName), A} | decode_names(Vsn, I-1, Tail2)];
decode_names(Vsn, I, <<?UINT32(Len), FileName:Len/binary, 
		      Tail/binary>>) when Vsn >= 5 ->
    {A, Tail2} = decode_ATTR(Vsn, Tail),
    [{binary_to_list(FileName), A} | decode_names(Vsn, I-1, Tail2)].
    

encode_acl(ACLList) ->
    Count = length(ACLList),
    [?uint32(Count) | encode_acl_items(ACLList)].

encode_acl_items([ACE|As]) ->
    Type = encode_ace_type(ACE#ssh_xfer_ace.type),
    Flag = encode_ace_flag(ACE#ssh_xfer_ace.flag), 
    Mask = encode_ace_mask(ACE#ssh_xfer_ace.mask), 
    Who = list_to_binary(ACE#ssh_xfer_ace.who),
    [?uint32(Type), ?uint32(Flag), ?uint32(Mask), 
     ?string(Who) | encode_acl_items(As)];
encode_acl_items([]) ->
    [].


decode_acl(<<?UINT32(Count), Tail>>) ->
    decode_acl_items(Count, Tail, []).

decode_acl_items(0, Tail, Acc) -> 
    {reverse(Acc), Tail};
decode_acl_items(I, <<?UINT32(Type), 
	       ?UINT32(Flag),
	       ?UINT32(Mask),
	       ?UINT32(WLen), BWho:WLen/binary,
	       Tail/binary>>, Acc) ->
    decode_acl_items(I-1, Tail,
		     [#ssh_xfer_ace { type = decode_ace_type(Type),
				      flag = decode_ace_flag(Flag),
				      mask = decode_ace_mask(Mask),
				      who = binary_to_list(BWho)} | Acc]).



encode_extensions(Exts) ->
    Count = length(Exts),
    [?uint32(Count) | encode_ext(Exts)].

encode_ext([{Type, Data} | Exts]) ->
    [?string(Type), ?string(Data) | encode_ext(Exts)];
encode_ext([]) ->
    [].


decode_extended(<<?UINT32(Count), Tail/binary>>) ->     
    decode_ext(Count, Tail, []).

decode_ext(0, Tail, Acc) ->
    {reverse(Acc), Tail};
decode_ext(I, <<?UINT32(TLen), Type:TLen/binary,
	       ?UINT32(DLen), Data:DLen/binary,
	       Tail/binary>>,  Acc) ->
    decode_ext(I-1, Tail, [{binary_to_list(Type), Data}|Acc]).



%% Encode bit encoded flags
encode_bits(Fun, BitNames) ->
    encode_bits(Fun, 0, BitNames).

encode_bits(Fun, F, [Bit|BitNames]) ->
    encode_bits(Fun, Fun(Bit) bor F, BitNames);
encode_bits(Fun, F, []) ->
    F.

%% Decode bit encoded flags
decode_bits(F, [{Bit,BitName}|Bits]) ->
    if F band Bit == Bit ->
	    [BitName | decode_bits(F, Bits)];
       true ->
	    decode_bits(F, Bits)
    end;
decode_bits(F, []) ->
    [].






    
    

    
    
