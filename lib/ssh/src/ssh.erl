%%% File    : ssh.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : ssh client
%%% Created : 20 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh).

-compile(export_all).

-include("../include/ssh.hrl").
-include("../include/ssh_connect.hrl").


connect(CM) ->
    case ssh_cm:attach(CM) of
	{ok,CMPid} ->
	    session(CMPid);
	Error ->
	    Error
    end.

connect(Host, Opts) ->
    connect(Host, 22, Opts).

connect(Host,Port,Opts) ->
    case ssh_cm:start(undefined, Host, Port, Opts) of
	{ok, CM} ->
	    session(CM);
	Error ->
	    Error
    end.

session(CM) ->
    case ssh_cm:session_open(CM) of
	{ok,Channel}  ->
	    case ssh_cm:shell(CM, Channel) of
		ok ->
		    {group_leader,GIO} = 
			process_info(self(), group_leader),
		    IO = spawn(?MODULE, input_loop, [GIO,self()]),
		    shell_loop(CM, Channel, IO, false);
		Error  ->
		    ssh_cm:close(CM, Channel),
		    Error
	    end;
	Error ->
	    Error
    end.


input_loop(Fd, Pid) ->
    case io:get_line(Fd, '>') of
	eof ->
	    Pid ! {input, self(), eof},
	    input_loop(Fd, Pid);
	Line ->
	    Pid ! {input, self(), Line},
	    input_loop(Fd, Pid)
    end.
    

shell_loop(CM, Channel, IO, SentClose) ->
    receive
	{input, IO, eof} ->
	    ssh_cm:send_eof(CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);
	    
	{input, IO, Line} ->
	    ssh_cm:send(CM, Channel, Line),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);

	{ssh_cm, CM, {data, Channel, Type, Data}} ->
	    if Type == 0 ->
		    io:format("~s", [binary_to_list(Data)]);
	       Type == ?SSH_EXTENDED_DATA_STDERR ->
		    io:format("STDERR: ~s", [binary_to_list(Data)]);
	       true ->
		    ok
	    end,
	    ssh_cm:adjust_window(CM, Channel, size(Data)),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);

	{ssh_cm, CM, {exit_signal,Channel,SIG, Err, Lang}} ->
	    io:format("SIGNAL: ~s (~s)\n", [SIG, Err]),
	    send_close(SentClose, CM, Channel),
	    shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {exit_status,Channel,Status}} ->
	    send_close(SentClose, CM, Channel),
	    shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {eof, Channel}} ->
	    send_close(SentClose, CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {closed, Channel}} ->
	    io:format("Closed: Channel=~p\n",[Channel]),
	    ssh_cm:detach(CM),
	    exit(IO, kill)
    end.

send_close(false, CM, Channel) ->
    ssh_cm:close(CM, Channel);
send_close(_, _, _) ->
    ok.
	    
	    
	    
	




	    
    
	    



	    
	    
	    

	    
	    


