-ifndef(_YSQL_HRL_).
-define(_YSQL_HRL, true).

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                        [?MODULE, ?LINE | Y])).

-record(ysql, {
	  odbc,    % Odbc reference
	  db,      % Database name
	  table    % Table name
	 }).


-endif.
