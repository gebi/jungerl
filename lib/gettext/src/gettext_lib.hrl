-ifndef(_GETTEXT_LIB_HRL).
-define(_GETTEXT_LIB_HRL, true).

-record(epot, {
	  msgid,      % The String
	  file_info   % List of '{Filename, LineNo}' tuples
	 }).

-endif.
