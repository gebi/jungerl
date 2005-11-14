-ifndef(_WBLOG_HRL).
-define(_WBLOG_HRL, true).


-define(NO_USER,  "").    % no userId


-record(wblog, {
	  id,             % unique Id
	  user,           % userId   
	  head,           % heading text
	  text,           % text content
	  comments = [],  % list of #wblog_comments{}
	  date,           % creation date
	  prev = false,   % previous Id
	  next = false    % next Id
	 }).

-record(wblog_comment, {
	  text = "",
	  who  = "unknown",
	  date   % creation date
	 }).
	

-endif.

