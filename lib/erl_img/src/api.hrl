%%
%% These are the functions that make the image api 
%%


%% magic(Bin) -> true | false
-export([magic/1]).

%% mime_type() -> <mime-type>
-export([mime_type/0]).

%% extensions() -> [ ".<ext1>" ... ".<extn>"]
-export([extensions/0]).

%% read_info(Fd) -> {ok, #erl_img} | Error
-export([read_info/1]).

%% write_info(Fd, #erl_img) -> ok | Error
-export([write_info/2]).

%% read(Fd, #erl_img)                -> {ok, #erl_img'} | Error
%% read(Fd, #erl_img, RowFun, State) -> {ok, #erl_img'} | Error
%%  RowFun = fun(#erl_img, Row, RowNumber, RowFormat, St) -> St'
%%
-export([read/2, read/4]).

%% write(Fd, #erl_img) -> ok | Error
-export([write/2]).






