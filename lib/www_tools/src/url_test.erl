-module(url_test).

-export([test/1]).

%IA Joe Armstrong
%ID 970314
%IK [url,copy]
%IH Test url copy
%IT This can only be run by joe on super.
% <p><b>url_test:test(1)</b> (2), (3) etc. copy some pages to
% ~joe/www/copied

test(1) -> test("http://www.ericsson.se/", "ericsson");
test(2) -> test("http://www.viasat.se/tvkan.html", "viasat");
test(3) -> test("http://www.altavista.digital.com/cgi-bin/query",
		"altavista").

test(URL, File) ->
    DestDir = local_info:root() ++ "/www/copied",
    Proxy   =  {"super.du.etx.ericsson.se", 888},
    url_copy:deep_copy(URL, File, DestDir, Proxy).
