-module(test).
-author('etxuwig@etxb.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).

foo() ->
    xmerl:export_simple(simple(), test_html, [{title, "Doc Title"}]).


foo2() ->
    xmerl:export_simple(simple(), xmerl_xml, [{title, "Doc Title"}]).


foo3() ->
    {A,_}=xmerl_scan:string(html(),[{fetch_fun, fun(DTDSpec,S) -> {ok,S} end}]),
    xmerl:export(A,xmerl_html).


simple() ->
    {document, [{title, "Doc Title"}, {author, "Ulf Wiger"}],
     [
      {section, [{heading, "heading1"}],
       [{'P', "This is a paragraph of text."},
	{section, [{heading, "heading2"}],
	 [
	  {'P', "This is another paragraph."},
	  {table, [{border, 1}], 
	   [{heading, 
	     [{col, "head1"},
	      {col, "head2"}]},
	    {row, 
	     [{col, "col11"},
	      {col, "col12"}]},
	    {row,
	     [{col, "col21"},
	      {col, "col22"}]}
	   ]}
	 ]}
	]}
     ]}.

html() ->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"DTD/xhtml1-strict.dtd\"><html>"
	"<head><title>Doc Title</title><author>Ulf Wiger</author></head>"
	"<h1>heading1</h1>"
	"<p>This is a paragraph of text.</p>"
	"<h2>heading2</h2>"
	"<p>This is another paragraph.</p>"
	"<table>"
	"<thead><tr><td>head1</td><td>head2</td></tr></thead>"
	"<tr><td>col11</td><td>col122</td></tr>"
	"<tr><td>col21</td><td>col122</td></tr>"
	"</table>"
	"</html>".
