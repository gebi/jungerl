For information on how to set up, configure and run the erlang_automated_build 
script please:
* either run it with the help option - erlang_automated_build -h
* or read DOCUMENTATION.TXT which is a dump of the help for your convenience

When the script runs it expects the following externalities to be in place:
* an ssh wrapper for CVS to use instead of rsh (erl_auto_bld_ssh_wrapper)
* a function that prepares your CVS download for compilation (prepare_build)
* a function to compile your erlang sources (compile_code.erl)
* a function that enables the test server to be called (ts_stub.erl)
* a pre-tsunami  script (pre-tsunami.script)
* a post-tsunami script (post-tsunami.script)

Examples of these externalities (plus examples of erlang functions they call)
are available in ./examples/src

An example of the output that the script produces is in ./examples/output

