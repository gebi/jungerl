#!/bin/sh

# The funny path to -pa is so that we can find the priv dir via the
# code module.
erl -pa ../lersp/ebin/ -noshell -s lersp repl

