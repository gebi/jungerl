%%%----------------------------------------------------------------------
%%% File    : edit_globalmap.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Global keymap
%%% Created : 23 Sep 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_globalmap).
-author('luke@bluetail.com').

-export([init/0]).

%% Initialise global keymap
init() ->
    Map = edit_keymap:new(global_map),
    edit_keymap:bind_each(Map, bindings()),
    edit_keymap:new(global_cx_map),
    edit_keymap:bind_each(global_cx_map, cx_bindings()),
    edit_keymap:new(minibuffer_map),
    edit_keymap:bind_each(minibuffer_map, minibuffer_bindings()),
    edit_keymap:new(term_escape_map),
    edit_keymap:bind_each(term_escape_map, term_escape_bindings()),
    edit_keymap:new(help_map),
    edit_keymap:bind_each(help_map, help_bindings()),
    edit_keymap:new(empty_keymap),
    global_map.

bindings() ->
    [
     %% Motion
     {"C-a", {edit_lib, beginning_of_line, []}},
     {"C-b", {edit_lib, backward_char, []}},
     {"C-e", {edit_lib, end_of_line, []}},
     {"C-f", {edit_lib, forward_char, []}},
     {"M-b", {edit_lib, backward_word, []}},
     {"M-f", {edit_lib, forward_word, []}},
     {"M-<", {edit_lib, start_of_buffer, []}},
     {"M->", {edit_lib, end_of_buffer, []}},
     {"C-n", {edit_lib, next_line, []}},
     {"C-p", {edit_lib, previous_line, []}},
     {"C-M-a", {edit_erlang, beginning_of_function, []}},
     {"C-v", {edit_lib, scroll_down, []}},
     {"M-v", {edit_lib, scroll_up, []}},
     {"C-l", {edit_lib, recenter, []}},
     %% Editing
     {"C-h", {edit_lib, delete_char_backward, []}},
     {"C-?", {edit_lib, delete_char_backward, []}},
     {"C-d", {edit_lib, delete_char_forward, []}},
     {"M-d", {edit_lib, delete_word_forward, []}},
     {"C-k", {edit_lib, kill_line, []}},
     {"C-w", {edit_lib, kill_region, [true]}},
     {"M-w", {edit_lib, kill_region, [false]}},
     {"C-y", {edit_lib, yank, []}},
     {"C-o", {edit_lib, open_line, []}},
     {"C-_", {edit_lib, undo, []}},
     {"C-/", {edit_lib, undo, []}},
     {"C-u", {edit_lib, undo, []}},
     %% Searching
     {"C-s", {edit_lib, search, [forward]}},
     {"C-r", {edit_lib, search, [backward]}},
     {"C-M-s", {edit_lib, regexp_search, [forward]}},
     {"C-M-r", {edit_lib, regexp_search, [backward]}},
     %% Misc
     {"C-@", {edit_lib, set_mark, []}},
     {"M-!", {edit_lib, unix_command, []}},
     {"C-t", {edit_lib, printf, []}},
     {"M-:", {edit_eval, eval_expression, []}},
     {"M-x", {edit_extended, execute_extended_command, []}},
     {"C-g", {edit_lib, abort, []}},
     %% Keymaps
     {"M-[", {keymap, term_escape_map}},
     {"C-x", {keymap, global_cx_map}},
     %% Help
     {"M-h", {keymap, help_map}},
     %% self insert commands (\r becomes \n)
     {$\r,   {edit_lib, self_insert_command, [$\n]}}
     | [{Ch, {edit_lib, self_insert_command, [Ch]}} || Ch <- self_inserts()]
    ].

%% ESC [ <key> - for arrows etc
term_escape_bindings() ->
    [{"A", {edit_lib, previous_line, []}},
     {"B", {edit_lib, next_line, []}},
     {"C", {edit_lib, forward_char, []}},
     {"D", {edit_lib, backward_char, []}}].

%% Bindings after C-x prefix
cx_bindings() ->
    [{"C-x", {edit_lib, exchange_point_and_mark, []}},
     {"C-c", {edit_lib, quit, []}},
     {"C-f", {edit_file, find_file, []}},
     {"C-s", {edit_file, save_file, []}},
     {"C-m", {edit_eval, interactive_eval, []}},
     {"i",   {edit_eval, erlang_interaction_mode, []}},
     {"f",   {edit_lib, fundamental_mode, []}},
     {"r",   {edit_eval, back_to_start, []}},
     {"b",   {edit_lib, switch_to_buffer, []}},
     {"k",   {edit_lib, kill_buffer, []}},
     {"h",   {edit_lib, mark_whole_buffer, []}},
     {"o",   {edit_lib, next_window, []}},
     {"0",   {edit_lib, delete_window, []}},
     {"1",   {edit_lib, delete_other_windows, []}},
     {"2",   {edit_lib, split_window_vertically, []}},
%      {"e",   {edit_erlang, erlang_mode, []}},
     {"d",   {edit_lib, buffer_cord_info, []}}
    ].

minibuffer_bindings() ->
    [{"C-m", {edit_extended, take_argument, []}},
     {"C-g", {edit_extended, abort, []}},
     {"C-i", {edit_complete, complete, []}},
     %% generic history
     {"M-p", {edit_extended, history_move, [up]}},
     {"M-n", {edit_extended, history_move, [down]}},
     {"M-r", {edit_extended, history_search, []}}
    ].

help_bindings() ->
    [{"k", {edit_help, describe_key, []}},
     {"s", {edit_help, find_source, []}}].

self_inserts() ->
    %% HACK
    "\n\t" ++ lists:seq(32, 126).
    %%"\r\n\t" ++ (lists:seq(32, 126) -- [127]).

