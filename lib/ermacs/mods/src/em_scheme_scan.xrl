Definitions.

AtomChar = [^\s\r\n\(\)]
WS       = [\s\r\n]

Rules.

\(     : {token, {'(', YYline}}.
\)     : {token, {')', YYline}}.

lambda : {token, {special, YYline}}.
define : {token, {special, YYline}}.
let    : {token, {special, YYline}}.

({AtomChar}{AtomChar}*) : {token, {atom, YYline, YYtext}}.
%% Ignore
{WS} : skip_token.

