Definitions.

Ch = [a-z]

Rules.

\+        : {token, {'+', YYline}}.
\+\+      : {token, {'++', YYline}}.
-         : {token, {'-', YYline}}.
\s        : skip_token.
({Ch}{Ch}*) : {token, {atom, YYline, YYtext}}.

