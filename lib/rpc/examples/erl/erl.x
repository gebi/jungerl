/*
** Erlang remote execution with SUN RPC
** Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
*/

enum erl_type {
	INTEGER = 0,
	FLOAT = 1,
	ATOM = 2,
	NIL = 3,
	LIST = 4,
	TUPLE = 5
};


union erl_term switch (erl_type type) {
case INTEGER:
	hyper i;
case FLOAT:
	double f;
case ATOM:
	string a<255>;
case NIL:
	void;
case LIST:
	struct {
		erl_term head;
		erl_term tail;
	} l;
case TUPLE:
	struct {
		int size;
		erl_term elem<>;
	} t;
};

		
		
	

	