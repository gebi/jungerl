/* Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved. */

#include	<stdio.h>
#include	<rpc/rpc.h>

#include	"test3.h"

int
main(int argc, char *argv[])
{
	CLIENT			*rpc_clnt;
	/* */
	int			int_arg;
	int			*int_res;
	repeatargs		rpt_arg;
	String			*char_res;
	powargs			pow_arg;
	sillypowres		*pow_res;

	if (argc != 2) {
		fprintf(stderr, "usage: %s rpc-server\n", argv[0]);
		exit(1);
	}
	if ((rpc_clnt = clnt_create(argv[1], TEST_PROG1, TEST_VERS2, "tcp")) == NULL) {
		perror(argv[1]);
		exit(2);
	}

	fprintf(stderr, "Calling FNULL() ... ");
	if (fnull_2(&int_arg /* ignored */, rpc_clnt) == NULL) {
		perror("fnull_2");
		exit(3);
	}
	fprintf(stderr, "done\n");

	int_arg = 21;
	fprintf(stderr, "Calling DOUBLE(%d) = ", int_arg);
	if ((int_res = double_2(&int_arg, rpc_clnt)) == NULL) {
		perror("double_2");
		exit(4);
	}
	fprintf(stderr, "%d\n", *int_res);

	rpt_arg.num = 4;
	rpt_arg.str = "Hi!";
	fprintf(stderr, "Calling REPEAT(%d, %s) = ", rpt_arg.num, rpt_arg.str);
	if ((char_res = repeat_2(&rpt_arg, rpc_clnt)) == NULL) {
		perror("repeat_2");
		exit(5);
	}
	fprintf(stderr, "%s\n", *char_res);

	pow_arg.x = 2.0;
	pow_arg.y = 4.0;
	fprintf(stderr, "Calling POW(%f, %f) = ", pow_arg.x, pow_arg.y);
	if ((pow_res = pow_2(&pow_arg, rpc_clnt)) == NULL ||
	    pow_res->res != STATUS_OK) {
		perror("pow_2");
		exit(6);
	}
	fprintf(stderr, "%f (%s)\n", pow_res->sillypowres_u.powok.result,
		pow_res->sillypowres_u.powok.message);

	clnt_destroy(rpc_clnt);
	exit(0);
}
