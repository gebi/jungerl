/*
** INTEGER STACK example
** Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
*/

program STACK_PROG {
	version STACK_VERS {
		void STACKPROC_NULL(void)               = 0;
		int  STACKPROC_POP (void)               = 1;
	        bool STACKPROC_PUSH(int)                = 2;
	  } = 1;
} = 0x20000001;



