/*
** Simple test
** Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
*/

typedef string String<>;

program TEST {
	version TEST_VER {
		void TNULL(void) = 0;
	
		int TINT(int) = 1;

		double TDBL(double) = 2;

		String TSTR(String) = 3;

		int TINT_FLT(int, float) = 4;
	} = 1;
} = 1;

		