/* Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved. */
typedef string String<>;

struct repeatargs {
	int	num;
	String	str;
};

struct powargs {
	double	x;
	double	y;
};

enum status {
	STATUS_OK	= 0,
	STATUS_ERR	= 1
};

struct sillypowok {
	String	message;
	double	result;
};

union sillypowres switch (status res) {
    case STATUS_OK:
	sillypowok	powok;
    default:
	void;
};

program TEST_PROG1 {
	version TEST_VERS2 {
	void FNULL(void) = 0;

	int DOUBLE(int) = 1;

	String REPEAT(repeatargs) = 2;

	sillypowres POW(powargs) = 3;
	} = 2;
} = 1;
