/* Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved. */
const T_CONST = 42;

enum t_enum {
  T_ENUM_0 = 0,
  T_ENUM_1 = 1,
  T_ENUM_2 = 2
};

struct t_primitive_struct {
  int            i;
  unsigned int   ui;
  t_enum         e;
  bool           b;
  hyper          h;
  unsigned hyper uh;
  /*  float          f;
      double         d; */
  opaque         ofix[3];
  opaque         olim<5>;
  string         sinf;
  string         slim<5>;
};

union t_union_e switch(t_enum x) {
 case T_ENUM_0:
   int i;
 case T_ENUM_1:
   t_primitive_struct tps;
};

union t_union_i switch(int x) {
 case 3:
   int i;
 case 6:
   t_primitive_struct tps;
};


struct *t_optional {
  int i;
  t_optional next;
};


struct t_struct {
  t_primitive_struct tfix[2];
  t_union_e          tue;
  t_primitive_struct tlim<5>;
};

