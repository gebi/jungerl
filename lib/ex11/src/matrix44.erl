-module(matrix44).
-author('tobbe@erix.ericsson.se').
%% ---------------------------------------------------------------------
%% Created:   92-01-29 by Tobbe (tobbe@erix.ericsson.se)
%%
%% matrix44  -  4x4 Matrix operations,suitable for 
%%              3D-Graphics transformations
%%
%% Data structure: Matrix ::=  {X11,X12,X13,X14,X21,...,X44}
%%                 Vector ::=  {X1,X2,X3,X4}
%%
%% Useful functions:
%%
%%   multiply44( Matrix_X , Matrix_Y )
%%
%%       Multiplies two 4 by 4 matrices and returns the 
%%       resulting matrix.
%%
%%   multiply14( Vector , Matrix )
%%
%%       Multiplies a vector (with 4 elements) with a Matrix and
%%       returns the resulting vector.
%%
%%   mk_rotate_matrix( Axis , Angle )
%%
%%       Returns the rotation matrix for rotating Angle degrees
%%       around the Axis (x/y/z) .
%%
%%   mk_projection_matrix( Projection_distance )
%%
%%       Returns the projection matrix, where the Projection_distance       
%%       is the distance along the Z-axis from the Projection point
%%       to the surface on which the projection is made.
%%
%%   mk_scale_matrix( Xs , Ys , Zs )
%%
%%       Returns the scale matrix, prepared with the scale 
%%       factors Xs,Ys,Zs .
%%
%%   mk_hcord( X , Y , Z )
%%
%%       Returns a Vector in homogenus coordinates. The Vector 
%%       represents the given point specified by X,Y,Z .
%%
%% ---------------------------------------------------------------------
-export([start/0,multiply44/2,multiply14/2,mk_projection_matrix/1,
	mk_rotate_matrix/2,mk_scale_matrix/3,mk_hcord/3,
	mk_translate_matrix/3]).
-import(math,[pi/0,cos/1,sin/1]).

start() -> 
    Y = mk_rotate_matrix(y,30),
    X = mk_rotate_matrix(x,15),
    P = mk_projection_matrix(5),
    T = multiply44(Y,X),
    M = multiply44(T,P),
    multiply14({1,0,0,1},M).

multiply14(V,X) ->
    vm4_mult(1,1,V,X).

multiply44(X,Y) ->
    {Z11,Z12,Z13,Z14} = vm4_mult(1,1,X,Y),
    {Z21,Z22,Z23,Z24} = vm4_mult(5,1,X,Y),
    {Z31,Z32,Z33,Z34} = vm4_mult(9,1,X,Y),
    {Z41,Z42,Z43,Z44} = vm4_mult(13,1,X,Y),
    {Z11,Z12,Z13,Z14,Z21,Z22,Z23,Z24,Z31,Z32,Z33,Z34,Z41,Z42,Z43,Z44}.

vm4_mult(S1,S2,X,Y) ->
    {v4_mult(S1,S2,X,Y),
     v4_mult(S1,S2+1,X,Y),
     v4_mult(S1,S2+2,X,Y),
     v4_mult(S1,S2+3,X,Y)}.

v4_mult(S1,S2,X,Y) ->
    T = element(S1,X)*element(S2,Y) + element(S1+1,X)*element(S2+4,Y),
    T + element(S1+2,X)*element(S2+8,Y) + element(S1+3,X)*element(S2+12,Y).

mk_rotate_matrix(y,Angle) ->
    Alpha = mk_rad(Angle),
    {cos(Alpha),0,-sin(Alpha),0,
     0,1,0,0,
     sin(Alpha),0,cos(Alpha),0,
     0,0,0,1};
mk_rotate_matrix(x,Angle) ->
    Alpha = mk_rad(Angle),
    {1,0,0,0,
     0,cos(Alpha),sin(Alpha),0,
     0,-sin(Alpha),cos(Alpha),0,
     0,0,0,1};
mk_rotate_matrix(z,Angle) ->
    Alpha = mk_rad(Angle),
    {cos(Alpha),sin(Alpha),0,0,
     -sin(Alpha),cos(Alpha),0,0,
     0,0,1,0,
     0,0,0,1}.

mk_projection_matrix(Z) ->
    {1,0,0,0,
     0,1,0,0,
     0,0,0,-1/Z,
     0,0,0,1}.

mk_scale_matrix(X,Y,Z) ->
    {X,0,0,0,
     0,Y,0,0,
     0,0,Z,0,
     0,0,0,1}.

mk_translate_matrix(X,Y,Z) ->
    {1,0,0,0,
     0,1,0,0,
     0,0,1,0,
     X,Y,Z,1}.

mk_rad(Angle) ->
    Pi = pi(),
    Angle*Pi/180.0.

mk_hcord(X,Y,Z) -> {X,Y,Z,1}.
