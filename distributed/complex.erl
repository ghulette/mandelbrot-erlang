-module(complex).

-export([new/2,add/2,mult/2,abs/1]).

% Represent a complex number C as a tuple {R,Z}, where C=R+Zi.

new(R,Z) ->
    {complex,float(R),float(Z)}.

add(C1,C2) ->
    {complex,A,B} = C1,
    {complex,C,D} = C2,
    new(A+C,B+D).

mult(C1,C2) ->
    {complex,A,B} = C1,
    {complex,C,D} = C2,
    R = A * C - B * D,
    Z = B * C + A * D,
    new(R,Z).
    
abs(C) ->
    {complex,A,B} = C,
    math:sqrt(A * A + B * B).
