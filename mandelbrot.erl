-module(mandelbrot).
-export([start/1, value_at/3, loop/1]).

sqr_len(C) ->
  R = complex:real(C),
  I = complex:imaginary(C),
  R * R + I * I.

mandel(C,MaxI,Z,I) ->
  case (I < MaxI) and (sqr_len(Z) < 2.0) of
    true ->
      Zsqrd = complex:mult(Z,Z),
      Znext = complex:add(C,Zsqrd),
      mandel(C,MaxI,Znext,I+1);
    false ->
      I
  end.
  
start(MaxI) -> spawn(mandelbrot,loop,[MaxI]).
  
value_at(Pid,X,Y) -> 
  rpc(Pid,{mandel,X,Y}).

rpc(Pid,Req) ->
  Pid ! {self(), Req},
  receive
    {Pid, Resp} -> 
      Resp
  end.

loop(MaxIter) ->
  receive
    {From,{mandel,X,Y}} ->
      C = complex:make(X,Y),
      V = mandel(C,MaxIter,C,1),
      From ! {self(),V},
      loop(MaxIter);
    _ ->
      io:format("What?~n"),
      loop(MaxIter)
  end.
