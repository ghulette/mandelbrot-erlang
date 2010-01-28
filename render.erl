-module(render).
-export([render/3]).

map(Omin,Omax,Nmin,Nmax,Val) ->
  (Val - Omin) / (Omax - Omin) * (Nmax - Nmin) + Nmin.

render(Width,Height,MaxIter) -> 
  Pts = [{X,Y} || X <- lists:seq(0,Width-1), Y <- lists:seq(0,Height-1)],
  Xmap = fun(V) -> map(0.0,float(Width-1),-2.0,0.5,V) end,
  Ymap = fun(V) -> map(0.0,float(Height-1),-1.25,1.25,V) end,
  Mand = mandelbrot:start(MaxIter),
  Img = image:start(Width,Height,MaxIter),
  F = fun({X,Y}) -> 
    V = mandelbrot:value_at(Mand,Xmap(X),Ymap(Y)),
    image:set_pixel(Img,X,Y,V)
  end,
  lists:map(F,Pts),
  image:save(Img,ppm).
