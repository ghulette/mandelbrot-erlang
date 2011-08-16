-module(image).
-export([start/3, set_pixel/4, save/2, loop/1]).

create(Width,Height,MaxVal) ->
  Size = Width * Height,
  Data = array:new([{size,Size},{fixed,true},{default,0}]),
  {image,Width,Height,MaxVal,Data}.
  
set(Img,X,Y,V) ->
  case Img of
    {image,W,H,M,Data} ->
      NewData = array:set(Y*W+X,V,Data),
      {image,W,H,M,NewData};
    _ ->
      io:format("What?~n")
  end.

save_ppm({image,W,H,M,Data}) ->
  io:format("P2~n"),
  io:format("~p ~p~n",[W,H]),
  io:format("~p~n",[M]),
  Xcoords = lists:reverse([eol|lists:reverse(lists:seq(0,W-1))]),
  Ycoords = lists:seq(0,H-1),
  PixStr = fun(Cor) ->
    case Cor of
      {eol,_} -> "\n";
      {X,Y} -> integer_to_list(array:get(Y*W+X,Data))
    end
  end,
  LineF = fun(Y) -> 
    string:join(lists:map(fun(X) -> PixStr({X,Y}) end, Xcoords), " ") end,
  Lines = lists:map(LineF,Ycoords),
  io:format(Lines),
  ok.

start(Width,Height,MaxVal) -> 
  Img = create(Width,Height,MaxVal),
  spawn(image,loop,[Img]).
  
set_pixel(Pid,X,Y,V) -> rpc(Pid,{set,X,Y,V}).
save(Pid,Fmt) -> rpc(Pid,{save,Fmt}).

rpc(Pid,Req) ->
  Pid ! {self(), Req},
  receive
    {Pid, Resp} -> Resp
  end.

loop(Img) ->
  receive
    {From,{set,X,Y,V}} ->
      NewImg = set(Img,X,Y,V),
      From ! {self(),ok},
      loop(NewImg);
    {From,{save,ppm}} ->
      save_ppm(Img),
      From ! {self(),ok},
      loop(Img);
    _ ->
      io:format("What?~n"),
      loop(Img)
  end.
