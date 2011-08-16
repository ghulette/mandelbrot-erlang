-module(array2d).
-export([new/2,set/4,get/3,print/1]).

offset(Arr,I,J) ->
    case Arr of
        {Width,_,_} ->
            J * Width + I
    end.

new(Width,Height) ->
    {Width,Height,array:new(Width * Height)}.
    
get(Arr,I,J) ->
    case Arr of
        {_,_,Rep} ->
            array:get(offset(Arr,I,J),Rep)
    end.
    
set(Arr,I,J,Val) ->
    case Arr of
        {_,_,Rep} ->
            array:set(Val,offset(Arr,I,J),Rep)
    end.

print(Arr) ->
    print(Arr,0).
    
print({Width,Height,Rep},I) when I < Width * Height ->
    case I rem Width of
        0 ->
            io:format("~n");
        _ -> 
            nop
    end,
    io:format("~w ",[array:get(I,Rep)]),
    print({Width,Height,Rep},I+1);
print({Width,Height,_},I) when I == Width * Height ->
    ok.
    