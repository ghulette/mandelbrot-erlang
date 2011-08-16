-module(mandelbrot).
-export([inside/2,generate_image/2,print_image/1,standard_grid/2]).

% A single iteration of the Mandelbrot computation is given by
% p_c : C -> C
% p_c : z -> z^2 + c

standard_grid(Width,Height) -> 
    grid:new(Width,Height,-2.0,-1.25,0.5,1.25).
    
inside(C,MaxIter) ->
    Pc = fun(Z) ->
        ZSqrd = complex:mult(Z,Z),
        complex:add(ZSqrd,C)
    end,
    inside(Pc, complex:new(0.0,0.0), MaxIter, 0).
    
inside(Pc,Z,MaxIter,CurrIter) when CurrIter < MaxIter ->
    Z1 = Pc(Z),
    AbsZ1 = complex:abs(Z1),
    if
        AbsZ1 > 2.0 -> {false,CurrIter};
        true -> inside(Pc,Z1,MaxIter,CurrIter+1)
    end;
inside(_,_,_,CurrIter) -> % When CurrIter == MaxIter
    {true,CurrIter}.
    
complex_grid(Grid) ->
    [[complex:new(X,Y) || {X,Y} <- Row] || Row <- grid:as_lists(Grid)].
    
generate_image(Grid,MaxIter) ->
    Rows = complex_grid(Grid),
    F = fun(C) -> inside(C,MaxIter) end,
    lists:map(fun(Row) -> lists:map(F, Row) end, Rows).

% Some functions to dump the generated image data to the console
print_pixel(C) ->
    {In,Iter} = C,
    if
        In -> io:format("##");
        Iter > 10 -> io:format("**");
        Iter > 0 -> io:format("::");
        true -> io:format("..")
    end.
    
print_row(Row) ->
    lists:foreach(fun print_pixel/1, Row),
    io:format("~n").

print_image(Rows) ->
    lists:foreach(fun print_row/1,Rows).
