-module(grid).
-export([new/6,get_width/1,get_height/1,
         get_rxmin/1,get_rymin/1,get_rxmax/1,get_rymax/1,
         get_pxmin/1,get_pymin/1,get_pxmax/1,get_pymax/1,
         subdivide/2,as_lists/1]).

new(Width,Height,XMin,YMin,XMax,YMax) ->
    {grid,
     {rmin,float(XMin),float(YMin)},
     {rmax,float(XMax),float(YMax)},
     {pmin,0,0},
     {pmax,trunc(Width)-1,trunc(Height)-1}}.

get_pdist(Min,Max) -> (Max - Min) + 1.

get_width(Grid)  -> get_pdist(get_pxmin(Grid), get_pxmax(Grid)).
get_height(Grid) -> get_pdist(get_pymin(Grid), get_pymax(Grid)).
get_rxmin(G) -> {grid,{rmin,N,_},{rmax,_,_},{pmin,_,_},{pmax,_,_}} = G, N.
get_rymin(G) -> {grid,{rmin,_,N},{rmax,_,_},{pmin,_,_},{pmax,_,_}} = G, N.
get_rxmax(G) -> {grid,{rmin,_,_},{rmax,N,_},{pmin,_,_},{pmax,_,_}} = G, N.
get_rymax(G) -> {grid,{rmin,_,_},{rmax,_,N},{pmin,_,_},{pmax,_,_}} = G, N.
get_pxmin(G) -> {grid,{rmin,_,_},{rmax,_,_},{pmin,N,_},{pmax,_,_}} = G, N.
get_pymin(G) -> {grid,{rmin,_,_},{rmax,_,_},{pmin,_,N},{pmax,_,_}} = G, N.
get_pxmax(G) -> {grid,{rmin,_,_},{rmax,_,_},{pmin,_,_},{pmax,N,_}} = G, N.
get_pymax(G) -> {grid,{rmin,_,_},{rmax,_,_},{pmin,_,_},{pmax,_,N}} = G, N.

even(N) -> N rem 2 =:= 0.
  
split_pixels(Min,Max) ->
    Dist = get_pdist(Min,Max),
    HalfDist = Dist div 2,
    {{Min,Min+(HalfDist-1)},{Min+HalfDist,Max}}.
    
split_extents(Min,Max) ->
    D = (Max - Min) / 2.0,
    {{Min,Min+D},{Max-D,Max}}.
   
divide(Grid,Axis) ->
    RXMin = get_rxmin(Grid),
    RYMin = get_rymin(Grid),
    RXMax = get_rxmax(Grid),
    RYMax = get_rymax(Grid),
    PXMin = get_pxmin(Grid),
    PYMin = get_pymin(Grid),
    PXMax = get_pxmax(Grid),
    PYMax = get_pymax(Grid),
    case Axis of
        vertical ->
            {{RXMin1,RXMax1},{RXMin2,RXMax2}} = split_extents(RXMin,RXMax),
            {{PXMin1,PXMax1},{PXMin2,PXMax2}} = split_pixels(PXMin,PXMax),
            SubGrid1 = {grid,{rmin,RXMin1,RYMin},{rmax,RXMax1,RYMax},
                             {pmin,PXMin1,PYMin},{pmax,PXMax1,PYMax}},
            SubGrid2 = {grid,{rmin,RXMin2,RYMin},{rmax,RXMax2,RYMax},
                             {pmin,PXMin2,PYMin},{pmax,PXMax2,PYMax}},
            {SubGrid1,SubGrid2};
        horizontal ->
            {{RYMin1,RYMax1},{RYMin2,RYMax2}} = split_extents(RYMin,RYMax),
            {{PYMin1,PYMax1},{PYMin2,PYMax2}} = split_pixels(PYMin,PYMax),
            SubGrid1 = {grid,{rmin,RXMin,RYMin1},{rmax,RXMax,RYMax1},
                             {pmin,PXMin,PYMin1},{pmax,PXMax,PYMax1}},
            SubGrid2 = {grid,{rmin,RXMin,RYMin2},{rmax,RXMax,RYMax2},
                             {pmin,PXMin,PYMin2},{pmax,PXMax,PYMax2}},
            {SubGrid1,SubGrid2}
    end.

divide_all([],_) -> [];
divide_all([H|T],Axis) ->
    {G1,G2} = divide(H,Axis),
    [G1,G2|divide_all(T,Axis)].

subdivide_all([],_) -> [];
subdivide_all(Grids,0) -> Grids;
subdivide_all(Grids,Levels) ->
    Axis = case even(Levels) of true -> vertical; false -> horizontal end,
    subdivide_all(divide_all(Grids,Axis),Levels-1).
    
subdivide(Grid,Levels) ->
    subdivide_all([Grid],Levels).
     
segment(Min,Max,Length) ->
    L1 = lists:seq(0,Length-1),
    F = fun(N) -> N / (Length-1) * (Max-Min) + Min end,
    lists:map(F,L1).     
     
as_lists(Grid) ->
     Width = get_width(Grid),
     Height = get_height(Grid),
     RXMin = get_rxmin(Grid),
     RYMin = get_rymin(Grid),
     RXMax = get_rxmax(Grid),
     RYMax = get_rymax(Grid),
     Xs = segment(RXMin,RXMax,Width),
     Ys = segment(RYMin,RYMax,Height),
     Row = fun(Y) -> [{X,Y} || X <- Xs] end,
     [Row(Y) || Y <- Ys].
     