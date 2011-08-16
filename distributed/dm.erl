-module(dm).
-export([start_boss/0,boss/0,start_worker/1,worker/1,
         start_workers/2,generate/2,test/4]).

test(NWorkers,Width,Height,DivLevels) ->
    start_boss(),
    start_workers(NWorkers,boss),
    generate(mandelbrot:standard_grid(Width,Height),DivLevels).
    
generate(Grid,DivLevel) ->
    boss ! {gen,Grid,DivLevel},
    ok.

kill_workers([]) -> ok;
kill_workers([{WID,WorkerPid}|Workers]) ->
    WorkerPid ! {die,self()},
    receive
        {dying,WID,WorkerPid} -> 
            io:format("Killed worker #~w~n",[WID]),
            ok
    end,
    kill_workers(Workers).

start_boss() ->
    BossPid = spawn(dm,boss,[]),
    register(boss,BossPid),
    BossPid.
    
assign_work([],Workers) -> {[],Workers};
assign_work(Segments,[]) -> {Segments,[]};
assign_work([{GID,_}=Segment|ST],[{WID,WorkerPid}|WT]) ->
    io:format("Boss: assigned segment #~w to worker #~w~n",[GID,WID]),
    WorkerPid ! {gen,self(),Segment},
    assign_work(ST,WT).

boss() -> 
    {ok,S} = file:open("imagedata.txt",write),
    boss([],[],{[],0},S).
boss(OrigSegments,OrigWorkers,{Results,MaxResults},OutFile) ->
    {Segments,Workers} = assign_work(OrigSegments,OrigWorkers),
    receive
        {hello,WorkerPid} -> 
            WID = length(Workers)+1,
            io:format("Boss: worker #~w joined~n", [WID]),
            WorkerPid ! {hello,WID},
            boss(Segments,[{WID,WorkerPid}|Workers],{Results,MaxResults},OutFile);
        {numworkers,From} ->
            From ! {numworkers,length(Workers)},
            boss(Segments,Workers,{Results,MaxResults},OutFile);
        {gen,Grid,DivLevel} -> 
            if 
                MaxResults /= 0 -> 
                    io:format("Boss: ignoring gen~n"),
                    boss(Segments,Workers,{Results,MaxResults},OutFile);
                true -> ok
            end,
            statistics(wall_clock),
            Width = grid:get_width(Grid),
            Height = grid:get_height(Grid),
            NumSubGrids = trunc(math:pow(2,DivLevel)),
            io:format(OutFile,"FORMAT: ~w,~w,~w~n",
                      [Width,Height,NumSubGrids]),
            io:format("Boss: generating ~wx~w in ~w segments~n",
                      [Width,Height,NumSubGrids]),
            SubGrids = grid:subdivide(Grid,DivLevel),
            NewSegments = lists:zip(lists:seq(1,length(SubGrids)),SubGrids),
            boss(NewSegments,Workers,{Results,NumSubGrids},OutFile);
        {complete,{WID,WorkerPid},{GID,Grid},Image} -> 
            io:format("Boss: received image for segment #~w~n",[GID]),
            dump_result(GID,Grid,Image,OutFile),
            NewResults = [{{GID,Grid},Image}|Results],
            NewWorkers = [{WID,WorkerPid}|Workers],
            if
                length(NewResults) == MaxResults ->
                    {_,Elapsed} = statistics(wall_clock),
                    io:format("Boss: finished in ~ws~n",[Elapsed/1000]),
                    file:close(OutFile),
                    kill_workers(NewWorkers),
                    init:stop(),
                    ok; %boss([],[From|Workers],{[],0});
                true ->
                    boss(Segments,NewWorkers,{NewResults,MaxResults},OutFile)
            end
    end.

dump_result(GID,Grid,Image,OutF) ->
    PXMin = grid:get_pxmin(Grid),
    PYMin = grid:get_pymin(Grid),
    PXMax = grid:get_pxmax(Grid),
    PYMax = grid:get_pymax(Grid),
    ImgData = lists:map(fun(Row) -> [Iter || {_,Iter} <- Row] end,Image),
    io:format(OutF,"DATA ~w (~w,~w) (~w,~w)~n",[GID,PXMin,PYMin,PXMax,PYMax]),
    lists:foreach(fun(Row) -> io:format(OutF,"~w~n",[Row]) end, ImgData),
    io:format(OutF,"END~n",[]),
    ok.

num_workers(BossPid) ->
    BossPid ! {numworkers,self()},
    receive
        {numworkers,N} -> N
    end.

start_workers(N,BossPid) ->
    start_workers_loop(N,BossPid),
    wait_for_workers(N,BossPid).
    
start_workers_loop(0,_) -> ok;
start_workers_loop(N,BossPid) ->
    start_worker(BossPid), 
    start_workers_loop(N-1,BossPid).
    
wait_for_workers(N,BossPid) ->
    NumWorkers = num_workers(BossPid),
    case NumWorkers == N of
        true -> N;
        false ->
            timer:sleep(100),
            wait_for_workers(N,BossPid)
    end.

start_worker(BossPid) ->
    WorkerPid = spawn(dm,worker,[0]),
    BossPid ! {hello,WorkerPid},
    WorkerPid.
    
worker(WID) ->
    receive
        {hello,NewWID} ->
            io:format("Worker #~w: hello from ~w~n",[NewWID,node()]),
            worker(NewWID);
        {gen,From,{GID,Grid}} ->
            io:format("Worker #~w: generating segment #~w~n",[WID,GID]),
            Image = mandelbrot:generate_image(Grid,10000),
            From ! {complete,{WID,self()},{GID,Grid},Image},
            io:format("Worker #~w: finished segment #~w~n",[WID,GID]),
            worker(WID);
        {die,From} -> 
            From ! {dying,WID,self()},
            io:format("Worker #~w: goodbye~n",[WID]),
            ok
    end.
