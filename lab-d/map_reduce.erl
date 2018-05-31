%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a very simple implementation of map-reduce, in both 
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(map_reduce).
-compile(export_all).

%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce. 

%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
	      || {K,V} <- Input,
		 {K2,V2} <- Map(K,V)],
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
	   KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).

group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

map_reduce_par(Map,M,Reduce,R,Input) ->
  main(Input, Map, Reduce).

spawn_mapper(Parent,Map,R,Split) ->
    spawn_link(fun() ->
			Mapped = [{erlang:phash2(K2,R),{K2,V2}}
				  || {K,V} <- Split,
				     {K2,V2} <- Map(K,V)],
			Parent ! {self(),group(lists:sort(Mapped))}
		end).

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).


main(Input, Map, Reduce) ->
  par_map ! {Map, Reduce, Input},
  [receive {Pid,L} -> L end || _ <- Input].

initMain(Reduce) ->
  register(par_map, spawn_link(map_reduce, parMap_, [])),
  register(reduce_map, spawn_link(map_reduce, parReduce_, [])).

parMap_() ->
  receive
    {Map, Reduce, List} -> parMap(Map, Reduce, List), parMap_()
   end.

parMap(Map, Reduce, []) ->
  nothing;
parMap(Map, Reduce, [Elt|Elts]) ->
  mappers_manager ! request,
  receive
    naw -> par_reduce ! Map(Elt), parMap(Map, Reduce, Elts);
    {wa, W} -> errorHandler ! {handleM, Map, Reduce, W, Elt},  parMap(Map, Reduce, Elts)
  end.

parReduce_() ->
  receive
    {Reduce, List} -> parReduce(Reduce, List), parReduce_()
  end.

parReduce(_, []) ->
  nothing;
parReduce(Reduce, [Elt|Elts]) ->
  reducers_manager ! request,
  receive
    naw -> reduce1 ! Reduce(Elt), parMap(Elt, Elts);
    {wa, W} -> W ! errorHandler ! {handleR, Reduce, W, Elt}
  end.


mapper() ->
  receive
    {MapF, Reduce, Data} -> par_reduce ! {Reduce, MapF(Data)},
                    handler ! {finishM, self()},
                    mapper()
  end.

reducer() ->
  receive
    {Reduce, Data} -> {Url,Body} = reduce_seq(Reduce, Data),
      master ! {reduced, Url,Body},
      handler ! {finishR, self()},
      reducer()
  end.

helper_function(Manager, Work, Func) ->
    Manager ! request,
    receive
      naw -> helper_function(Manager);
      {Pid, W} -> Pid ! {Func, Work}
    end.

error_handler(Mappers, Reducers, Map, Reduce) ->
  receive
    {finishM ,Pid} -> mappers_manager ! {finish, Pid}, error_handler(Mappers#{ Pid := empty }, Reducers);
    {finishR ,Pid} -> reducers_manager ! {finish, Pid}, error_handler(Mappers, Reducers#{ Pid := empty });
    {handleM, F, Pid, Reduce, Work} -> Pid ! {F, Reduce, Work}, error_handler(Mappers#{ Pid := Work }, Reducers);
    {handleR, F, Pid, Work} -> Pid ! {F, Work},error_handler(Mappers, Reducers#{ Pid := Work });
    {'EXIT',Pid,Reason} -> case (maps:is_key(Pid, Mappers)) of
                             true -> helper_function(mappers_manager, maps:get(Pid, Mappers, Map));
                             false -> case (maps:is_key(Pid, Reducers)) of
                                        true -> helper_function(mappers_manager, maps:get(Pid, Reducers, Reduce));
                                        false -> exit(pid_not_found)
                                      end
                           end
  end.



pool_manager([]) ->
  receive
    {finish, Name} -> pool_manager([Name]);
    request        -> master ! naw, pool_manager([])
  end;
pool_manager([W]) ->
  receive
    {finish, Name} -> pool_manager([Name|[W]]);
    request        -> master ! {wa, W}, pool_manager([])
  end;
pool_manager([W|Ws]) ->
  receive
    {finish, Name} -> pool_manager([Name|([W]++Ws)]); %TODO fix this
    request        -> %%io:format("***WS: ~p\n",[Ws]),
      master ! {wa, W}, pool_manager(Ws)
  end.

foldR (Map, Pid) ->
  maps:put(Pid, empty, Map).

initPs(Size, Map, Reduce) ->
  Mappers = [spawn_link(map_reduce, mapper,[])|| X <- lists:seq(1, Size)],
  Reducers = [spawn_link(map_reduce, reducer,[])|| X <- lists:seq(1, Size)],
  register(mappers_manager, spawn_link(sudoku, pool_manager,[Mappers])),
  register(reducer_manager, spawn_link(sudoku, pool_manager,[Reducers])),
  register(error_handler, self()),
  process_flag(trap_exit,true),
  error_handler(lists:foldr(foldR/2, maps:new(), Mappers),
                    lists:foldr(foldR/2, maps:new(), Reducers)).