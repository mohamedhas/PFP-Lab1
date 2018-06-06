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
  Parent = self(),
  Splits = split_into(M,Input),
  Mappers =
    [spawn_mapper(Parent,Map,R,Split)
      || Split <- Splits],
  Mappeds =
    [receive {L, Ref} ->  L end || Ref <- worker_pool(Mappers)],
  Reducers =
    [spawn_reducer(Parent,Reduce,I,Mappeds)
      || I <- lists:seq(0,R-1)],

  Reduceds =
    [receive {L, Ref} -> L end || Ref <- worker_pool(Reducers)],
  lists:sort(lists:flatten(Reduceds)).

spawn_mapper(Parent,Map,R,Split) ->
  fun(X) ->
    Mapped = [{erlang:phash2(K2,R),{K2,V2}}
      || {K,V} <- Split,
      {K2,V2} <- Map(K,V)],
    Parent ! {group(lists:sort(Mapped)), X}
  end.

split_into(N,L) ->
  split_into(N,L,length(L)).

split_into(1,L,_) ->
  [L];
split_into(N,L,Len) ->
  {Pre,Suf} = lists:split(Len div N,L),
  [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
  (fun(X) ->
    Inputs = [KV
      || Mapped <- Mappeds,
      {J,KVs} <- Mapped,
      I==J,
      KV <- KVs],
    Parent ! {reduce_seq(Reduce,Inputs), X} end).


worker() ->
  global:whereis_name(workerSupervisor) ! {request, self()},
  receive
    {Func, Ref} -> Func(Ref), worker()
  end.

new_worker() ->
  io:format("new worker spawned  \n",[]),
  receive
    {Func, Ref} -> Func(Ref)
  end,
  new_worker().

worker_pool(Funs) ->
  [
    begin
      Ref = make_ref(),
      receive
        {request, Pid} -> global:whereis_name(workerSupervisor) ! {Fun, Ref}, Ref
      end
    end
    || Fun <- Funs ].



workers_supervisor([], Map, MPid) ->
  receive
    {request, Pid} -> MPid ! {request, Pid}, workers_supervisor([Pid], maps:put(Pid, empty, Map), MPid);
    {Func, Ref}    -> receive
                       {request, Pid} -> Pid ! {Func, Ref}, workers_supervisor([], maps:put(Pid, {Func, Ref}, Map), MPid)
                     end;
    {'EXIT', EPid, Reason} ->  case (maps:get(EPid, Map)) of
                                 empty -> spawn_link(fun() -> worker() end),
                                   workers_supervisor([], Map, MPid);
                                 {Func, Ref} -> Pid = spawn_link(fun() -> new_worker() end), Pid ! {Func, Ref},
                                   workers_supervisor([], maps:put(Pid, {Func, Ref}, Map), MPid)
                              end;
    Var -> exit(Var)
  end;
workers_supervisor([W], Map, MPid) ->
  receive
    {request, Pid} -> MPid ! {request, Pid}, workers_supervisor([W]++[Pid], maps:put(Pid, empty, Map), MPid);
    {Func, Ref}    -> W ! {Func, Ref}, workers_supervisor([], maps:put(W, {Func, Ref}, Map), MPid);
    {'EXIT', EPid, Reason} -> case (maps:get(EPid, Map)) of
                               empty -> spawn_link(fun() -> worker() end),
                                 workers_supervisor(lists:filter(fun(X)-> X==EPid end,[W]), Map, MPid);
                               {Func, Ref} -> Pid = spawn_link(fun() -> new_worker() end), Pid ! {Func, Ref},
                                 workers_supervisor(lists:filter(fun(X)-> X==EPid end,[W]), maps:put(Pid, {Func, Ref}, Map), MPid)
                             end;
    Var -> exit(Var)
  end;
workers_supervisor([W|Ws], Map, MPid) ->
  receive
    {request, Pid} -> MPid ! {request, Pid}, workers_supervisor([W|[Pid|Ws]], maps:put(Pid, empty, Map), MPid);
    {Func, Ref}    -> W ! {Func, Ref}, workers_supervisor(Ws, maps:put(W, {Func, Ref}, Map), MPid);
    {'EXIT', EPid, Reason} -> case (maps:get(EPid, Map)) of
                               empty -> spawn_link(fun() -> worker() end),
                                 workers_supervisor(lists:filter(fun(X)-> X==EPid end,[W|Ws]), Map, MPid);
                               {Func, Ref} -> Pid = spawn_link(fun() -> new_worker() end), Pid ! {Func, Ref},
                                 workers_supervisor(lists:filter(fun(X)-> X==EPid end,[W|Ws]), maps:put(Pid, {Func, Ref}, Map), MPid)
                             end;
    Var -> exit(Var)
  end.

%%node_superviser (Map, Num_Workers) ->


initWorker(Size) ->
  MPid = self(),
  spawn_link(fun() ->
    process_flag(trap_exit,true),
  global:register_name(workerSupervisor, self()),
  Workers = [begin Pid = self(),spawn_link(foo@moh, fun() -> worker() end)end|| X <- lists:seq(1, Size)],
    io:format("Workers:  ~p\n",[Workers]),
    spawn(fun() -> kill_workers(Workers) end),
  workers_supervisor([], maps:new(), MPid)end).

kill_workers(Ws) ->
  [begin timer:sleep(20000), exit(Pid, procKilled) end|| Pid <- Ws].