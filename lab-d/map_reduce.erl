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
    [receive {Pid,L} -> L end || Pid <- worker_pool(Mappers)],
  Reducers =
    [spawn_reducer(Parent,Reduce,I,Mappeds)
      || I <- lists:seq(0,R-1)],

  Reduceds =
    [receive {Pid,L} -> L end || Pid <- worker_pool(Reducers)],
  lists:sort(lists:flatten(Reduceds)).

spawn_mapper(Parent,Map,R,Split) ->
  fun() ->
    Mapped = [{erlang:phash2(K2,R),{K2,V2}}
      || {K,V} <- Split,
      {K2,V2} <- Map(K,V)],
    Parent ! {self(),group(lists:sort(Mapped))}
             end.

split_into(N,L) ->
  split_into(N,L,length(L)).

split_into(1,L,_) ->
  [L];
split_into(N,L,Len) ->
  {Pre,Suf} = lists:split(Len div N,L),
  [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
  (fun() ->
  Inputs = [KV
    || Mapped <- Mappeds,
    {J,KVs} <- Mapped,
    I==J,
    KV <- KVs],
   Parent ! {self(),reduce_seq(Reduce,Inputs)} end).


worker(Pid) ->
  Pid ! {request, self()},
  receive
    Func -> Func(), worker(Pid)
  end.

worker_pool(Funs) ->
  [
    begin
      receive
        {request, Pid} -> Pid ! Fun, Pid
      end
    end
    || Fun <- Funs ].

poolManager() ->
  receive
    Funcs -> worker_pool(Funcs)
  end.

%%bouncer() ->
%%  receive
%%    {'exit', Pid, Reason} ->
%%  end

%%handleRequest(Map, Func, Ref, Pid) ->


pool_manager([], Map, MPid) ->
  receive
    {request, Pid} -> MPid ! {request, Pid}, pool_manager([Pid], Map#{ Pid := empty }, MPid)
  end;
pool_manager([W], Map, MPid) ->
  receive
    {Func, Pid}    -> W ! Func,  pool_manager([], Map#{ Pid := {Func, Ref} }, MPid);
    {request, Pid} -> MPid ! {request, Pid}, pool_manager([W|Pid], Map, MPid)
  end;
pool_manager([W|Ws], Map, MPid) ->
  receive
    {Func, Pid}    -> W ! Func,  pool_manager(Ws, Map#{ Pid := {Func, Ref} }, MPid);
    {request, Pid} -> MPid ! {request, Pid}, pool_manager([W|[Pid|Ws]], Map, MPid)
  end.

initWorker(Size) ->
  Workers = [begin Pid = self(),spawn_link(foo@moh, fun() -> worker(Pid) end)end|| X <- lists:seq(1, Size)].
  %%register(pool_manager, self()).


