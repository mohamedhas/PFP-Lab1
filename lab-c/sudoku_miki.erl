-module(sudoku_miki).
-author("moh").
%% API
%% -export([]).
-compile(export_all).


%%%%%%%% generators %%%%%%%%

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose
transpose([Row]) ->
  [[X] || X <- Row];
transpose([Row|M]) ->
  [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

triples([A,B,C|D]) ->
  [[A,B,C]|triples(D)];
triples([]) ->
  [].

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order
blocks(M) ->
  Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
  lists:append(
    lists:map(fun(X)->
      lists:map(fun lists:append/1, X)
              end,
      Blocks)).

unblocks(M) ->
  lists:map(
    fun lists:append/1,
    transpose(
      lists:map(
        fun lists:append/1,
        lists:map(
          fun(X)->lists:map(fun triples/1,X) end,
          triples(M))))).

entries(Row) ->
  [X || X <- Row,
    1 =< X andalso X =< 9].

%% decide whether a position is safe
safe_entries(Row) ->
  Entries = entries(Row),
  lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
  lists:all(fun safe_entries/1,M).

safe(M) ->
  safe_rows(M) andalso
    safe_rows(transpose(M)) andalso
    safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9
fill(M) ->
  Nine = lists:seq(1,9),
  [[if 1=<X, X=<9 ->
    X;
      true ->
        Nine
    end
    || X <- Row]
    || Row <- M].

prefine(M) ->
  case catch solve_refined(M) of
    {'EXIT',no_solution} -> io:format("***--exit: ~p\n",[no_solution]);
      %%master ! no_solution;
    {'EXIT',Ms} ->
      io:format("***--exit: ~p\n",[Ms]);
    Solution ->
      master !  {solution, Solution}
  end.

% Given a matrix it will solve it sequentially (by refining and possibly subsequent guessing)
% If there is a solution it will be sent to master 
% If there's not, it will msg master
% When wkr is done, it will make itself available in pool and restart
worker() ->
  receive
    exitP -> io:format("***--exit: ~p\n",['worker']);
    M -> case catch solve(M) of 
            {'EXIT',{invalid_solution,Solution}} -> 
                io:format("***worker exit: invalid solution: ~p\n", [Solution]),
                master ! deadend;
            {'EXIT',no_solution} -> master ! deadend;
            Solution -> master !  {solution, Solution}
    	   end,
        manager ! {finish, self()},
        worker()
  end.

% The mgr is running with a list of wkrs in its own process.
% if recieve request, master will get ok/wa & a pid, mgr restarts with new-- list
% if recieve finish wkr, mgr will resrart with new++ list
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
    {finish, Name} -> pool_manager([Name|[W|Ws]]); % OR: ([Name,W|Ws])
    request        -> master ! {wa, W}, pool_manager(Ws)
  end.

%% refine entries which are lists by removing numbers they are known
%% not to be
refine(M) ->
  NewM =
    refine_rows(
      transpose(
        refine_rows(
          transpose(
            unblocks(
              refine_rows(
                blocks(M))))))),
  if M==NewM ->
    M;
    true ->
      refine(NewM)
  end.

refine_rows(M) ->
  lists:map(fun refine_row/1,M).

refine_row(Row) ->
  Entries = entries(Row),
  NewRow =
    [if is_list(X) ->
      case X--Entries of
        [] ->
          exit(no_solution);
        [Y] ->
          Y;
        NewX ->
          NewX
      end;
       true ->
         X
     end
      || X <- Row],
  NewEntries = entries(NewRow),
  %% check we didn't create a duplicate entry
  case length(lists:usort(NewEntries)) == length(NewEntries) of
    true ->
      NewRow;
    false ->
      exit(no_solution)
  end.

is_exit({'EXIT',_}) ->
  true;
is_exit(_) ->
  false.

%% is a puzzle solved?
solved(M) ->
  lists:all(fun solved_row/1,M).

solved_row(Row) ->
  lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?
hard(M) ->
  lists:sum(
    [lists:sum(
      [if is_list(X) ->
        length(X);
         true ->
           0
       end
        || X <- Row])
      || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices
guess(M) ->
  Nine = lists:seq(1,9),
  {_,I,J,X} =
    lists:min([{length(X),I,J,X}
      || {I,Row} <- lists:zip(Nine,M),
      {J,X} <- lists:zip(Nine,Row),
      is_list(X)]),
  {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.
guesses(M) ->
  {I,J,Guesses} = guess(M),
  Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
  SortedGuesses =
    lists:sort(
      [{hard(NewM),NewM}
        || NewM <- Ms,
        not is_exit(NewM)]),
  [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
  update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
  {Pre,[_|Post]} = lists:split(I-1,Xs),
  Pre++[X|Post].

%% solve a puzzle sequentially
solve(M) ->
  Solution = solve_refined(refine(fill(M))),
  case valid_solution(Solution) of
    true ->
      Solution;
    false ->
      exit({invalid_solution,Solution})
  end.

%Entry point, solve sudoku in paralell
psolve(M) ->
  Solution = psolve_refined(refine(fill(M))),
  case valid_solution(Solution) of
    true ->
      Solution;
    false ->
      exit({invalid_solution,Solution})
  end.

solve_refined(M) ->
  case solved(M) of
    true ->
      M;
    false ->
      solve_one(guesses(M))
  end.

%input is a refined sudoku, resort to guessing unless solved
psolve_refined(M) ->
  case solved(M) of
    true ->
      M;
    false ->
      psolve_one(guesses(M))
  end.

% Solve one of the guesses, resursive func.
solve_one([]) ->
  exit(no_solution);
solve_one([M]) ->
  solve_refined(M);
solve_one([M|Ms]) ->
  case catch solve_refined(M) of
    {'EXIT',no_solution} ->
      solve_one(Ms);
    Solution ->
      Solution
  end.

%init the wkrs, then the mgr with list of all wkrs
initPs(Size) ->
  Workers = [spawn_link(sudoku, worker,[])|| Foorloopsimulator <- lists:seq(1, Size)],
  io:format("list of threads : ~p\n" , [Workers]),
  register(manager, spawn_link(sudoku, pool_manager, [Workers])).

% the master requests a worker frm the manager,
% if no available worker: wait
% else if worker available: assign guess to be sloved
% unless master recieves a solution it will continue recursively through guesses
helperFunc(M, Ms) ->
  manager ! request,
  receive
    {solution, Solution} -> Solution;
    deadend -> io:format("A guess has been evluated and yielded no solution\n"),
               helperFunc (M, Ms);
    naw -> helperFunc (M, Ms);
           % case catch solve_refined(M) of
           %  {'EXIT',no_solution} ->
           %    psolve_one(Ms) ;
           %  Solution ->
           %    Solution
           %end,
      %psolve(Ms);
    {wa, W}   -> W ! M, psolve_one(Ms)
  end.

%input a list of gueses, recursively solves each of them
psolve_one([]) ->
  exit(no_solution);
psolve_one([M]) ->
  helperFunc( M, []);
psolve_one([M|Ms]) ->
  helperFunc(M, Ms)
.


%%%%%%%% benchmarks %%%%%%%%%%
-define(EXECUTIONS,5).
-define(PROCESSES,2).

bm(F) ->
  {T,_} = timer:tc(?MODULE,repeat,[F]),
  T/?EXECUTIONS/1000.

repeat(F) ->
  [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
  [{Name,bm(fun()->psolve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  initPs(?PROCESSES),
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).

%% check solutions for validity
valid_rows(M) ->
  lists:all(fun valid_row/1,M).

valid_row(Row) ->
  lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
  valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

