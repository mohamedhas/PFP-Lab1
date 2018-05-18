-module(sudoku).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% %% generators

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

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

%% decide whether a position is safe
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

on_exit(Fun) ->
    receive
      {'EXIT',Pid,Why} -> Fun(Why)
    end.

receiveM(Ref) ->
  receive
    no_solution ->
      exit(no_solution);
    {Ref, Solution} ->
      Solution
  end.

handle_recive(M) ->
  receive
    {xt, no_solution} -> io:format("***exit: no_solution \n",[]), exit(no_solution);
    {xt, Ms} -> io:format("***exit: ~p\n",[Ms]), handle_recive(M);
    Xs -> if M==Xs ->
      M;
            true ->
              refine(Xs)
          end end.

refine_rows(M) ->
  whereis(manager1) ! {self(), M},
  NewM = receiveM('ref1') ++ receiveM('ref2'),
  io:format("***Matrix: ~p\n",[NewM]),
  NewM.

prefine_rows(M) ->
  lists:map(fun refine_row/1,M).

worker() ->
  receive {Ref, Parent, M} ->
    case catch prefine_rows(M) of
        {'EXIT',no_solution} ->
          Parent ! no_solution;
      {'EXIT',Ms} -> io:format("***--exit: ~p\n",[Ms])
        ;
        Solution ->
            Parent ! {Ref, Solution}
      end
  end,
  worker().

manager() ->
  %%process_flag(trap_exit,true),
  %%spawn_link(sudoku, refineM, [Parent, M]),
  receive
    {Parent, M} -> whereis(worker1) ! {'ref1', Parent, lists:sublist(M,1,5)},
                   whereis(worker2) ! {'ref2', Parent, lists:sublist(M,6,4)}
  end,
  manager().
  %%on_exit( (fun(T) -> Parent ! {xt, T} end)).


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

refine_row(Row, Parent) ->
  Entries = entries(Row),
  NewRow =
    [if is_list(X) ->
      case X--Entries of
        [] ->
          [];
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
      Parent ! no_solution
  end.

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

%% solve a puzzle
solve(M) ->
    Solution = solve_refined(refine(fill(M))),
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

initPs() ->
  register(worker1,  spawn_link(sudoku, worker,[])),
  register(worker2,  spawn_link(sudoku, worker,[])),
  register(manager1, spawn_link(sudoku, manager,[])).


%%%%%%% benchmarks %%%%%%%

-define(EXECUTIONS,5).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  initPs(),
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).


%% check solutions for validity
valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).


