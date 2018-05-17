
on_exit(Fun) ->
    receive
      {'EXIT',Pid,Why} -> Fun(Why)
    end.

refineM(Parent, M) ->
  Parent !
    refine_rows(
      transpose(
        refine_rows(
          transpose(
            unblocks(
              refine_rows(
                blocks(M))))))).


manager(Parent,M) ->
  process_flag(trap_exit,true),
  spawn_link(sudoku, refineM, [Parent, M]),
  on_exit( (fun(T) -> Parent ! {xt, T} end)).

handle_recive(M) ->
  receive
    {xt, no_solution} -> io:format("***exit: no_solution \n",[]), exit(no_solution);
    {xt, Ms} -> io:format("***exit: ~p\n",[Ms]), handle_recive(M);
    Xs -> if M==Xs ->
      M;
            true ->
              refine(Xs)
          end end.

refine(M) ->
  spawn_link(sudoku, manager, [self(), M]),
  handle_recive(M).

