%% @doc Minimizing Delta Debugging Algorithm
%% 
-module(ddmin).

-export([ddmin/2]).

-type circumstance() :: term().
-type test() :: fun(([circumstance()]) -> pass | fail | unresolved).

-spec ddmin(test(), [circumstance()]) -> [circumstance()].
ddmin(Test, Circumstances) when is_list(Circumstances) ->
  %% Test function must fulfill the following preconditions
  pass = Test([]),
  fail = Test(Circumstances),  
  ddmin(Test, Circumstances, 2).

ddmin(Test, Circumstances, N) when N =< length(Circumstances), length(Circumstances) >= 2 ->
  %% split given circumstances into subsets and check if maybe a smaller subset fails as well
  Subsets = split(Circumstances, length(Circumstances) div N),
  %% pick smallest subset
  lists:min([ddmin(Test, Subset, Circumstances, N) || Subset <- Subsets]);
ddmin(_, Circumstances, _) ->
  Circumstances.

ddmin(Test, Subset, Circumstances, N) ->
  Complement = lists:subtract(Circumstances, Subset),
  case {Test(Subset), Test(Complement)} of
    {fail, _} -> ddmin(Test, Subset, 2);
    {_, fail} -> ddmin(Test, Complement, max(N-1, 2));
    _ when N < length(Circumstances) -> ddmin(Test, Circumstances, min(length(Circumstances), 2*N));
    _ -> Circumstances
  end.

split([], _Len) ->
  [];
split(Circumstances, Len) when Len =< length(Circumstances) ->
  {Subset, Rest} = lists:split(Len, Circumstances),
  [Subset | split(Rest, Len)];
split(Rest, _Len) ->
  [Rest].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_test() ->
  ?assertEqual([[1,2,3,4,5],[6,7,8,9,10]], split(lists:seq(1,10), 5)),
  ?assertEqual([[1,2,3],[4,5,6],[7,8,9],[10,11]], split(lists:seq(1,11), 3)).

foo([6,7|_]) -> throw(expected_error);
foo([H|T]) -> foo(T);
foo([]) -> done.

foo_test() ->
  Test = 
    fun(Circumstances) ->
      try 
        foo(Circumstances),
        pass
      catch _:expected_error -> fail;
            _:_ -> unresolved
      end
    end,
  ?assertEqual([6,7], ddmin(Test, [1,2,3,4,5,6,7,8,9,10])).


bar([2|_]) -> throw(expected_error);
bar([H|T]) -> bar(T);
bar([]) -> done.

bar_test() ->
  Test = 
    fun(Circumstances) ->
      try 
        bar(Circumstances),
        pass
      catch _:expected_error -> fail;
            _:_ -> unresolved
      end
    end,
  ?assertEqual([2], ddmin(Test, [1,2,3,4,5,6,7,8,9,10,11])).


foo_loop(N) when N < 0 -> 
  error(expected_error);
foo_loop(N) ->
  receive 
    yay -> foo_loop(N+1);
    nay -> foo_loop(N-1)
  after 200 -> done
  end.

foo_loop_test() ->
  Test = 
    fun(Circumstances) ->
        %% setup process under test
        Pid = spawn(fun() -> foo_loop(0) end),
        erlang:monitor(process, Pid),
        [Pid ! Circumstance || Circumstance <- Circumstances],
        receive    %% wait for expected error to happen
          {'DOWN', _, _, _, {expected_error, _}} -> fail;
          {'DOWN', _, _ ,_, normal} -> pass;
          _ -> unresolved
        end
    end,
  % this test checks for the smallest input sequence of messages to produce the expected error
  ?assertEqual([nay], ddmin(Test, [yay,yay,nay,yay,nay,nay,yay,nay,nay])).


-endif.



