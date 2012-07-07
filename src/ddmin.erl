%% Minimizing Delta Debugging Algorithm
%%
-module(ddmin).

-export([ddmin/2]).


-type circumstance() :: term().
-type test() :: fun(([circumstance()]) -> pass | fail | unresolved).

-spec ddmin(test(), [circumstance()]) -> [circumstance()].
ddmin(Test, Circumstances) when is_list(Circumstances) ->
  fail = Test(Circumstances),  %% precondition
  ddmin(Test, Circumstances, 2).

ddmin(Test, Circumstances, N) when N =< length(Circumstances), length(Circumstances) >= 2 ->
  Subsets = split(Circumstances, length(Circumstances) div N, []),
  lists:min([ddmin(Test, Subset, Circumstances, N) || Subset <- Subsets]).

ddmin(Test, Subset, Circumstances, N) ->
  Complement = lists:subtract(Circumstances, Subset),
  case {Test(Subset), Test(Complement)} of
    {fail, _} -> ddmin(Test, Subset, 2);
    {_, fail} -> ddmin(Test, Complement, max(N-1, 2));
    _ when N < length(Circumstances) -> ddmin(Test, Circumstances, min(length(Circumstances), 2*N));
    _ -> Circumstances
  end.

split(Circumstances, Len, Acc) when Len =< length(Circumstances) ->
  {Subset, Rest} = lists:split(Len, Circumstances),
  [Subset | split(Rest, Len, Acc)];
split(_Circumstances, _Len, Acc) ->
  Acc.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_test() ->
  ?assert(split(lists:seq(1,10), 5, []) == [[1,2,3,4,5],[6,7,8,9,10]]).

derp([6,7|_]) -> throw(expected_error);
derp([H|T]) -> derp(T);
derp([]) -> done.

simple_test() ->
  Test = 
    fun(Input) ->
      try 
        derp(Input),
        pass
      catch _:expected_error -> fail;
            _:_ -> unresolved
      end
    end,
  ?assert(ddmin(Test, [1,2,3,4,5,6,7,8,9,10]) =:= [6,7]).

-endif.



