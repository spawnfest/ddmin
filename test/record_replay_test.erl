-module(record_replay_test).

-include_lib("eunit/include/eunit.hrl").

foo_loop() ->
  receive 
    {hello,_} -> foo_loop();
    _ -> error(expected_error)
  after 100 -> ok
  end.

setup_fun() -> 
  spawn(fun() -> foo_loop() end).

test_fun(Messages) when is_list(Messages) ->
  Pid = setup_fun(),
  erlang:monitor(process, Pid),
  [Pid ! Msg || Msg <- Messages],
  receive    %% wait for expected error to happen
    {'DOWN', _, _, _, {expected_error, _}} -> fail;
    {'DOWN', _, _, _, normal} -> pass;
    _ -> unresolved
  end.

record_replay_test() ->
  Pid = setup_fun(),
  Recorder = naive_recorder:record(Pid),
  Pid ! {hello, 1},
  Pid ! {hello, 2},
  Pid ! {hello, 3},
  Pid ! {hello, 4},
  Pid ! {hello, 5},
  Pid ! boom,
  naive_recorder:pause(Recorder),
  ?assertEqual([boom], ddmin:ddmin(fun test_fun/1, naive_recorder:get_recorded_messages(Recorder))),
  naive_recorder:stop(Recorder).
