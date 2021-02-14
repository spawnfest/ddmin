%% @doc Naive recorder of messages to a process using dbg tracer.
%%      Records all received messages to a pid and 
%%      stores them ordered by arrival in an ETS.
-module(naive_recorder).

-export([ record/1
        , pause/1
        , get_recorded_messages/1
        , stop/1
        ]).

-record(recorder, {observe_pid, table_id}).

-spec record(pid()) -> #recorder{}.
record(Pid) when is_pid(Pid) ->
  dbg:stop_clear(),
  Tid = ets:new(?MODULE, [ordered_set, public]),
  dbg:tracer(process, {
      fun({trace_ts,_,'receive', ReceivedMsg, TS}, Tab) ->
          ets:insert(Tab, {TS, ReceivedMsg}),
          Tab;
         (_, Tab) -> Tab
      end,
      Tid}),
  {ok, [{matched, _, 1}]} = dbg:p(Pid, [r, timestamp]),
  #recorder{observe_pid=Pid, table_id=Tid}.

-spec pause(#recorder{}) -> ok.
pause(#recorder{}) ->
  dbg:stop().

-spec stop(#recorder{}) -> ok.
stop(#recorder{table_id=Tid}) ->
  ets:delete(Tid),
  dbg:stop_clear().

-spec get_recorded_messages(#recorder{}) -> [term()] | [].
get_recorded_messages(#recorder{table_id=Tid}) ->
  ets:foldr(fun({_TS, Msg}, Acc) -> [Msg | Acc] end, [], Tid).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

foo_loop() ->
  receive 
    {hello,_} -> foo_loop();
    bye -> ok;
    _ -> throw(expected_error)
  end.

simple_test() ->
  Pid = spawn(fun foo_loop/0),
  Rec = naive_recorder:record(Pid),
  Pid ! {hello,2},
  Pid ! {hello,1},
  Pid ! {hello,3},
  Pid ! bye,
  naive_recorder:pause(Rec),
  ?assertEqual([{hello,2}, {hello,1}, {hello,3}, bye], naive_recorder:get_recorded_messages(Rec)),
  naive_recorder:stop(Rec),
  ok.

-endif.
