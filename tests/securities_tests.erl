-module(securities_tests).
-include("operation.hrl").
-include_lib("eunit/include/eunit.hrl").


test_data() ->
  [{"echo", "2012-04-11 10:31", 1.21, 1200},
   {"echo", "2012-04-11 10:52", 1.22, 400},
   {"echo", "2012-04-11 11:16", 1.24, 1300},
   {"echo", "2012-04-11 11:21", 1.23, 1000},
   {"echo", "2012-04-11 11:48", 1.26, 1200},
   {"echo", "2012-04-11 12:20", 1.25, 300},
   {"echo", "2012-04-11 13:05", 1.22, 200},
   {"echo", "2012-04-11 13:44", 1.23, 2000},
   {"echo", "2012-04-11 14:01", 1.24, 1400},
   {"echo", "2012-04-11 14:13", 1.25, 500},
   {"echo", "2012-04-11 14:22", 1.24, 700},
   {"echo", "2012-04-11 14:37", 1.23, 1100},
   {"echo", "2012-04-11 14:39", 1.22, 1500},
   {"echo", "2012-04-11 15:04", 1.22, 200},
   {"echo", "2012-04-11 16:32", 1.24, 400},
   {"pogo", "2012-04-11 10:40", 0.56, 100},
   {"pogo", "2012-04-11 15:21", 0.54, 120}].


setup() ->
  securities:start_link(),
  Data = test_data(),
  lists:foreach(fun add_operation/1, Data),  
  Data.


cleanup() ->
  securities:shutdown().


securities_test_() ->
  {setup, fun() -> setup() end,
   fun(_) -> cleanup() end,
   fun securities_tests/1}.


securities_tests(Data) ->
  EchoData = lists:filter(fun({Paper, _, _, _}) -> Paper == "echo" end, Data),
  [echo_tests(EchoData)].


echo_tests(EchoData) ->
  Operations = securities:get_operations("echo"),
  Tests = lists:map(fun(Op) -> operation_match(Op, Operations) end, EchoData),
  [?_assert(lists:member("echo", securities:get_papers())),
   ?_assertEqual(length(EchoData), length(Operations)),
   Tests].


operation_match(Op, Operations) ->
  #op{timestamp = Timestamp} = Operation = lift_op(Op),
  ?_assertMatch(Operation, lists:keyfind(Timestamp, #op.timestamp, Operations)).


parse_datetime(Datetime) ->
  {ok, [Y, M, D, H, I], _} = io_lib:fread("~4d-~2d-~2d ~2d:~2d", Datetime),
  {{Y, M, D}, {H, I, 0}}.


add_operation(Data) ->
  {Paper, Datetime, Price, Amount} = Data,
  securities:add_operation(Paper, parse_datetime(Datetime), Price, Amount).


lift_op({_Paper, Datetime, Price, Amount}) ->
  Timestamp = calendar:datetime_to_gregorian_seconds(parse_datetime(Datetime)),
  #op{timestamp = Timestamp,
      price = Price,
      amount = Amount}.
