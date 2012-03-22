-module(securities_tests).
-include("securities.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPERATIONS, [{"echo", "2012-04-11 10:31", 1.21, 1200},
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
                     {"echo", "2012-04-11 16:32", 1.24, 400}]).

-define(ENTRIES, [{"2012-04-11 10:00", 1.21, 1.22, 1.21, 1.22, 1600},
                  {"2012-04-11 11:00", 1.24, 1.26, 1.23, 1.26, 3500},
                  {"2012-04-11 12:00", 1.25, 1.25, 1.25, 1.25, 300},
                  {"2012-04-11 13:00", 1.22, 1.23, 1.22, 1.23, 2200},
                  {"2012-04-11 14:00", 1.24, 1.22, 1.22, 1.25, 5200},
                  {"2012-04-11 15:00", 1.22, 1.22, 1.22, 1.22, 200},
                  {"2012-04-11 16:00", 1.24, 1.24, 1.24, 1.24, 400}]).

-export([load_operations/0]).


load_operations() ->
  lists:foreach(fun add_operation/1, ?OPERATIONS).




setup() ->
  securities:start_link(),
  load_operations(),
  ok.


cleanup() ->
  securities:shutdown().


securities_test_() ->
  {setup, fun() -> setup() end,
   fun(_) -> cleanup() end,
   fun(_) -> securities_tests() end}.


securities_tests() ->
  [papers_tests(), echo_papers_tests()].


papers_tests() ->
  [?_assertMatch(["echo"], securities:get_papers()),
   ?_assertError({case_clause, {error, not_found}}, securities:get_operations("unexistance"))].


echo_papers_tests() ->
  EchoOperations = lists:filter(fun({Paper, _, _, _}) -> Paper == "echo" end, ?OPERATIONS),
  StoredOperations = securities:get_operations("echo"),
  OperationTests = lists:map(fun(Op) -> operation_match(Op, StoredOperations) end, EchoOperations),
  Entries = securities:get_entries("echo",
                                   parse_datetime("2012-04-11 00:00"),
                                   parse_datetime("2012-04-12 00:00"),
                                   hour),
  EntryTests = lists:map(fun(Entry) -> entry_match(Entry, Entries) end, ?ENTRIES),
  [?_assertEqual(length(EchoOperations), length(StoredOperations)),
   ?_assertEqual(length(?ENTRIES), length(Entries)),
   OperationTests,
   EntryTests].


%% Internal functions

operation_match(Op, Operations) ->
  #op{timestamp = Timestamp} = Operation = lift_op(Op),
  ?_assertMatch(Operation, lists:keyfind(Timestamp, #op.timestamp, Operations)).


entry_match(RawEntry, Entries) ->
  #entry{start_timestamp = Timestamp} = Entry = lift_entry(RawEntry),
  ?_assertMatch(Entry, lists:keyfind(Timestamp, #entry.start_timestamp, Entries)).


parse_datetime(Datetime) ->
  {ok, [Y, M, D, H, I], _} = io_lib:fread("~4d-~2d-~2d ~2d:~2d", Datetime),
  {{Y, M, D}, {H, I, 0}}.


add_operation({Paper, Datetime, Price, Amount}) ->
  securities:add_operation(Paper, parse_datetime(Datetime), Price, Amount).


dt_to_sec(Datetime) ->
  calendar:datetime_to_gregorian_seconds(parse_datetime(Datetime)).


lift_op({_Paper, Datetime, Price, Amount}) ->
  #op{timestamp = dt_to_sec(Datetime),
      price = Price,
      amount = Amount}.

lift_entry({StartDatetime, StartPrice, EndPrice, MinPrice, MaxPrice, Amount}) ->
  #entry{start_timestamp = dt_to_sec(StartDatetime),
         start_price = StartPrice,
         end_price = EndPrice,
         min_price = MinPrice,
         max_price = MaxPrice,
         amount = Amount}.
