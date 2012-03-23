-module(securities_tests).
-include("securities.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPERATIONS, [{"echo", "2012-04-11 10:31", 1.21, 1200.0},
                     {"echo", "2012-04-11 10:52", 1.22, 400.0},
                     {"echo", "2012-04-11 11:16", 1.24, 1300.0},
                     {"echo", "2012-04-11 11:21", 1.23, 1000.0},
                     {"echo", "2012-04-11 11:48", 1.26, 1200.0},
                     {"echo", "2012-04-11 12:20", 1.25, 300.0},
                     {"echo", "2012-04-11 13:05", 1.22, 200.0},
                     {"echo", "2012-04-11 13:44", 1.23, 2000.0},
                     {"echo", "2012-04-11 14:01", 1.24, 1400.0},
                     {"echo", "2012-04-11 14:13", 1.25, 500.0},
                     {"echo", "2012-04-11 14:22", 1.24, 700.0},
                     {"echo", "2012-04-11 14:37", 1.23, 1100.0},
                     {"echo", "2012-04-11 14:39", 1.22, 1500.0},
                     {"echo", "2012-04-11 15:04", 1.22, 200.0},
                     {"echo", "2012-04-11 16:32", 1.24, 400.0},

                     {"entry_test", "0000-01-01 23:59:59", 1.0, 100.0},
                     {"entry_test", "0000-01-02 00:00", 1.0, 100.0},
                     {"entry_test", "0000-01-02 00:20", 1.0, 100.0},
                     {"entry_test", "0000-01-02 00:59:59", 1.0, 100.0},
                     {"entry_test", "0000-01-02 01:20", 1.0, 100.0},
                     {"entry_test", "0000-01-02 01:40", 1.0, 100.0},
                     {"entry_test", "0000-01-02 02:00", 1.0, 100.0},
                     {"entry_test", "0000-01-02 05:20", 1.0, 100.0},
                     {"entry_test", "0000-01-02 05:40", 1.0, 100.0},

                     {"month_test", "2012-04-11 10:31", 1.21, 1200.0},
                     {"month_test", "2012-04-29 12:20", 1.24, 400.0},
                     {"month_test", "2012-05-04 17:00", 1.26, 3000.0}]).

-define(ECHO_11_12_MAR_ENTRIES, [{"2012-04-11 10:00", 1.21, 1.22, 1.21, 1.22, 1600.0},
                                 {"2012-04-11 11:00", 1.24, 1.26, 1.23, 1.26, 3500.0},
                                 {"2012-04-11 12:00", 1.25, 1.25, 1.25, 1.25, 300.0},
                                 {"2012-04-11 13:00", 1.22, 1.23, 1.22, 1.23, 2200.0},
                                 {"2012-04-11 14:00", 1.24, 1.22, 1.22, 1.25, 5200.0},
                                 {"2012-04-11 15:00", 1.22, 1.22, 1.22, 1.22, 200.0},
                                 {"2012-04-11 16:00", 1.24, 1.24, 1.24, 1.24, 400.0}]).

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
  [parse_datetime_tests(),
   papers_tests(),
   operations_tests(),
   entries_tests(),
   entries_month_tests(),
   echo_papers_tests()].


%% Tests

parse_datetime_tests() ->
  [?_assertMatch({{2012, 4, 11}, {10, 42, 0}}, securities:parse_datetime("2012-04-11 10:42")),
   ?_assertMatch({{2012, 4, 11}, {10, 42, 32}}, securities:parse_datetime("2012-04-11 10:42:32")),
   ?_assertError({badmatch, _}, securities:parse_datetime("2012-04-11 10:42:12 secs")),
   ?_assertError({badmatch, _}, securities:parse_datetime("2012-4-11 10:01")),
   ?_assertError({badmatch, _}, securities:parse_datetime("2012-04-11 10:1")),
   ?_assertError(function_clause, securities:parse_datetime(not_string))].


papers_tests() ->
  [?_assertMatch(["echo", "entry_test", "month_test"], lists:sort(securities:get_papers())),
   ?_assertError({case_clause, {error, not_found}}, securities:get_operations("not exist"))].


operations_tests() ->
  %%
  %% ordering operations test here
  %%
  Paper = "operation_test",
  ZeroOp = lift_op({Paper, "0000-01-01 00:00:00", 2.0, 1500.0}),
  FirstOp = lift_op({Paper, "0000-01-01 00:10", 1.21, 400.0}),
  SecondOp = lift_op({Paper, "0000-01-01 00:20", 1.26, 900.0}),
  LastOp = lift_op({Paper, "0000-01-01 00:40", 1.24, 1200.0}),
  [%% gregorian seconds in time argument
   ?_assertMatch(ok, securities:add_operation(Paper, 10 * 60, 1.21, 400.0)),
   %% check
   ?_assertMatch(FirstOp, lists:nth(1, securities:get_operations(Paper))),
   %% datetime in time argument
   ?_assertMatch(ok, securities:add_operation(Paper, {{0, 1, 1}, {0, 40, 0}}, 1.24, 1200.0)),
   %% check: should be first, lates operations putting in begin
   ?_assertMatch(LastOp, lists:nth(1, securities:get_operations(Paper))),
   %% string in time argument
   ?_assertMatch(ok, securities:add_operation(Paper, "0000-01-01 00:20", 1.26, 900.0)),
   %% check: should be between 00:10 and 00:40
   ?_assertMatch(SecondOp, lists:nth(2, securities:get_operations(Paper))),
   %% integer converts to float automaticly
   ?_assertMatch(ok, securities:add_operation(Paper, "0000-01-01 00:00:00", 2, 1500)),
   ?_assert(4 == length(securities:get_operations(Paper))),
   %% check: should be in the end of operations because erliest
   ?_assertMatch(ZeroOp, lists:last(securities:get_operations(Paper))),

   %% wrong date format
   ?_assertError({badmatch, _}, securities:add_operation(Paper, "not a date", 0, 0)),
   %% wrong paper name format
   ?_assertError(function_clause, securities:add_operation(not_string, "0000-01-01 00:00:00", 0, 0)),
   %% wrong price argument
   ?_assertError(badarg, securities:add_operation(Paper, "0000-01-01 00:00:00", not_number, 0))].


entries_tests() ->
  FiveOclock = lift_entry({"0000-01-02 05:00", 1.0, 1.0, 1.0, 1.0, 200.0}),

  [%% gregorian seconds in args
   ?_assert(2 == length(securities:get_entries("entry_test", 3600 * 24, 3600 * 24 + 7200, hour))),
   %% datetime in args
   ?_assert(2 == length(securities:get_entries("entry_test", {{0, 1, 2}, {0, 0, 0}}, {{0, 1, 2}, {2, 0, 0}}, hour))),
   %% strings in args
   ?_assert(2 == length(securities:get_entries("entry_test", "0000-01-02 00:00", "0000-01-02 02:00", hour))),

   %% skip hours test, after 2 o'clock missing two hours,
   %% next entry should start from 5 o'clock
   ?_assertMatch(FiveOclock,
                 lists:last(securities:get_entries("entry_test", "0000-01-02 02:00", "0000-01-02 06:00", hour))),

   %% bad scale argument
   ?_assertError({badmatch, false}, securities:get_entries("entry_test", 0, 1, bad_scale)),
   %% start time should be less than end time
   ?_assertError(function_clause, securities:get_entries("entry_test", 1, 0, hour)),
   %% not exist paper name
   ?_assertError({case_clause, {error, not_found}}, securities:get_entries("not exist", 0, 1, hour)),
   %% bad paper name argument
   ?_assertError(function_clause, securities:get_entries(not_string, 0, 1, hour)),
   %% time arguments shold be same type
   ?_assertError(function_clause, securities:get_entries("entry_test", "0000-01-01 00:00", 3600, hour)),
   %% datetime parsing error
   ?_assertError({badmatch, _}, securities:get_entries("entry_test", "not a date", "not a date", hour))].


entries_month_tests() ->
  Entries = securities:get_entries("month_test", "2012-04-10 00:00", "2012-05-31 00:00", month),
  April = {"2012-04-01 00:00", 1.21, 1.24, 1.21, 1.24, 1600.0},
  May   = {"2012-05-01 00:00", 1.26, 1.26, 1.26, 1.26, 3000.0},
  [?_assertEqual(2, length(Entries)),
   entry_match(April, Entries),
   entry_match(May, Entries)].


echo_papers_tests() ->
  EchoOperations = lists:filter(fun({Paper, _, _, _}) -> Paper == "echo" end, ?OPERATIONS),

  StoredOperations = securities:get_operations("echo"),
  OperationTests = lists:map(fun(Op) -> operation_match(Op, StoredOperations) end, EchoOperations),

  Entries = securities:get_entries("echo", "2012-04-11 00:00", "2012-04-12 00:00", hour),
  EntryTests = lists:map(fun(Entry) -> entry_match(Entry, Entries) end, ?ECHO_11_12_MAR_ENTRIES),

  [?_assertEqual(length(EchoOperations), length(StoredOperations)),
   ?_assertEqual(length(?ECHO_11_12_MAR_ENTRIES), length(Entries)),
   OperationTests,
   EntryTests].


%% Internal functions

operation_match(Op, Operations) ->
  #op{timestamp = Timestamp} = Operation = lift_op(Op),
  ?_assertMatch(Operation, lists:keyfind(Timestamp, #op.timestamp, Operations)).


entry_match(RawEntry, Entries) ->
  #entry{start_timestamp = Timestamp} = Entry = lift_entry(RawEntry),
  ?_assertMatch(Entry, lists:keyfind(Timestamp, #entry.start_timestamp, Entries)).


add_operation({Paper, Datetime, Price, Amount}) ->
  securities:add_operation(Paper, Datetime, Price, Amount).


dt_to_sec(Datetime) ->
  calendar:datetime_to_gregorian_seconds(securities:parse_datetime(Datetime)).


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
