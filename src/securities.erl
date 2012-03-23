-module(securities).
-behaviour(gen_server).
-include("securities.hrl").

%% API
-export([start_link/0, parse_datetime/1, get_papers/0, add_operation/4, get_operations/1, get_entries/4, shutdown/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-define(SCALES, [{hour, 3600},
                 {day, 3600 * 24},
                 {week, 3600 * 24 * 7},
                 {month, fun month_scale/2}]).
%% Client API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


parse_datetime(Dt) when is_list(Dt) ->
  case io_lib:fread("~4d-~2d-~2d ~2d:~2d", Dt) of
    {ok, [Y, M, D, H, I], []} ->
      {{Y, M, D}, {H, I, 0}};
    _ ->
      {ok, [Y, M, D, H, I, S], []} = io_lib:fread("~4d-~2d-~2d ~2d:~2d:~2d", Dt),
      {{Y, M, D}, {H, I, S}}
  end.


get_papers() ->
  case gen_server:call(?MODULE, papers) of
    {ok, Papers} ->
      Papers
  end.


add_operation(Paper, Dt, Price, Amount) when is_list(Dt) ->
  add_operation(Paper, parse_datetime(Dt), Price, Amount);

add_operation(Paper, {{_,_,_},{_,_,_}} = Dt, Pr, Am) ->
  add_operation(Paper, calendar:datetime_to_gregorian_seconds(Dt), Pr, Am);

add_operation(Paper, Dt, Price, Amount) when is_integer(Price) orelse
                                             is_integer(Amount) ->
  add_operation(Paper, Dt, float(Price), float(Amount));

add_operation(Paper, Ts, Price, Amount) when is_list(Paper) andalso
                                             is_integer(Ts) andalso
                                             is_float(Price) andalso
                                             is_float(Amount) ->
  gen_server:call(?MODULE, {operation, Paper, #op{timestamp = Ts,
                                                  price = Price,
                                                  amount = Amount}}).


get_operations(Paper) when is_list(Paper) ->
  case gen_server:call(?MODULE, {operation, Paper}) of
    {ok, Ops} ->
      Ops
  end.


get_entries(Paper, StartDt, EndDt, Scale) when is_list(StartDt) andalso is_list(EndDt) ->
  get_entries(Paper,
              parse_datetime(StartDt),
              parse_datetime(EndDt),
              Scale);

get_entries(Paper, {{_, _, _}, {_, _, _}} = StartDt, {{_, _, _}, {_, _, _}} = EndDt, Scale) ->
  get_entries(Paper,
              calendar:datetime_to_gregorian_seconds(StartDt),
              calendar:datetime_to_gregorian_seconds(EndDt),
              Scale);

get_entries(Paper, StartTs, EndTs, Scale) when is_list(Paper) andalso
                                               is_integer(StartTs) andalso
                                               is_integer(EndTs) andalso
                                               StartTs < EndTs ->
  true = lists:member(Scale, proplists:get_keys(?SCALES)),
  case gen_server:call(?MODULE, {entries, Paper, StartTs, EndTs, Scale}) of
    {ok, Entries} ->
      Entries
  end.


shutdown() ->
  gen_server:call(?MODULE, terminate).


%% gen_server callbacks
init([]) ->
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  {ok, dict:new()}.


handle_call(papers, _From, Papers) ->
  {reply, {ok, dict:fetch_keys(Papers)}, Papers};

handle_call({operation, Paper, #op{} = Operation}, _From, Papers) ->
  Value = case dict:find(Paper, Papers) of
            {ok, Operations} ->
              insert_operation(Operation, Operations);
            error ->
              [Operation]
          end,
  {reply, ok, dict:store(Paper, Value, Papers)};

handle_call({operation, Paper}, _From, Papers) ->
  Res = case dict:find(Paper, Papers) of
          {ok, _} = Found ->
            Found;
          error ->
            {error, not_found}
        end,
  {reply, Res, Papers};

handle_call({entries, Paper, StartTimestamp, EndTimestamp, Scale}, _From, Papers) ->
  Res = case dict:find(Paper, Papers) of
          {ok, Operations} ->
            {ok, make_entries(filter_operations(Operations, StartTimestamp, EndTimestamp), Scale)};
          error ->
            {error, not_found}
        end,
  {reply, Res, Papers};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ignored_message, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  io:format("~p (~p) shutting down...", [?MODULE, self()]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

insert_operation(Op, Ops) ->
  insert_operation(Op, Ops, []).


insert_operation(#op{timestamp = Ts} = Op,
                 [#op{timestamp = CompareTs} = CompareOp | Ops],
                 Acc) when Ts < CompareTs ->
  insert_operation(Op, Ops, [CompareOp | Acc]);

insert_operation(Op, [], Acc) ->
  lists:reverse(Acc) ++ [Op];

insert_operation(Op, Ops, Acc) ->
  lists:reverse(Acc) ++ [Op] ++ Ops.


filter_operations(Ops, StartTs, EndTs) ->
  filter_operations(Ops, StartTs, EndTs, []).

%% @TODO: document [StartTs, EndTs), noted reversing

filter_operations([#op{timestamp = Ts} | Ops], StartTs, EndTs, Acc) when Ts >= EndTs ->
  filter_operations(Ops, StartTs, EndTs, Acc);

filter_operations([#op{timestamp = Ts} | _], StartTs, _, Acc) when Ts < StartTs ->
  Acc;

filter_operations([], _, _, Acc) ->
  Acc;

filter_operations([Op | Ops], StartTs, EndTs, Acc) ->
  filter_operations(Ops, StartTs, EndTs, [Op | Acc]).


make_entries(Operations, Scale) ->
  lists:reverse(lists:foldl(fun(Op, Entries) -> fold_entry(Op, Entries, Scale) end, [], Operations)).


fold_entry(#op{timestamp = Ts, price = Price, amount = Amount}, [], Scale) ->
  [create_entry(scale_start(Ts, Scale), Price, Amount)];

fold_entry(#op{timestamp = OpTs, price = Price, amount = Amount} = Op,
           [#entry{start_timestamp = CurrentTs} = Entry | Entries], Scale) ->
  NextEntryTs = scale_next(CurrentTs, Scale),
  if
    %% if operation time less than current entry time
    %% add new operation to entry
    OpTs < NextEntryTs ->
      [update_entry(Entry, Op) | Entries];
    %% or else create new entry
    true ->
      [create_entry(scale_start(OpTs, Scale), Price, Amount) | [Entry | Entries]]
  end.


update_entry(#entry{min_price = MinPrice, max_price = MaxPrice, amount = Amount} = Entry,
             #op{price = OpPrice, amount = OpAmount}) ->
  Entry#entry{end_price = OpPrice,
              min_price = erlang:min(OpPrice, MinPrice),
              max_price = erlang:max(OpPrice, MaxPrice),
              amount = Amount + OpAmount}.


create_entry(Ts, Price, Amount) ->
  #entry{start_timestamp = Ts,
         start_price = Price,
         end_price = Price,
         min_price = Price,
         max_price = Price,
         amount = Amount}.


scale_start(Ts, PropName) ->
  Scale = proplists:get_value(PropName, ?SCALES),
  case is_integer(Scale) of
    true ->
      Ts - (Ts rem Scale);
    false ->
      Scale(start, Ts)
  end.


scale_next(Ts, PropName) ->
  Scale = proplists:get_value(PropName, ?SCALES),
  case is_integer(Scale) of
    true ->
      Ts + Scale;
    false ->
      Scale(next, Ts)
  end.


month_scale(start, Ts) ->
  {{Y, M, _}, _} = calendar:gregorian_seconds_to_datetime(Ts),
  calendar:datetime_to_gregorian_seconds({{Y, M, 1}, {0, 0, 0}});

month_scale(next, Ts) ->
  {{Y, M, _}, _} = calendar:gregorian_seconds_to_datetime(Ts),
  NextMonth = case M + 1 > 12 of
                true -> 1;
                false -> M + 1
              end,
  calendar:datetime_to_gregorian_seconds({{Y, NextMonth, 1}, {0, 0, 0}}).
