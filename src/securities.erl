-module(securities).
-behaviour(gen_server).
-include("securities.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

%% API
-export([start_link/0, get_papers/0, add_operation/4, get_operations/1, get_entries/4, shutdown/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%% Client API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_papers() ->
  case gen_server:call(?MODULE, papers) of
    {ok, Papers} ->
      Papers
  end.


add_operation(Paper, Datetime, Price, Amount) ->
  Timestamp = calendar:datetime_to_gregorian_seconds(Datetime),
  gen_server:call(?MODULE, {operation, Paper, #op{timestamp = Timestamp,
                                                  price = Price,
                                                  amount = Amount}}).


get_operations(Paper) ->
  case gen_server:call(?MODULE, {operation, Paper}) of
    {ok, Ops} ->
      Ops
  end.


get_entries(Paper, StartDatetime, EndDatetime, Scale) ->
  StartTimestamp = calendar:datetime_to_gregorian_seconds(StartDatetime),
  EndTimestamp = calendar:datetime_to_gregorian_seconds(EndDatetime),
  case gen_server:call(?MODULE, {entries, Paper, StartTimestamp, EndTimestamp, Scale}) of
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

%% insert_operation(Op, []) ->
%%   ?D({insert, last, Op}),
%%   [Op];

%% insert_operation(#op{timestamp = Ts} = Op, [#op{timestamp = CompareTs} = CompareOp | Operations]) when Ts > CompareTs ->
%%   ?D({insert, more, Op}),
%%   [CompareOp | insert_operation(Op, Operations)];

%% insert_operation(Op, Operations) ->
%%   ?D({insert, gotcha, Op}),
%%   [Op | Operations].

insert_operation(Op, Ops) ->
  insert_operation(Op, Ops, []).


insert_operation(#op{timestamp = Ts} = Op,
                 [#op{timestamp = CompareTs} = CompareOp | Ops],
                 Acc) when Ts < CompareTs ->
  insert_operation(Op, Ops, [CompareOp | Acc]);

insert_operation(Op, [], Acc) ->
  [Op | Acc];

insert_operation(Op, Ops, Acc) ->
  lists:reverse(Acc) ++ [Op] ++ Ops.



%% filter_operations(Operations, StartTimestamp, EndTimestamp) ->
%%   lists:filter(fun(#op{timestamp = Timestamp}) ->
%%                    Timestamp >= StartTimestamp andalso Timestamp =< EndTimestamp
%%                end, Operations).

filter_operations(Ops, StartTs, EndTs) ->
  filter_operations(Ops, StartTs, EndTs, []).


filter_operations([#op{timestamp = Ts} | Ops], StartTs, EndTs, Acc) when Ts > EndTs ->
  filter_operations(Ops, StartTs, EndTs, Acc);

filter_operations([#op{timestamp = Ts} | _], StartTs, _, Acc) when Ts < StartTs ->
  Acc;

filter_operations([], _, _, Acc) ->
  Acc;

filter_operations([Op | Ops], StartTs, EndTs, Acc) ->
  filter_operations(Ops, StartTs, EndTs, [Op | Acc]).



make_entries(Operations, _Scale) ->
  lists:reverse(lists:foldl(fun fill_entry/2, [], Operations)).


fill_entry(#op{timestamp = Timestamp, price = Price, amount = Amount}, []) ->
  {{Y, M, D}, {H, _, _}} = calendar:gregorian_seconds_to_datetime(Timestamp),
  [#entry{start_timestamp = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, 0, 0}}),
          start_price = Price,
          end_price = Price,
          min_price = Price,
          max_price = Price,
          amount = Amount}];

fill_entry(#op{timestamp = OpTimestamp, price = OpPrice, amount = OpAmount},
           [#entry{start_timestamp = EntryTimestamp,
                   min_price = MinPrice,
                   max_price = MaxPrice,
                   amount = EntryAmount} = Entry
            | Tail] = Entries) ->
  case OpTimestamp < (EntryTimestamp + 3600) of
    true ->
      NewEntry = Entry#entry{end_price = OpPrice,
                             min_price = lists:min([OpPrice, MinPrice]),
                             max_price = lists:max([OpPrice, MaxPrice]),
                             amount = EntryAmount + OpAmount},
      [NewEntry | Tail];
    false ->
      [#entry{start_timestamp = EntryTimestamp + 3600,
              start_price = OpPrice,
              end_price = OpPrice,
              min_price = OpPrice,
              max_price = OpPrice,
              amount = OpAmount} | Entries]
  end.
