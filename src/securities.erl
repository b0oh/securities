-module(securities).
-behaviour(gen_server).
-include("securities.hrl").

%% API
-export([start_link/0, parse_datetime/1, get_scales/0, get_papers/0,
         add_operation/4, get_operations/1, get_entries/4, shutdown/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SCALES, [{hour, 3600},
                 {day, 3600 * 24},
                 {week, 3600 * 24 * 7},
                 {month, fun month_scale/2}]).

%%%------------------------------------------------------------------------
%%% External API
%%%------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @spec (Dt :: string()) -> calendar:datetime()
%%
%% @doc parsing string formats like YYYY-MM-DD HH:mm or YYYY-MM-DD HH:mm:SS
%% MM — month, mm — minutes
%% @end
parse_datetime(Dt) when is_list(Dt) ->
  case io_lib:fread("~4d-~2d-~2d ~2d:~2d", Dt) of
    {ok, [Y, M, D, H, I], []} ->
      {{Y, M, D}, {H, I, 0}};
    _ ->
      {ok, [Y, M, D, H, I, S], []} = io_lib:fread("~4d-~2d-~2d ~2d:~2d:~2d", Dt),
      {{Y, M, D}, {H, I, S}}
  end.


%% @spec get_scales() -> [atom()]
get_scales() ->
  % proplists:get_keys(?SCALES). — it work but break ordering
  lists:map(fun({Name, _}) -> Name end, ?SCALES).


%% @spec get_papers() -> [string()]
get_papers() ->
  case gen_server:call(?MODULE, papers) of
    {ok, Papers} ->
      Papers
  end.


%% @spec (Paper, Dt, Price, Amount) -> ok when
%%       Paper    :: string(),
%%       Dt       :: string() | calendar:datetime() | integer(),
%%       Price    :: Currency,
%%       Amount   :: Currency,
%%       Currency :: float() | integer()
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
  ok = gen_server:call(?MODULE, {operation, Paper, #op{timestamp = Ts,
                                                       price = Price,
                                                       amount = Amount}}).


%% @spec (Paper :: string()) -> [op()]
get_operations(Paper) when is_list(Paper) ->
  case gen_server:call(?MODULE, {operation, Paper}) of
    {ok, Ops} ->
      Ops
  end.

%% @spec (Paper, StartDt, EndDt, Scale) -> [entry()] when
%%       Paper   :: string(),
%%       StartDt :: Dt,
%%       EndDt   :: Dt,
%%       Scale   :: atom(),
%%       Dt      :: string() | calendar:datetime() | integer()
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
  gen_server:call(?MODULE, stop).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

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

handle_call(stop, _From, State) ->
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


%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% @spec (Op :: op(), Ops :: [op()]) -> [op()]
%%
%% @doc Operations sorting by #op.timestamp (operated at)
%% late operation time in beginning, for fast insertion
%% expecting that the operations will come from early to late
%% @end
insert_operation(Op, Ops) ->
  insert_operation(Op, Ops, []).

%% traversing through list until operation time will not be bigger than
insert_operation(#op{timestamp = Ts} = Op,
                 [#op{timestamp = CompareTs} = CompareOp | Ops],
                 Acc) when Ts < CompareTs ->
  insert_operation(Op, Ops, [CompareOp | Acc]);
%% stop if list ended
insert_operation(Op, [], Acc) ->
  lists:reverse(Acc) ++ [Op];
%% otherwise reverse accumulator and push operation between acc and rest
insert_operation(Op, Ops, Acc) ->
  lists:reverse(Acc) ++ [Op] ++ Ops.


%% @spec (Ops :: [op()], StartTs :: integer(), EndTs :: integer()) -> [op()]
%%
%% @doc Taking operations between StartTs (include) and EndTs (not include)
%% [StartTs, EndTs)
%% BE CARE! return reverse sorted list: late is last, optimisation for make_entries
%% @end
filter_operations(Ops, StartTs, EndTs) ->
  filter_operations(Ops, StartTs, EndTs, []).

%% skipping operations until operation time bigger than EndTs
filter_operations([#op{timestamp = Ts} | Ops], StartTs, EndTs, Acc) when Ts >= EndTs ->
  filter_operations(Ops, StartTs, EndTs, Acc);
%% stop if list ended
filter_operations([], _, _, Acc) ->
  Acc;
%% return accumulator if operation time less than StartTs
filter_operations([#op{timestamp = Ts} | _], StartTs, _, Acc) when Ts < StartTs ->
  Acc;
%% keeping operatins between StartTs and EndTs
filter_operations([Op | Ops], StartTs, EndTs, Acc) ->
  filter_operations(Ops, StartTs, EndTs, [Op | Acc]).


%% @spec (Operations :: [op()], Scale :: atom()) -> [entry()]
%%
%% @doc Folding operations
%% may will be better to make it in two shots
%% 1) grouping operations by scale
%% 2) mapping groups to entries
%% slower but more obviously
%% @end
make_entries(Operations, Scale) ->
  lists:reverse(lists:foldl(fun(Op, Entries) -> fold_entry(Op, Entries, Scale) end, [], Operations)).

%% create first one
fold_entry(#op{timestamp = Ts, price = Price, amount = Amount}, [], Scale) ->
  [create_entry(scale_start(Ts, Scale), Price, Amount)];

fold_entry(#op{timestamp = OpTs, price = Price, amount = Amount} = Op,
           [#entry{start_timestamp = CurrentTs} = Entry | Entries], Scale) ->
  NextEntryTs = scale_next(CurrentTs, Scale),
  if
    % if operation time less than current entry time
    % add new operation to entry
    OpTs < NextEntryTs ->
      [update_entry(Entry, Op) | Entries];
    % or else create new entry
    true ->
      [create_entry(scale_start(OpTs, Scale), Price, Amount) | [Entry | Entries]]
  end.


%% @spec (Entry :: entry(), Operation :: op()) -> entry()
%%
%% @doc changing prices and summing amounts
update_entry(#entry{min_price = MinPrice, max_price = MaxPrice, amount = Amount} = Entry,
             #op{price = OpPrice, amount = OpAmount}) ->
  Entry#entry{end_price = OpPrice,
              min_price = erlang:min(OpPrice, MinPrice),
              max_price = erlang:max(OpPrice, MaxPrice),
              amount = Amount + OpAmount}.


%% @spec (Ts :: integer(), Price :: float(), Amount :: float()) -> entry()
create_entry(Ts, Price, Amount) ->
  #entry{start_timestamp = Ts,
         start_price = Price,
         end_price = Price,
         min_price = Price,
         max_price = Price,
         amount = Amount}.


%% @spec (Ts :: integer(), PropName :: atom()) -> integer()
%%
%% @doc return closest time before Ts by scale
scale_start(Ts, PropName) ->
  Scale = proplists:get_value(PropName, ?SCALES),
  case is_integer(Scale) of
    true ->
      Ts - (Ts rem Scale);
    false ->
      Scale(start, Ts)
  end.


%% @spec (Ts :: integer(), PropName :: atom()) -> integer()
%%
%% @doc return closest time after Ts by scale
scale_next(Ts, PropName) ->
  Scale = proplists:get_value(PropName, ?SCALES),
  case is_integer(Scale) of
    true ->
      Ts + Scale;
    false ->
      Scale(next, Ts)
  end.


%% start and next function for month scale
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
