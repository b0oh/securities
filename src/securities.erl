-module(securities).
-behaviour(gen_server).
-include("operation.hrl").

%% API
-export([start_link/0, get_papers/0, add_operation/4, get_operations/1, shutdown/0]).
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
              [Operation | Operations];
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
