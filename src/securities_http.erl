-module(securities_http).
-behaviour(gen_server).
-include("securities.hrl").
-include("yaws_api.hrl").

-export([out/1]).
%% API
-export([start_link/0, shutdown/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%------------------------------------------------------------------------
%%% External API
%%%------------------------------------------------------------------------

out(Arg) ->
  Uri = yaws_api:request_url(Arg),
  Path = string:tokens(Uri#url.path, "/"),
  Method = (Arg#arg.req)#http_request.method,
  Req = case Method of
          'POST' -> yaws_api:parse_post(Arg);
          _      -> yaws_api:parse_query(Arg)
        end,
  handle(Path, Method, Req).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


shutdown() ->
  gen_server:call(?MODULE, stop).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  {ok, Port} = application:get_env(securities, http_port),
  {ok, Name} = application:get_env(securities, http_servername),
  {ok, Listen} = application:get_env(securities, http_listen),
  yaws:start_embedded("priv/www", [{port, Port},
                                   {servername, Name},
                                   {dir_listings, true},
                                   {listen, Listen},
                                   {appmods, [{"api", ?MODULE}]},
                                   {flags, [{auth_log, false}, {access_log, false}]}]),
  erlang:monitor(process, yaws_server),
  {ok, []}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {stop, {unknown_call, _Request}, State}.


handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.


handle_info({'DOWN', _, _, {yaws_server, _}, _}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.


terminate(_Reason, _State) ->
  io:format("~p (~p) shutting down...", [?MODULE, self()]),
  yaws:stop(),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).


response(Code, Type, Body) ->
  [{status, Code},
   {content, Type, Body}].


json(Arg) ->
  response(200, "application/json", json2:encode(Arg)).


handle(["api", "papers"], 'GET', _Req) ->
  json(securities:get_papers());

handle(["api", "scales"], 'GET', _Req) ->
  json(lists:map(fun atom_to_list/1, securities:get_scales()));

handle(["api", "operation"], 'POST', Req) ->
  Paper = proplists:get_value("name", Req),
  Dt = proplists:get_value("time", Req),
  Price = str_to_float(proplists:get_value("price", Req)),
  Amount = str_to_float(proplists:get_value("amount", Req)),
  ok = securities:add_operation(Paper, Dt, Price, Amount),
  json("ok");
  
handle(["api", "entries"], 'GET', Req) ->
  Paper = proplists:get_value("name", Req),
  StartDt = proplists:get_value("from", Req),
  EndDt = proplists:get_value("to", Req),
  Scale = list_to_atom(proplists:get_value("scale", Req)),
  json(lists:map(fun entry_to_struct/1, securities:get_entries(Paper, StartDt, EndDt, Scale)));

handle(Path, Method, _Req) ->
  response(404, "text/plain", format("Unknown api action: ~p [~p]", [string:join(Path, "/"), Method])).


str_to_float(Str) ->
  case string:to_float(Str) of
    {error, no_float} ->
      float(list_to_integer(Str));
    {Float, []} ->
      Float
  end.


entry_to_struct(#entry{start_timestamp = StartTs,    amount    = Amount,
                       start_price     = StartPrice, end_price = EndPrice,
                       min_price       = MinPrice,   max_price = MaxPrice}) ->
  {{Y, M, D}, {H, I, S}} = calendar:gregorian_seconds_to_datetime(StartTs),
  {struct, [{startTtime, format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, I, S])},
            {startPrice, StartPrice}, {endPrice, EndPrice},
            {minPrice, MinPrice}, {maxPrice, MaxPrice},
            {amount, Amount}]}.
                
