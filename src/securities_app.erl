-module(securities_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).


start() ->
  application:start(securities).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    securities_app_sup:start_link().

stop(_State) ->
    ok.
