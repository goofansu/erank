-module(erank_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erank_sup:start_link().

stop(_State) ->
    ok.

start() ->
    application:start(eredis_pool),
    application:start(erank),
    ok.

stop() ->
    application:stop(eredis_pool),
    application:stop(erank),
    ok.
