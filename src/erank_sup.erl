-module(erank_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(erank, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, Name}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs)
    end, Pools),
    {ok, {{one_for_one, 5, 10}, PoolSpecs}}.
