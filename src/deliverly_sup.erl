-module(deliverly_sup).

-behaviour(supervisor).

-include_lib("deliverly/include/priv.hrl").

%% API
-export([start_link/0, start_server/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Module) ->
  supervisor:start_child(?MODULE, ?CHILD(Module, worker)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Children = [
    ?CHILD(deliverly_http, worker),
    ?CHILD(deliverly_server, worker),
    ?CHILD(deliverly_nodes, worker)
  ],
  RSizeArgs = [
    {size, 5},
    {max_overflow, 10}
  ],
  RPoolArgs = [
    {name, {local, ?R_POOL}},
    {worker_module, eredis}
  ] ++ RSizeArgs,
  RedisOpt = [
    {host, ulitos_app:get_var(?APP, redis_host, "127.0.0.1")},
    {port, ulitos_app:get_var(?APP, redis_port, 6379)},
    {database, ulitos_app:get_var(?APP, redis_database, 0)},
    {password, ulitos_app:get_var(?APP, redis_password, "")},
    {reconnect_sleep, ulitos_app:get_var(?APP, redis_reconnect_sleep, 100)},
    {connect_timeout, ulitos_app:get_var(?APP, redis_connect_timeout, 5000)}
  ],
  RPoolSpecs = [poolboy:child_spec(?R_POOL, RPoolArgs, RedisOpt)],
  {ok, { {one_for_one, 5, 10}, Children ++ RPoolSpecs} }.

