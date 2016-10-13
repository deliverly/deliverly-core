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
  RPoolSpecs = redis_pool_specs(?Config(use_redis, true)),
  {ok, { {one_for_one, 5, 10}, Children ++ RPoolSpecs} }.

  %%internal

redis_pool_specs(false) ->
  [];

redis_pool_specs(_) ->
  RSizeArgs = [
    {size, 5},
    {max_overflow, 10}
  ],
  RPoolArgs = [
    {name, {local, ?R_POOL}},
    {worker_module, eredis}
  ] ++ RSizeArgs,
  RedisOpt = [
    {host, ?Config(redis_host, "127.0.0.1")},
    {port, ?Config(redis_port, 6379)},
    {database, ?Config(redis_database, 0)},
    {password, ?Config(redis_password, "")},
    {reconnect_sleep, ?Config(redis_reconnect_sleep, 100)},
    {connect_timeout, ?Config(redis_connect_timeout, 5000)}
  ],
  [poolboy:child_spec(?R_POOL, RPoolArgs, RedisOpt)].

