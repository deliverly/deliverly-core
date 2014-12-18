-module(deliverly_sup).

-behaviour(supervisor).

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
    ?CHILD(deliverly_http,worker),
    ?CHILD(deliverly_server,worker),
    ?CHILD(deliverly_nodes, worker)
  ],
  {ok, { {one_for_one, 5, 10}, Children} }.

