-module(deliverly_http).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([add_routes/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  listener = http_listener,
  port,
  routes = [
    {"/ws/[:app/[...]]", ws_handler, []},
    {"/probe", probe_handler, []},
    {'_', notfound_handler, []}
  ]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_routes(Routes) ->
  gen_server:cast(?SERVER, {add_routes, Routes}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  Port = ?Config(http_port, 8081),
  ?D({<<"Start HTTP on port">>, Port}),
  self() ! start,
  {ok, #state{port = Port}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({add_routes, NewRoutes}, #state{listener = Listener, routes = Routes} = State) ->
  AllRoutes = NewRoutes ++ Routes,
  Dispatch = cowboy_router:compile([
    {'_', AllRoutes}
  ]),
  cowboy:set_env(Listener, dispatch, Dispatch),
  {noreply, State#state{routes = AllRoutes}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(start, #state{listener = Listener, port = Port, routes = Routes} = State) ->
  Dispatch = cowboy_router:compile([
    {'_', Routes}
  ]),
  cowboy:start_http(Listener, 100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------