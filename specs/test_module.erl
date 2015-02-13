-module(test_module).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(application).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% Application callbacks
-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Connection module functions
-export([send/2, close/1]).

-record(state, {}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  ok.

stop(_State) ->
  ?SERVER ! stop,
  ok.

send(_, _) -> ok.

close(_) -> ok.

init(_) ->
  {ok, #state{}}.

handle_call(_, _, State) -> {reply, ok, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(stop, State) -> {stop, normal, State};

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.