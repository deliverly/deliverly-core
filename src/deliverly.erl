%% Copyright
-module(deliverly).
-author("palkan").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").
-define(APPS, [lager, cowboy]).

%% ------------------------------------------------------------------
%% Common Application Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0, upgrade/0, ping/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  register_handler/2,
  connections_list/0,
  connections_list/1,
  apps_list/0
]).

-define(SERVER, deliverly_server).

start() ->
  ulitos_app:ensure_started(?APPS),
  application:start(deliverly).

stop() ->
  application:stop(deliverly).

upgrade() ->
 ulitos_app:reload(deliverly),
 ok.
 
ping() ->
  pong.


%% @doc
%% Add App to known apps
%% @end

-spec register_handler(App::atom(), Handler::atom()) -> ok | {error, Reason::atom()}.

register_handler(App, Handler) ->
  gen_server:call(?SERVER, {register_handler, App, Handler}).

%% @doc
%% Return all active clients.
%% @end

-spec connections_list() -> list(client()).

connections_list() ->
  ets:tab2list(?APP).

%% @doc
%% Return all registered apps.
%% @end

-spec apps_list() -> list(atom()).

apps_list() ->
  [App || {App,_} <- ets:tab2list(?ETS_HANDLERS)].

%% @doc
%% Return all active clients for App.
%% @end

-spec connections_list(App::atom()) -> list().

connections_list(App) ->
  ets:match_object(?APP, #de_client{app = App, _ = '_'}).



