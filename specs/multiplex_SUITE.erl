-module(multiplex_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-compile(export_all).

-define(config(K,P), proplists:get_value(K,P)).

-define(sub_msg(App), {text, list_to_binary("sub,"++App)}).
-define(unsub_msg(App), {text, list_to_binary("unsub,"++App)}).

init_per_suite(Config) ->
  lager:start(),
  ulitos_app:set_var(?APP, default_app, true),
  Config.

end_per_suite(_) ->
  application:stop(lager),
  ok.

init_per_group(_, Config) ->  
  test_module:start(),
  deliverly:start(),
  test_app_app:start([],[]),
  timer:sleep(200),
  Client = test_module:create_client(mpx), 
  [{client, Client},Config].

end_per_group(_,Config) ->
  test_app_app:stop([]),
  deliverly:stop(),
  test_module:stop(),
  ok.

all() ->
  [
    {group, subscription_tests}
  ].

groups() ->
  [
    {
      subscription_tests, [sequence], 
      [
        subscribe_to_apps,
        unsubscribe_from_app,
        client_disconnected
      ]
    }
  ].


subscribe_to_apps(Config) ->
  Client = ?config(client, Config),
  test_module:received(Client, ?sub_msg("default")),
  1 = length(deliverly:connections_list(default)),
  test_module:received(Client, ?sub_msg("test_app_app")),
  1 = length(deliverly:connections_list(test_app_app)),
  1 = length(deliverly:connections_list(mpx)),
  3 = length(deliverly:connections_list()),
  ok.


unsubscribe_from_app(Config) ->
  Client = ?config(client, Config),
  test_module:received(Client, ?unsub_msg("default")),
  0 = length(deliverly:connections_list(default)),
  1 = length(deliverly:connections_list(test_app_app)),
  1 = length(deliverly:connections_list(mpx)),
  2 = length(deliverly:connections_list()),
  ok.

client_disconnected(Config) ->
  Client = ?config(client, Config),
  test_module:disconnect(Client),
  0 = length(deliverly:connections_list(default)),
  0 = length(deliverly:connections_list(test_app_app)),
  0 = length(deliverly:connections_list(mpx)),
  0 = length(deliverly:connections_list()),
  ok.