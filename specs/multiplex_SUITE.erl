-module(multiplex_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-compile(export_all).

-define(config(K,P), proplists:get_value(K,P)).

-define(sub_msg(App), {text, list_to_binary(App++",sub")}).
-define(unsub_msg(App), {text, list_to_binary(App++",unsub")}).
-define(msg(App, Data), {text, list_to_binary(App++","++Data)}).

init_per_suite(Config) ->
  lager:start(),
  ulitos_app:set_var(?APP, default_app, []),
  Config.

end_per_suite(_) ->
  application:stop(lager),
  ok.

init_per_group(messages, Config) ->
  Config_ = init_per_group(subscription, Config),
  Client = ?config(client, Config_),
  {ok, _} = test_module:received(Client, ?sub_msg("default")),
  {ok, _} = test_module:received(Client, ?sub_msg("test_app_app")),
  Config_;

init_per_group(send_messages, Config) ->
  Config;

init_per_group(subscription, Config) ->  
  Config_ = init_per_group(false, Config),
  Client = test_module:create_client(mpx), 
  [{client, Client},Config_];

init_per_group(_, Config) ->
  test_module:start(),
  deliverly:start(),
  test_app_app:start([],[]),
  timer:sleep(200),
  Config.

end_per_group(send_messages, Config) ->
  Config;

end_per_group(_,_Config) ->
  timer:sleep(200),
  test_app_app:stop([]),
  deliverly:stop(),
  test_module:stop(),
  ok.

all() ->
  [
    {group, subscription},
    {group, messages},
    {group, disconnect},
    {group, unsubscribed}
  ].

groups() ->
  [
    {
      subscription, [sequence], 
      [
        subscribe_to_apps,
        unsubscribe_from_app  
      ]
    },
    {
      disconnect, [shuffle],
      [
        client_disconnected_with_apps,
        client_disconnected_without_apps
      ]
    },
    {
      messages, [sequence],
      [
        {group, send_messages},
        validate_messages
      ]
    },
    {
      send_messages, [shuffle, parallel, {repeat, 5}],
      [
        send_message_to_default,
        send_message_to_test_app
      ]
    },
    {
      unsubscribed, [shuffle],
      [
        send_message_without_subscription,
        send_message_after_unsubscribe
      ]
    }
  ].


subscribe_to_apps(Config) ->
  Client = ?config(client, Config),
  {ok, _} = test_module:received(Client, ?sub_msg("default")),
  1 = length(deliverly:connections_list(default)),
  {ok, _} = test_module:received(Client, ?sub_msg("test_app_app")),
  1 = length(deliverly:connections_list(test_app_app)),
  1 = length(deliverly:connections_list(mpx)),
  3 = length(deliverly:connections_list()),
  ok.


unsubscribe_from_app(Config) ->
  Client = ?config(client, Config),
  {ok, _} = test_module:received(Client, ?unsub_msg("default")),
  0 = length(deliverly:connections_list(default)),
  1 = length(deliverly:connections_list(test_app_app)),
  1 = length(deliverly:connections_list(mpx)),
  2 = length(deliverly:connections_list()),
  ok.

client_disconnected_with_apps(Config) ->
  Client = test_module:create_client(mpx),
  {ok, _} = test_module:received(Client, ?sub_msg("default")),
  {ok, _} = test_module:received(Client, ?sub_msg("test_app_app")),
  1 = length(deliverly:connections_list(default)),
  1 = length(deliverly:connections_list(test_app_app)),
  1 = length(deliverly:connections_list(mpx)),  
  ok = test_module:disconnect(Client),
  timer:sleep(100),
  0 = length(deliverly:connections_list(default)),
  0 = length(deliverly:connections_list(test_app_app)),
  0 = length(deliverly:connections_list(mpx)),
  0 = length(deliverly:connections_list()),
  ok.

client_disconnected_without_apps(Config) ->
  Client = test_module:create_client(mpx),
  ok = test_module:disconnect(Client),
  timer:sleep(100),
  0 = length(deliverly:connections_list(mpx)),
  0 = length(deliverly:connections_list()),
  ok.

send_message_to_default(Config) ->
  Client = ?config(client, Config),
  {ok, _} = test_module:received(Client, ?msg("default", "hello!")),
  ok.

send_message_to_test_app(Config) ->
  Client = ?config(client, Config),
  {ok, _} = test_module:received(Client, ?msg("test_app_app", "{\"message\":\"goodbye!\"}")),
  ok.

validate_messages(Config) ->
  Client = ?config(client, Config),
  #{sent := S, received := R} = test_module:info(Client),
  12 = length(S), %% 5 to default,  5 to test_app_app and 2 subs
  13 = length(R), %% 1 history from default, 5 broadcasts from default, 5 replies from test and 2 open
  5 = length(default:history()),
  ct:log(error, [?HI_IMPORTANCE], "Received: ~p", [R]),
  true = lists:member({text, <<"default,open">>}, R),
  true = lists:member({text, <<"test_app_app,open">>}, R),
  true = lists:member({text, <<"test_app_app,{\"reply\":\"goodbye!\"}">>}, R),
  ok.

send_message_without_subscription(Config) ->
  Client = test_module:create_client(mpx),
  close = test_module:received(Client, ?msg("default", "hello!")),
  timer:sleep(200),
  0 = length(deliverly:connections_list(mpx)),
  0 = length(deliverly:connections_list()),
  #{disconnected := true} = test_module:info(Client).

send_message_after_unsubscribe(Config) ->
  Client = test_module:create_client(mpx),
  {ok, _} = test_module:received(Client, ?sub_msg("default")),
  {ok, _} = test_module:received(Client, ?unsub_msg("default")),
  close = test_module:received(Client, ?msg("default", "hello!")),
  timer:sleep(200),
  0 = length(deliverly:connections_list(mpx)),
  0 = length(deliverly:connections_list()),
  #{disconnected := true} = test_module:info(Client).