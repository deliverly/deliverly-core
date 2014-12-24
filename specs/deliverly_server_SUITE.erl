-module(deliverly_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-compile(export_all).

init_per_suite(Config) ->
  lager:start(),
  ulitos_app:set_var(?APP, default_app, true),
  Config.

end_per_suite(_) ->
  application:stop(lager),
  ok.

init_per_group(_, Config) ->
  deliverly:start(),
  test_app_app:start([],[]),
  timer:sleep(200),
  Config.

end_per_group(_,Config) ->
  test_app_app:stop([]),
  deliverly:stop(),
  ok.

all() ->
  [
    {group, simple_tests},
    {group, clients_auth}
  ].

groups() ->
  [
    {
      simple_tests, [shuffle, sequence], 
      [
        ensure_default_started,
        ensure_test_started,
        app_connections_list
      ]
    },
    {
      clients_auth, [sequence],
      [
        auth_success,
        auth_failed,
        client_disconnected
      ]
    }
  ].


ensure_default_started(_) ->
  true = lists:member(default, deliverly:apps_list()),
  ok.

ensure_test_started(_) ->
  true = lists:member(test_app, deliverly:apps_list()),
  ok.


app_connections_list(_) ->
  deliverly:register_handler(test_app_bad, test_app_app),
  deliverly_server:auth_client(#de_client{socket=1, app = test_app},[]),
  deliverly_server:auth_client(#de_client{socket=2, app = test_app_bad},[]),
  2 = length(deliverly:connections_list()),
  1 = length(deliverly:connections_list(test_app)),
  ok.


auth_success(_) ->
  Res = deliverly_server:auth_client(#de_client{socket=1, app = test_app},[]),
  Size = length(deliverly:connections_list()),
  {ok,_} = Res,
  1 = Size,
  ok.

auth_failed(_) ->
  Res = deliverly_server:auth_client(#de_client{socket=2, app = test_app},[{fake, fake}]),
  Size = length(deliverly:connections_list()),
  {error, _} = Res,
  1 = Size,
  ok.

client_disconnected(_) ->
  deliverly_server:client_disconnected(#de_client{socket=1, app = test_app}),
  0 = length(deliverly:connections_list()),
  ok.