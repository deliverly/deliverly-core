-module(deliverly_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-compile(export_all).

init_per_suite(Config) ->
  lager:start(),
  ulitos_app:set_var(?APP, default_app, true),
  ulitos_app:set_var(?APP, default_app_opt, [{auth_token, true}]),
  ulitos_app:set_var(?APP, redis_database, 7),
  Config.

end_per_suite(_) ->
  application:stop(lager),
  ok.

init_per_group(_, Config) ->
  deliverly:start(),
  test_app_app:start([],[]),
  timer:sleep(200),
  Config.

end_per_group(_, Config) ->
  redis_cli:q(["EVAL \"return redis.call('del', unpack(redis.call('keys', ARGV[1])))\" 0 *"]),
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
        auth_token_success,
        auth_token_failed,
        auth_token_timeout,
        auth_token_once,
        auth_token_infinity,
        client_disconnected
      ]
    }
  ].


ensure_default_started(_) ->
  deliverly_server:handle_message(default, [], []),
  true = lists:member(default, deliverly:apps_list()),
  ok.

ensure_test_started(_) ->
  deliverly_server:handle_message(test_app_app, [], []),
  true = lists:member(test_app_app, deliverly:apps_list()),
  ok.


app_connections_list(_) ->
  deliverly_server:auth_client(#de_client{socket=1, app = test_app_app},[]),
  1 = length(deliverly:connections_list()),
  1 = length(deliverly:connections_list(test_app_app)),
  ok.


auth_success(_) ->
  Res = deliverly_server:auth_client(#de_client{socket=1, app = test_app_app},[]),
  Size = length(deliverly:connections_list()),
  {ok,_} = Res,
  1 = Size,
  ok.

auth_failed(_) ->
  Res = deliverly_server:auth_client(#de_client{socket=2, app = test_app_app},[{fake, fake}]),
  Size = length(deliverly:connections_list()),
  {error, _} = Res,
  1 = Size,
  ok.

auth_token_success(_) ->
  Token = proplists:get_value(token, de_auth_token:request_token(#{})),
  Res = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, Token}]),
  {reply, _, _} = Res,
  ok.

auth_token_failed(_) ->
  Res = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, <<"123">>}]),
  {error, 3401} = Res,
  ok.

auth_token_timeout(_) ->
  Token = proplists:get_value(token, de_auth_token:request_token(#{expires_in => 1})),
  timer:sleep(1000),
  Res = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, Token}]),
  {error, 3401} = Res,
  ok.

auth_token_once(_) ->
  Token = proplists:get_value(token, de_auth_token:request_token(#{expires_in => 10, once => true})),
  Res1 = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, Token}]),
  {reply, _, _} = Res1,
  Res2 = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, Token}]),
  {error, 3401} = Res2,
  ok.

auth_token_infinity(_) ->
  Token = proplists:get_value(token, de_auth_token:request_token(#{expires_in => 10, once => true})),
  Res1 = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, Token}]),
  {reply, _, _} = Res1,
  Res2 = deliverly_server:auth_client(#de_client{socket=1, app = default}, [{<<"token">>, Token}]),
  {error, 3401} = Res2,
  ok.

client_disconnected(_) ->
  deliverly_server:client_disconnected(#de_client{socket=1, app = test_app_app}),
  0 = length(deliverly:connections_list()),
  ok.