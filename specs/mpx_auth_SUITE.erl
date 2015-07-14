-module(mpx_auth_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-import(test_utils, [
  gun_open/1,
  request_token/2,
  request_token/3,
  message/0
]).

-compile(export_all).

init_per_suite(Config) ->
  ulitos_app:set_var(?APP, redis_database, 7),
  User = <<"admin">>,
  Pass = list_to_binary(ulitos:random_string(8)),
  ulitos_app:set_var(?APP, api_user, User),
  ulitos_app:set_var(?APP, api_pass, Pass),
  ulitos_app:set_var(?APP, default, #{}),
  ulitos_app:set_var(?APP, mpx, #{}),
  lager:start(),
  deliverly:start(),
  ulitos_app:ensure_started(gun),
  [{user, User}, {pass, Pass}, {port, 8081} | Config].

end_per_suite(_) ->
  redis_cli:q(["EVAL", "return redis.call('del', unpack(redis.call('keys', ARGV[1])))",  0,  "*"]),
  deliverly:stop(),
  application:stop(gun),
  application:stop(lager),
  ok.

init_per_group(token, Config) ->
  [{auth_opt, [{de_auth_token, []}]} | Config];

init_per_group(referer_success, Config) ->
  [{auth_opt, [{de_auth_referer, ["localhost"]}]} | Config];

init_per_group(referer_failed, Config) ->
  [{auth_opt, [{de_auth_referer, ["example.com"]}]} | Config];

init_per_group(Group, Config) ->
  ulitos_app:set_var(?APP, Group, #{auth => ?config(auth_opt, Config)}),
  Config.

end_per_group(token, _Config) ->
  ok;

end_per_group(referer_success, _Config) ->
  ok;

end_per_group(referer_failed, _Config) ->
  ok;

end_per_group(Group, _Config) ->
  ulitos_app:set_var(?APP, Group, #{}),
  ok.

all() ->
  [
    {group, token},
    {group, referer_success},
    {group, referer_failed}
  ].

groups() ->
  [
    {
      token, [],
      [
        {
          mpx, [sequence],
          [
            mpx_auth_token_success,
            mpx_auth_token_failed,
            mpx_auth_token_scope_success,
            mpx_auth_token_scope_failed
          ]
        }
      ]
    },
    {
      referer_success, [],
      [
        {
          mpx, [sequence],
          [
            mpx_auth_referer_success
          ]
        }
      ]
    },
    {
      referer_failed, [],
      [
        {
          mpx, [sequence],
          [
            mpx_auth_referer_failed
          ]
        }
      ]
    }
  ].

mpx_auth_token_success(Config) ->
  ConnPid = gun_open(Config),
  Token = request_token(ConnPid, Config),
  gun:ws_upgrade(ConnPid, <<"/ws/mpx?token=", Token/binary>>),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  gun:ws_send(ConnPid, {text, <<"default,sub">>}),
  {gun_ws, ConnPid, {text, <<"default,open">>}} = message(),
  ok.

mpx_auth_token_failed(Config) ->
  ConnPid = gun_open(Config),
  Token = <<"token">>,
  gun:ws_upgrade(ConnPid, <<"/ws/mpx?token=", Token/binary>>),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  {gun_ws, ConnPid, close} = message(),
  ok.

mpx_auth_token_scope_success(Config) ->
  ConnPid = gun_open(Config),
  Token = request_token(ConnPid, Config, [{<<"scope">>, <<"mpx">>}]),
  gun:ws_upgrade(ConnPid, <<"/ws/mpx?token=", Token/binary>>),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  gun:ws_send(ConnPid, {text, <<"default,sub">>}),
  {gun_ws, ConnPid, {text, <<"default,open">>}} = message(),
  ok.

mpx_auth_token_scope_failed(Config) ->
  ConnPid = gun_open(Config),
  Token = request_token(ConnPid, Config, [{<<"scope">>, <<"foo">>}]),
  gun:ws_upgrade(ConnPid, <<"/ws/mpx?token=", Token/binary>>),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  {gun_ws, ConnPid, close} = message(),
  ok.

mpx_auth_referer_success(Config) ->
  ConnPid = gun_open(Config),
  gun:ws_upgrade(ConnPid, <<"/ws/mpx">>),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  gun:ws_send(ConnPid, {text, <<"default,sub">>}),
  {gun_ws, ConnPid, {text, <<"default,open">>}} = message(),
  ok.

mpx_auth_referer_failed(Config) ->
  ConnPid = gun_open(Config),
  gun:ws_upgrade(ConnPid, <<"/ws/mpx">>),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  {gun_ws, ConnPid, close} = message(),
  ok.
