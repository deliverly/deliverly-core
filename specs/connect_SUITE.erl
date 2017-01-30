-module(connect_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-import(test_utils, [
  gun_open/1,
  message/0
]).

-compile(export_all).

init_per_suite(Config) ->
  ulitos_app:set_var(?APP, default, #{}),
  lager:start(),
  deliverly:start(),
  ulitos_app:ensure_started(gun),
  [{port, 8081} | Config].

end_per_suite(_) ->
  deliverly:stop(),
  application:stop(gun),
  application:stop(lager),
  ok.

all() ->
  [
    {group, app_resolver}
  ].

groups() ->
  [
    {
      app_resolver, [sequence],
      [
        default_app_success,
        unknown_app_failed
      ]
    }
  ].

default_app_success(Config) ->
  ConnPid = gun_open(Config),
  gun:ws_upgrade(ConnPid, "/ws/default"),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  gun:ws_send(ConnPid, {text, <<"echo">>}),
  {gun_ws, ConnPid, {text, <<"echo">>}} = message(),
  ok.

unknown_app_failed(Config) ->
  ConnPid = gun_open(Config),
  gun:ws_upgrade(ConnPid, "/ws/foobar"),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  {gun_ws, ConnPid, close} = message(),
  ok.
