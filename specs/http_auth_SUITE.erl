-module(http_auth_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-import(test_utils, [
  gun_open/1,
  auth_headers/2,
  post/3
]).

-compile(export_all).

init_per_suite(Config) ->
  ulitos_app:set_var(?APP, redis_database, 7),
  ulitos_app:set_var(?APP, api_user, <<"admin">>),
  ulitos_app:set_var(?APP, api_pass, list_to_binary(ulitos:random_string(8))),
  lager:start(),
  deliverly:start(),
  ulitos_app:ensure_started(gun),
  [{url, "/api/auth/token"}, {port, 8081} | Config].

end_per_suite(_) ->
  ulitos_app:set_var(?APP, api_user, <<"">>),
  ulitos_app:set_var(?APP, api_pass, <<"">>),
  redis_cli:q(["EVAL", "return redis.call('del', unpack(redis.call('keys', ARGV[1])))",  0,  "*"]),
  deliverly:stop(),
  application:stop(gun),
  application:stop(lager),
  ok.

init_per_group(success, Config) ->
  User = ulitos_app:get_var(?APP, api_user),
  Pass = ulitos_app:get_var(?APP, api_pass),
  [{headers, auth_headers(User, Pass)} | Config];

init_per_group(failed, Config) ->
  User = ulitos_app:get_var(?APP, api_user),
  Pass = <<"miss">>,
  [{headers, auth_headers(User, Pass)} | Config].

end_per_group(_Group, _Config) ->
  ok.

all() ->
  [
    {group, success},
    {group, failed}
  ].

groups() ->
  [
    {
      success, [sequence],
      [
        one_token_success,
        multi_token_success
      ]
    },
    {
      failed, [sequence],
      [
        token_failed
      ]
    }
  ].

one_token_success(Config) ->
  ConnPid = gun_open(Config),
  {200, Data} = post(ConnPid, Config, [{<<"expires_in">>, <<"40">>}, {<<"scope">>, <<"foo,bar">>}, {<<"once">>, true}]),
  <<"foo,bar">> = proplists:get_value(<<"scope">>, Data),
  40 = proplists:get_value(<<"expires_in">>, Data),
  true = proplists:get_value(<<"once">>, Data),
  {<<"token">>, _} = proplists:lookup(<<"token">>, Data),
  ok.

multi_token_success(Config) ->
  ConnPid = gun_open(Config),
  {200, Data} = post(ConnPid, Config, [{<<"count">>, <<"23">>}]),
  Tokens = proplists:get_value(<<"tokens">>, Data),
  true = is_list(Tokens) andalso length(Tokens) =< 23 andalso length(Tokens) > 0,
  ok.

token_failed(Config) ->
  ConnPid = gun_open(Config),
  {401, []} = post(ConnPid, Config, [{<<"expires_in">>, <<"40">>}, {<<"scope">>, <<"foo,bar">>}, {<<"once">>, true}]),
  ok.
