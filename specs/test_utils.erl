-module(test_utils).

-include_lib("common_test/include/ct.hrl").

%% API
-export([
  gun_open/1,
  request_token/1,
  request_token/2,
  request_token/3,
  request_token/4,
  auth_headers/1,
  auth_headers/2,
  post/3,
  message/0,
  message/1
]).

gun_open(Config) ->
  {ok, ConnPid} = gun:open("localhost", ?config(port, Config)),
  {gun_up, ConnPid, http} = message(),
  ConnPid.

request_token(Pid) ->
  request_token(Pid, <<"">>, <<"">>, []).

request_token(Pid, Config) ->
  request_token(Pid, ?config(user, Config), ?config(pass, Config), []).

request_token(Pid, Config, Params) ->
    request_token(Pid, ?config(user, Config), ?config(pass, Config), Params).

request_token(Pid, User, Pass, Params) ->
  case post(Pid, "/api/auth/token", auth_headers(User, Pass), Params) of
    {200, Data} ->
      proplists:get_value(<<"token">>, Data);
    {401, _} ->
      <<"token">>
  end.

post(Pid, Config, Params) ->
  post(Pid, ?config(url, Config), ?config(headers, Config), Params).

post(Pid, Url, Headers, Params) ->
  Body = cow_qs:qs(Params),
  Ref = gun:post(Pid, Url, Headers, Body),
  case gun:await(Pid, Ref) of
    {response, nofin, 200, _} ->
      {ok, Data} = gun:await_body(Pid, Ref),
      {200, jsx:decode(Data)};
    {response, fin, 401, _} ->
      {401, []}
  end.

auth_headers(Config) ->
  auth_headers(?config(user, Config), ?config(pass, Config)).

auth_headers(User, Pass) ->
  Base64 = base64:encode(<<User/binary, ":", Pass/binary>>),
  [{<<"Authorization">>, <<"Basic ", Base64/binary>>}].

message() ->
  message(1000).

message(Timeout) ->
  receive
    Message ->
      Message
  after Timeout ->
    timeout
  end.
