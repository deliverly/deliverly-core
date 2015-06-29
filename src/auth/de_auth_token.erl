-module(de_auth_token).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-define(NAMESPACE, <<"delivery:auth:token:">>).

-export([request_token/1, verify/2, verify/3]).

request_token(Options) ->
  random:seed(erlang:now()),
  request_token_opt(merge(Options)).

verify(#de_client{app = App}, Data) ->
  Token = proplists:get_value(<<"token">>, Data),
  case get_scope(Token) of
    undefined ->
      false;
    <<"*">> ->
      true;
    Apps->
      lists:member(atom_to_list(App), string:tokens(binary_to_list(Apps), ","))
  end.

verify(Client, Data, _Args) ->
  verify(Client, Data).

merge(Options) ->
  Default = #{
    expires_in => ulitos_app:get_var(deliverly, token_timeout, 60),
    once => ulitos_app:get_var(deliverly, token_once, false),
    scope => <<"*">>
  },
  maps:map(fun to_term/2, merge(Default, Options)).

merge(Default, Options) when is_map(Options) ->
  maps:map(fun(K, V) -> maps:get(K, Options, V) end, Default);

merge(Default, Options) when is_list(Options) ->
  maps:map(
    fun(K, V) ->
      proplists:get_value(K, Options, proplists:get_value(atom_to_binary(K, utf8), Options, V))
    end,
    Default
  );

merge(Default, _) ->
  Default.

to_term(expires_in, <<"infinity">>) ->
  infinity;

to_term(expires_in, Timeout) when is_binary(Timeout) ->
  binary_to_integer(Timeout);

to_term(once, <<"true">>) ->
  true;

to_term(once, <<"false">>) ->
  false;

to_term(_Key, Value) ->
  Value.

request_token_opt(#{expires_in := infinity, once := true, scope := Apps}) ->
  Token = list_to_binary(ulitos:random_string(8)),
  redis_cli:q(["SET", <<?NAMESPACE/binary, Token/binary>>, Apps]),
  [{token, Token}];

request_token_opt(#{expires_in := infinity, once := false, scope := Apps}) ->
  Token = list_to_binary(ulitos:random_string(9)),
  redis_cli:q(["SET", <<?NAMESPACE/binary, Token/binary>>, Apps]),
  [{token, Token}];

request_token_opt(#{expires_in := Timeout, once := false, scope := Apps}) ->
  Token = list_to_binary(ulitos:random_string(9)),
  redis_cli:q(["SETEX", <<?NAMESPACE/binary, Token/binary>>, Timeout, Apps]),
  [{token, Token}, {expires_in, Timeout}];

request_token_opt(#{expires_in := Timeout, once := true, scope := Apps}) ->
  Token = list_to_binary(ulitos:random_string(8)),
  redis_cli:q(["SETEX", <<?NAMESPACE/binary, Token/binary>>, Timeout, Apps]),
  [{token, Token}, {expires_in, Timeout}].

get_scope(Token) when bit_size(Token) == 72 ->
  case redis_cli:q(["GET", <<?NAMESPACE/binary, Token/binary>>]) of
    {ok, undefined} ->
      undefined;
    {ok, Apps} ->
      Apps;
    _ ->
      undefined
  end;

get_scope(Token) ->
  redis_cli:q(["MULTI"]),
  redis_cli:q(["GET",  <<?NAMESPACE/binary, Token/binary>>]),
  redis_cli:q(["DEL",  <<?NAMESPACE/binary, Token/binary>>]),
  case redis_cli:q(["EXEC"]) of
    {ok, [undefined, _]} ->
      undefined;
    {ok, [Apps, _]} ->
      Apps;
    _ ->
      undefined
  end.