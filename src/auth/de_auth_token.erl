-module(de_auth_token).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").

-export([request_token/1, verify/2, verify/3]).


%% @doc
%% Create token and write it in redis
%% @end

-spec request_token(Options :: map() | proplists:proplist()) -> proplists:proplist().

request_token(Options) ->
  rand:seed(os:timestamp()),
  NewOptions = merge(Options),
  {Params, Tokens} = request_token_opt(NewOptions),
  NewParams =
    case NewOptions of
      #{scope := <<"*">>} -> Params;
      #{scope := Apps} -> [{scope, Apps} | Params]
    end,
  case NewOptions of
    #{count := undefined} -> [{token, hd(Tokens)} | NewParams];
    _ -> [{tokens, Tokens} | NewParams]
  end.

%% @doc
%% Verify token
%% @end

-spec verify(Client :: client(), Data :: proplists:proplist()) -> true | false.

verify(#de_client{app = App}, Data) ->
  Token = proplists:get_value(<<"token">>, Data, <<"">>),
  case get_scope(Token) of
    undefined ->
      false;
    <<"*">> ->
      true;
    Apps ->
      lists:member(atom_to_list(App), string:tokens(binary_to_list(Apps), ","))
  end.

-spec verify(Client :: client(), Data :: proplists:proplist(), any()) -> true | false.

verify(Client, Data, _Args) ->
  verify(Client, Data).

-spec merge(Options :: proplists:proplist() | map()) -> map().

merge(Options) ->
  Default = #{
    count => undefined,
    expires_in => ulitos_app:get_var(deliverly, token_timeout, 60),
    once => ulitos_app:get_var(deliverly, token_once, false),
    scope => <<"*">>
  },
  maps:map(fun to_term/2, merge(Default, Options)).

-spec merge(map(), Options :: proplists:proplist() | map()) -> map().

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

-spec to_term(atom(), any()) -> any().

to_term(expires_in, <<"infinity">>) ->
  infinity;

to_term(expires_in, Timeout) when is_binary(Timeout) ->
  binary_to_integer(Timeout);

to_term(count, Count) when is_binary(Count) ->
  max(min(binary_to_integer(Count), ?Config(max_tokens, 100)), 1);

to_term(once, <<"true">>) ->
  true;

to_term(once, <<"false">>) ->
  false;

to_term(_Key, Value) ->
  Value.

-spec request_token_opt(map()) -> proplists:proplist().

request_token_opt(#{expires_in := infinity, once := true, scope := Apps, count := Count}) ->
  Tokens = tokens(infinity, true, Apps, Count),
  {[{once, true}], Tokens};

request_token_opt(#{expires_in := infinity, once := false, scope := Apps, count := Count}) ->
  Tokens = tokens(infinity, false, Apps, Count),
  {[], Tokens};

request_token_opt(#{expires_in := Timeout, once := false, scope := Apps, count := Count}) ->
  Tokens = tokens(Timeout, false, Apps, Count),
  {[{expires_in, Timeout}], Tokens};

request_token_opt(#{expires_in := Timeout, once := true, scope := Apps, count := Count}) ->
  Tokens = tokens(Timeout, true, Apps, Count),
  {[{expires_in, Timeout}, {once, true}], Tokens}.

-spec token(infinity | non_neg_integer(), boolean(), binary()) -> binary().

token(Timeout, Once, Apps) ->
  Length = 9 - if Once -> 1; true -> 0 end,
  Token = list_to_binary(ulitos:random_string(Length)),
  Namespace = ?Config(redis_namespace, <<"">>),
  case Timeout of
    infinity ->
      redis_cli:q(["SET", <<Namespace/binary, Token/binary>>, Apps]);
    _ ->
      redis_cli:q(["SETEX", <<Namespace/binary, Token/binary>>, Timeout, Apps])
  end,
  Token.

-spec tokens(infinity | non_neg_integer(), boolean(), binary(), undefined | non_neg_integer())
      -> [binary()].

tokens(Timeout, Once, Apps, undefined) ->
  [token(Timeout, Once, Apps)];

tokens(Timeout, Once, Apps, Count) ->
  redis_cli:q(["MULTI"]),
  Tokens = [token(Timeout, Once, Apps) || _ <- lists:seq(1, Count)],
  redis_cli:q(["EXEC"]),
  Tokens.

-spec get_scope(binary()) -> undefined | binary().

get_scope(Token) when bit_size(Token) == 72 ->
  Namespace = ?Config(redis_namespace, <<"">>),
  case redis_cli:q(["GET", <<Namespace/binary, Token/binary>>]) of
    {ok, undefined} ->
      undefined;
    {ok, Apps} ->
      Apps;
    _ ->
      undefined
  end;

get_scope(Token) ->
  Namespace = ?Config(redis_namespace, <<"">>),
  redis_cli:q(["MULTI"]),
  redis_cli:q(["GET",  <<Namespace/binary, Token/binary>>]),
  redis_cli:q(["DEL",  <<Namespace/binary, Token/binary>>]),
  case redis_cli:q(["EXEC"]) of
    {ok, [undefined, _]} ->
      undefined;
    {ok, [Apps, _]} ->
      Apps;
    _ ->
      undefined
  end.
