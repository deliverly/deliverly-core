-module(deliverly_utils).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/priv.hrl").
-include_lib("deliverly/include/log.hrl").

%% API
-export([auth_from_config/3]).

%% @doc
%% Authorization by modules described in config
%% @end

-spec auth_from_config(App :: atom(), Client :: client(), Data :: any()) -> true | false.

auth_from_config(App, Client, Data) ->
  AuthModules = maps:get(auth, ?Config(App, #{}), []),
  not lists:any(fun({Module, Args}) -> not Module:verify(Client, Data, Args) end, AuthModules).