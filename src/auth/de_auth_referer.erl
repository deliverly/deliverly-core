-module(de_auth_referer).
-include_lib("deliverly/include/deliverly.hrl").

%% API
-export([verify/3]).

%% @doc
%% Verify client's host
%% @end

-spec verify(Client :: client(), Data :: proplists:proplist(), [atom()]) -> true | false.

verify(#de_client{host = Host}, _Data, Hosts) ->
  lists:member(Host, Hosts).