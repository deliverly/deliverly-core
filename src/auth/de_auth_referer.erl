-module(de_auth_referer).
-include_lib("deliverly/include/deliverly.hrl").

%% API
-export([verify/3]).

verify(#de_client{host = Host}, _Data, Hosts) ->
  lists:member(Host, Hosts).