-module(test_app_app).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(application).
-behaviour(deliverly_handler).

%% Application callbacks
-export([start/2, stop/1, deliverly_handler/0]).

%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

%% Connection module functions
-export([send/2, close/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ok.

stop(_State) ->
    ok.

deliverly_handler() ->
  test_app_app.

authorize(Client,[]) -> {ok, Client#de_client{encoder=json_encoder}};

authorize(_,_) -> {error, undefined}.

handle_message(_,_) -> ok.

handle_client_message(Client, #{<<"message">> := Msg}) -> {reply, Client, #{reply => Msg}};

handle_client_message(_,_) -> ok.

client_disconnected(_) -> ok.

send(_,_) -> ok.

close(_) -> ok.