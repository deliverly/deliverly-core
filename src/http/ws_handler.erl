-module(ws_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(cowboy_websocket).

-export([init/2, websocket_handle/3, websocket_info/3, terminate/3]).

-export([send/2, close/1]).

%% ------------------------------------------------------------------
%% cowboy_websocket_handler Function Definitions
%% ------------------------------------------------------------------

init(Req, _Opts) ->
  Client = build_client(Req),
  Data = cowboy_req:parse_qs(Req),
  ?D({client_connecting, Client, Data}),
  self() ! {authorize, Data},
  {cowboy_websocket, Req, Client}.

websocket_handle(Data, Req, Client) ->
  case deliverly_server:handle_client_message(Client,decode(Data)) of
    ok -> {ok, Req, Client};
    {ok, Response} -> {reply, encode(Response), Req, Client};
    _ -> {reply, close, Req, Client}
  end.

websocket_info({authorize, Data}, Req, Client) ->
  case deliverly_server:auth_client(Client, Data) of
    ok -> {ok, Req, Client};
    {ok, Response} -> {reply, encode(Response), Req, Client};
    _ -> {reply, close, Req, Client}
  end;

websocket_info({handle_message, Data}, Req, Client) ->
  {reply, encode(Data), Req, Client};

websocket_info(handle_close, Req, Client) ->
  {reply, close, Req, Client};

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, Client) ->
  ?D({websocket_disconnected, Client}),
  deliverly_server:client_disconnected(Client),
  ok.


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc
%% Send message to WebSocket
%% @end

-spec send(Client::client(), Data::any()) -> ok.

send(#de_client{socket = Socket}, Data) ->
  Socket ! {handle_message, Data},
  ok.

close(#de_client{socket = Socket}) ->
  Socket ! handle_close,
  ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

encode(Bin) when is_binary(Bin) ->
  {binary, Bin};

encode(Data) ->
  {text, jsx:encode(Data)}.

decode({text,Data}) ->
  jsx:decode(Data);

decode({binary, Data}) ->
  Data.

build_client(Req) ->
  App = binary_to_atom(cowboy_req:binding(app,Req,<<"default">>), latin1),
  Path = cowboy_req:path_info(Req),
  #de_client{connected_at = ulitos:timestamp(), app = App, path = Path, socket = self(), module = ws_handler}.