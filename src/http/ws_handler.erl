-module(ws_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(cowboy_websocket).

-export([init/2, websocket_handle/3, websocket_info/3, terminate/3]).


%% Client functions
-export([send/2, close/1, info/2, update/1]).

%% ------------------------------------------------------------------
%% cowboy_websocket_handler Function Definitions
%% ------------------------------------------------------------------

init(Req, _Opts) ->
  Client = build_client(Req),
  Data = cowboy_req:parse_qs(Req),
  ?D({client_connecting, Client, Data}),
  self() ! {authorize, Data},
  {cowboy_websocket, Req, Client}.

websocket_handle({pong, _}, Req, Client) ->
  {ok, Req, Client};

websocket_handle({ping, _}, Req, Client) ->
  {ok, Req, Client};

websocket_handle(Data, Req, Client) ->
  case deliverly_server:handle_client_message(Client,de_client:decode(Client,Data)) of
    ok -> {ok, Req, Client};
    {ok, Client2} -> {ok, Req, Client2};
    {reply, Client2, Response} -> {reply, de_client:encode(Client2, Response), Req, Client2};
    _Other -> 
      ?D({close_connection ,_Other}),
      {reply, close, Req, Client}
  end.

websocket_info({authorize, Data}, Req, Client) ->
  case deliverly_server:auth_client(Client, Data) of
    {ok, Client2} -> {ok, Req, Client2};
    {reply, Client2, Response} -> {reply, de_client:encode(Client2, Response), Req, Client2};
    _ -> {reply, close, Req, Client}
  end;

websocket_info({handle_message, Data}, Req, Client) ->
  {reply, Data, Req, Client};

websocket_info(handle_close, Req, Client) ->
  {reply, close, Req, Client};

websocket_info({info, Receiver}, Req, Client) ->
  Receiver ! {client_info, Client},
  {ok, Req, Client};

websocket_info({update, NewClient}, Req, _) ->
  {ok, Req, NewClient};


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

send(#de_client{socket = Socket}=Client, Data) ->
  Socket ! {handle_message, de_client:encode(Client, Data)},
  ok.

%% @doc
%% Close socket connection 
%% @end

-spec close(client()) -> ok.

close(#de_client{socket = Socket}) ->
  Socket ! handle_close,
  ok.

%% @doc
%% Ask websocket to send its state.
%% Websocket process will send erlang message {client_info, State} to Receiver.
%% @end

-spec info(client(), Receiver::pid()) -> ok.

info(#de_client{socket = Socket}, Receiver) ->
  Socket ! {info, Receiver},
  ok.


%% @doc
%% Update websocket process state
%% @end

-spec update(client()) -> ok.

update(#de_client{socket=Socket}=Client) ->
  Socket ! {update, Client},
  ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_client(Req) ->
  App = binary_to_atom(cowboy_req:binding(app, Req, <<"default">>), latin1),
  Path = cowboy_req:path_info(Req),
  Host = binary_to_list(cowboy_req:host(Req)),
  #de_client{connected_at = ulitos:timestamp(), app = App, host = Host, path = Path, socket = self(), module = ?MODULE, encoder = raw_encoder}.