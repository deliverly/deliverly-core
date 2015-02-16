-module(test_module).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% Application callbacks
-export([start/0, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Connection module functions
-export([send/2, close/1, info/2, update/1]).

%% API functions
-export([create_client/1, create_client/2, received/2, info/1, disconnect/1]).

-record(client_data,{
  id,
  client,
  disconnected = false,
  sent = [],
  received = []
}).

-record(state, {
  clients = #{}
}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  spawn(gen_server, start_link, [{local, ?SERVER}, ?MODULE, [], []]),
  ok.

stop() ->
  ?SERVER ! stop,
  ok.

received(Id, Data) ->
  gen_server:call(?SERVER, {received, Id, Data}).

disconnect(Id) ->
  gen_server:call(?SERVER, {disconnect, Id}).

send(#de_client{socket=Id}, Data) -> 
  gen_server:call(?SERVER, {send, Id, Data}).

close(#de_client{socket=Id}) -> 
  disconnect(Id).

info(Client, Receiver) ->
  %% to prevent deadlock send async message to self
  ?SERVER ! {info, Client, Receiver},
  ok.

update(_) ->
  %% do nothing
  ok.

create_client(App) ->
  create_client(App, []).

create_client(App, AuthData) ->
  gen_server:call(?SERVER, {create_client, App, AuthData}).

info(#de_client{socket=Id}=Client) ->
  gen_server:call(?SERVER, {info, Id, Client}).

init(_) ->
  {ok, #state{}}.

handle_call({create_client, App, AuthData}, _, #state{clients=Clients}=State) ->
  Ref = make_ref(),
  Client_ = #de_client{connected_at = ulitos:timestamp(), app = App, socket = Ref, module = ?MODULE, encoder = raw_encoder},
  Res = deliverly_server:auth_client(Client_,AuthData),
  Client = element(2, Res),
  {reply, Ref, State#state{clients=maps:put(Ref, #client_data{id=Ref, client=Client}, Clients)}};

handle_call({send, Id, Data}, _, #state{clients=Clients}=State) ->
  #client_data{received=Received}=ClientData = maps:get(Id, Clients),
  {reply, ok, State#state{clients = maps:update(Id, ClientData#client_data{received=[Data|Received]}, Clients)}}; 

handle_call({received, Id, Data}, _, #state{clients=Clients}=State) ->
  #client_data{sent=Sent, client=Client, received=Received}=ClientData = maps:get(Id, Clients),
  {Reply, ClientData_} = case deliverly_server:handle_client_message(Client,de_client:decode(Client,Data)) of
    ok -> {{ok, Client},ClientData};
    {ok, Client2} -> {{ok, Client2},ClientData#client_data{client=Client2}};
    {reply, Client2, Response} -> {{ok, Client2}, ClientData#client_data{client=Client2, received=[de_client:encode(Client2, Response)|Received]}};
    _Other -> 
      {close, ClientData}
  end,
  {reply, Reply, State#state{clients = maps:update(Id, ClientData_#client_data{sent=[Data|Sent]}, Clients)}}; 


handle_call({info, Id}, _, #state{clients=Clients}=State) ->
  #client_data{received=R, sent=S} = maps:get(Id, Clients),
  {reply, #{sent => S, received => R}, State};

handle_call({disconnect, Id}, _, #state{clients=Clients}=State) ->
  #client_data{client=Client}=ClientData = maps:get(Id, Clients),
  deliverly_server:client_disconnected(Client),
  {reply, ok, State#state{clients=maps:update(Id, ClientData#client_data{disconnected=true}, Clients)}};

handle_call(_, _, State) -> {reply, ok, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({info, Client, Receiver},State) ->
  Receiver ! {client_info, Client},
  {noreply, State};

handle_info(stop, State) -> {stop, normal, State};

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.