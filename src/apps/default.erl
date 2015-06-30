-module(default).
-behaviour(gen_server).
-behaviour(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/priv.hrl").
-include_lib("deliverly/include/log.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0, deliverly_handler/0]).
%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

%%% Public API
-export([history/0]).

-record(state, {
  clients = #{}::#{pid() => client()},
  messages = [] ::list(),
  clear_timer
}).

-define(CLEAR_TIMEOUT, 30000).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

deliverly_handler() ->
  ?SERVER.

history() ->
  gen_server:call(?SERVER, history).

init([]) ->
  ?D(<<"Starting default application">>),
  {ok, #state{}}.

authorize(Client, Data) ->
  case deliverly_utils:auth_from_config(default, Client, Data) of
    true -> gen_server:call(?SERVER, {authorize, Client});
    false -> {error, 3401}
  end.

handle_message(_,_) -> ok.

handle_client_message(Client,Message) -> gen_server:call(?SERVER, {handle_client_message, Client, Message}).

client_disconnected(Client) -> gen_server:call(?SERVER, {client_disconnected, Client}).

handle_call({authorize, #de_client{socket=Socket}=Client}, _, #state{clients = Clients, messages = History, clear_timer = undefined}=State) ->
  {reply, {reply, Client, History}, State#state{clients = maps:put(Socket, Client, Clients)}};
  
handle_call({authorize, Client}, From, #state{clear_timer = Timer}=State) ->
  erlang:cancel_timer(Timer),
  handle_call({authorize, Client}, From, State#state{clear_timer = undefined});

handle_call({client_disconnected, #de_client{socket = Socket}}, _, #state{clients=Clients} = State) ->
  NewClients = maps:remove(Socket, Clients),
  Timer = case maps:size(NewClients) of
    0 -> 
      ?D({no_more_clients}),
      erlang:send_after(?CLEAR_TIMEOUT, self(), clear_history);
    _ -> undefined
  end,
  {reply, ok, State#state{clients = NewClients, clear_timer = Timer}};

handle_call({handle_client_message, _Client, Message}, _, #state{messages = Messages}=State) ->
  self() ! {broadcast, Message},
  {reply, broadcast, State#state{messages = lists:append(Messages,[Message])}};

handle_call(history, _, #state{messages=Messages} = State) ->
  {reply, Messages, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(clear_history, _State) ->
  ?D({clear_history}),
  {noreply, #state{}};

handle_info({broadcast, Message}, #state{clients=Clients}=State) ->
  de_client:broadcast_to(Clients, Message),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.