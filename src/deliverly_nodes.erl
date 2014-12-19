%%% @doc
%%% Provides functionality to broadcast messages among nodes.
%%% @end

-module(deliverly_nodes).
-behaviour(gen_server).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").
-define(SERVER, ?MODULE).
-define(POLL_SERVER_TIMEOUT, 30000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([
  broadcast_message/3,
  broadcast_client_message/2
]).

-record(state, {
  connected = false ::boolean(),
  remote_node ::atom()
}).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @doc
%% Send client message to all nodes.
%% @end

-spec broadcast_client_message(Client::client(), Message::any()) -> ok.

broadcast_client_message(Client, Message) ->
  gen_server:cast(?SERVER, {cast_nodes, gen_server, call, [deliverly_server, {handle_client_message, Client, Message}]}).


%% @doc
%% Send custom message to all nodes.
%% @end

-spec broadcast_message(App::atom(), Message::any(), Context::any()) -> ok.

broadcast_message(App, Message, Context) ->
  gen_server:cast(?SERVER, {cast_nodes, gen_server, call, [deliverly_server, {handle_message, App, Message, Context}]}).
  

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
  net_kernel:monitor_nodes(true),
  self() ! connect,
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({cast_nodes, Module, Fun, Args}, State) ->
  ?D({cast_nodes, Module, Fun, Args, nodes()}),
  rpc:eval_everywhere(nodes(), Module, Fun, Args),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(connect, State) ->
  Node = ?Config(remote_node, undefined),
  handle_info({connect, Node}, State);

handle_info({connect, undefined}, State) ->
  {noreply, State}; 

handle_info({connect, Node}, _) -> 
  ?D({connect_to_remote_node, Node}),
  Connected = 
    case net_kernel:connect_node(Node) of
      true -> true;
      _ -> ?E(server_not_found),
            erlang:send_after(?POLL_SERVER_TIMEOUT,self(),connect),
            false
      end,
  {noreply, #state{connected=Connected, remote_node=Node}};

handle_info({nodedown, Node}, #state{connected=true, remote_node=Node}) ->
  Nodes = nodes(),
  if length(Nodes) =:= 0
    -> erlang:send_after(?POLL_SERVER_TIMEOUT,self(),connect);
    true -> pass
  end,
  {noreply, #state{}};

handle_info(_Info, State) ->
  ?D({node_info, _Info}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------