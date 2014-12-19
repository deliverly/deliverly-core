-module(deliverly_server).
-behaviour(gen_server).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  auth_client/2,
  handle_message/3,
  handle_client_message/2,
  client_disconnected/1
]).

-record(state, {
  started_at ::non_neg_integer(),
  apps = #{} ::#{atom() => atom()}
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Authorize client within application.
%% Add client to ETS if authorization was successful.
%% @end

-spec auth_client(Client::client(), Data::any()) -> ok | {ok, Response::any()} | {error, Reason::atom()}.

auth_client(Client, Data) -> 
  gen_server:call(?SERVER, {auth_client, Client, Data}).

%% @doc
%% Send data to app (precisely, to app's clients).
%% Context is defined within App.
%% @end

-spec handle_message(App::atom(), Data::any(), Context::any()) -> ok | {error, Reason::atom()}.

handle_message(App, Data, Context) -> 
  deliverly_nodes:broadcast_message(App, Data, Context),
  gen_server:call(?SERVER, {handle_message, App, Data, Context}).

%% @doc
%% Process client's data within application.
%% @end

-spec handle_client_message(Client::client(), Data::any()) -> ok | {ok, Response::any()} | {error, Reason::atom()}.

handle_client_message(Client, Data) -> 
  deliverly_nodes:broadcast_client_message(Client, Data),
  gen_server:call(?SERVER, {handle_client_message, Client, Data}).

%% @doc
%% Notify app that client has been disconnected.
%% @end

-spec client_disconnected(Client::client()) -> ok | {error, Reason::atom()}.

client_disconnected(Client) -> 
  gen_server:call(?SERVER, {client_disconnected, Client}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  ets:new(?APP, [public, named_table, {keypos, #de_client.socket}]),
  self() ! post_init,  
  {ok, #state{started_at = ulitos:timestamp()}}.

handle_call({register_handler, App, Handler}, _From, #state{apps = Apps}=State) ->
  case find_handler(App, Apps) of
    false ->
      {reply, ok, State#state{apps=maps:put(App, Handler, Apps)}};
    _ -> 
      {reply, {error, already_exists}, State}
  end;

handle_call({auth_client, #de_client{app = App} = Client, Data}, _From, #state{apps = Apps} = State) ->
  case find_handler(App, Apps) of
    false ->
      ?E({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:authorize(Client, Data),
      case Res of
        {error, Reason} -> 
          ?ACCESS("AUTH_FAILED ~p ~p ~p",[App,Client#de_client.path,Reason]),
          pass;
        _ -> 
          Client2 = element(2, Res),
          ?ACCESS("AUTH_SUCCESS ~p ~p",[App,Client2#de_client.path]),
          ets:insert(?APP, Client2)
      end,
      {reply, Res, State}
  end;

handle_call({handle_message, App, Data, Context}, _From, #state{apps = Apps} = State) ->
  case find_handler(App, Apps) of
    false ->
      ?E({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:handle_message(Data, Context),
      {reply, Res, State}
  end;

handle_call({handle_client_message, #de_client{app = App} = Client, Data}, _From, #state{apps = Apps} = State) ->
  case find_handler(App, Apps) of
    false ->
      ?E({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:handle_client_message(Client, Data),
      
      Client2 = element(2, Res),
      if Client =/= Client2 ->
        ets:insert(?APP, Client2);
        true -> pass
      end,
      
      {reply, Res, State}
  end;

handle_call({client_disconnected, #de_client{app = App} = Client}, _From, #state{apps = Apps} = State) ->
  ?ACCESS("DISCONNECT ~p ~p",[App,Client#de_client.path]),
  case find_handler(App, Apps) of
    false ->
      ?E({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:client_disconnected(Client),
      ets:delete(?APP, Client#de_client.socket),
      {reply, Res, State}
  end;


handle_call({apps_list}, _From, #state{apps=Apps} = State) ->
  {reply, maps:keys(Apps), State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(post_init, State) ->
  case ?Config(default_app, false) of
    true -> 
      ?D(<<"Default app is enabled">>),
      deliverly_sup:start_server(default_app);
    _ -> pass
  end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_handler(App, Apps) -> 
  maps:get(App,Apps,false).