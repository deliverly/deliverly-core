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
  client_disconnected/1,
  find_handler/1
]).

-record(state, {
  started_at ::non_neg_integer()
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

-spec handle_message(App::atom(), Data::any(), Context::any()) -> ok | broadcast | {error, Reason::atom()}.

handle_message(App, Data, Context) -> 
  Res = gen_server:call(?SERVER, {handle_message, App, Data, Context}),
  Res2 = 
    if Res =:= broadcast
      ->  deliverly_nodes:broadcast_message(App, Data, Context),
          ok;
      true -> Res    
  end,
  Res2.
  

%% @doc
%% Process client's data within application.
%% @end

-spec handle_client_message(Client::client(), Data::any()) -> ok | broadcast | {ok, Client2::client()} | {broadcast, Client2::client()} | {reply, Client2::client(), Data::any()} | {error, Reason::any()}.

%% don't handle bad messages!
handle_client_message(_, false) -> ok;

handle_client_message(Client, Data) -> 
  Res = gen_server:call(?SERVER, {handle_client_message, Client, Data}),
  Res2 = 
    case Res of
      broadcast ->  deliverly_nodes:broadcast_client_message(Client, Data),
                    ok;
      {broadcast, Client2} -> gen_server:call(?SERVER, {handle_client_message, Client2, Data}),
                              {ok, Client2};
      _ -> Res
    end,
  Res2.

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
  ets:new(?APP, [duplicate_bag, public, named_table, {keypos, #de_client.socket}]),
  ets:new(?ETS_HANDLERS, [public, named_table]),
  self() ! post_init,  
  {ok, #state{started_at = ulitos:timestamp()}}.

handle_call({auth_client, #de_client{app = App} = Client, Data}, _From, State) ->
  case find_handler(App) of
    false ->
      ?D({app_not_exist, App}),
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

handle_call({handle_message, App, Data, Context}, _From, State) ->
  case find_handler(App) of
    false ->
      ?D({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:handle_message(Data, Context),
      {reply, Res, State}
  end;

handle_call({handle_client_message, #de_client{app = App, socket = Socket} = Client, Data}, _From, State) ->
  case find_handler(App) of
    false ->
      ?D({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:handle_client_message(Client, Data),
      
      case Res of
        Atom when is_atom(Atom) -> pass; %% ok or broadcast
        {error, _} -> pass;
        _ ->
          Client2 = element(2, Res),
          if Client =/= Client2 ->
            %% first delete matched client (because our ets is dup bag)
            ets:match_delete(?APP, #de_client{app=App, socket=Socket, _='_'}),
            ets:insert(?APP, Client2);
            true -> pass
          end
      end,
    
      {reply, Res, State}
  end;

handle_call({client_disconnected, #de_client{app = App} = Client}, _From, State) ->
  ?ACCESS("DISCONNECT ~p ~p",[App,Client#de_client.path]),
  case find_handler(App) of
    false ->
      ?D({app_not_exist, App}),
      {reply, {error, noexist}, State};
    Handler -> 
      Res = Handler:client_disconnected(Client),
      ets:delete(?APP, Client#de_client.socket),
      {reply, Res, State}
  end;


handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(post_init, State) ->
  case ?Config(default_app, false) of
    true -> 
      ?D(<<"Default app is enabled">>),
      deliverly_sup:start_server(default);
    _ -> pass
  end,
  %% run multiplex app
  deliverly_sup:start_server(mpx),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

find_handler(App) -> 
  case ets:lookup(?ETS_HANDLERS, App) of
    [] -> find_from_code(App);
    [{App, Handler}] -> Handler
  end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_from_code(App) ->
  case code:is_loaded(App) of
    false -> false;
    {file, _} -> 
      case erlang:function_exported(App, deliverly_handler, 0) of
        true -> 
          Handler = erlang:apply(App, deliverly_handler, []),
          ets:insert(?ETS_HANDLERS, {App, Handler}),
          Handler;
        false -> false
      end
  end.