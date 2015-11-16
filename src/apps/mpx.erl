-module(mpx).
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

%%% encoder functions
-export([decode/2, encode/2]).

%%% module functions
-export([send/2, close/1]).

-record(state,{}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

deliverly_handler() ->
  ?SERVER.

init([]) ->
  ?I("Starting multiplex application"),
  {ok, #state{}}.

authorize(Client, Data) ->
  case deliverly_utils:auth_from_config(mpx, Client, Data) of
    true -> {ok, Client#de_client{data = #{}, meta = #{auth => Data}, encoder = mpx, mpx = []}};
    false -> {error, 3401}
  end.

handle_message(Data, Context) -> gen_server:call(?SERVER, {handle_message, Data, Context}).

handle_client_message(#de_client{mpx=Apps}=Client, {App, sub, Data}) ->
  case lists:member(App, Apps) of
    false -> gen_server:call(?SERVER, {subscribe, Client, App, Data});
    true -> {error, already_subscribed}
  end;

handle_client_message(#de_client{mpx=Apps}=Client, {App, unsub}) ->
  case lists:member(App, Apps) of
    true -> gen_server:call(?SERVER, {unsubscribe, Client, App});
    false -> {error, unsubscribed}
  end;

handle_client_message(#de_client{mpx=Apps}=Client, {App, Msg}) ->
  case lists:member(App, Apps) of
    true -> gen_server:call(?SERVER, {handle_client_message, Client, App, Msg});
    false -> {error, unsubscribed}
  end;

handle_client_message(_,_) -> ok.

client_disconnected(Client) -> gen_server:call(?SERVER, {client_disconnected, Client}).

handle_call({subscribe, #de_client{meta = Meta, mpx = Apps, data = AppData} = Client, App, Data}, _, State) ->
  Reply = 
    case deliverly_server:find_handler(App) of
      false -> 
        ?D({app_not_exist, App}),
        {error, noexist};
      Handler ->
        Res =
          case Data of
            [] -> Handler:authorize(app_client(App, Client), maps:get(auth, Meta, []));
            _ -> Handler:authorize(app_client(App, Client), Data)
          end,
        case Res of
          {error, Reason} -> 
            ?ACCESS("AUTH_FAILED ~p ~p ~p",[App, Client#de_client.path, Reason]),
            Res;
          Tuple -> 
            #de_client{data=ClientData, encoder=Encoder}=NewClient = element(2, Res),
            ?ACCESS("AUTH_SUCCESS ~p ~p",[App,NewClient#de_client.path]),
            ets:insert(?APP, NewClient),
            gen_server:cast(?SERVER, {reply_to_client, NewClient, open}),
            if tuple_size(Tuple) =:= 3 ->
              gen_server:cast(?SERVER, {reply_to_client, NewClient, element(3, Tuple)});
              true -> pass
            end,
            NewClientData = #{data => ClientData, encoder => Encoder},
            {ok, Client#de_client{mpx=[App|Apps], data=maps:put(App, NewClientData, AppData)}}
        end
    end,
  {reply, Reply, State};

handle_call({unsubscribe, #de_client{socket=Socket, data=AppData, mpx=Apps}=Client, App}, _, State) ->
  Reply = 
    case deliverly_server:find_handler(App) of
      false -> 
        ?D({app_not_exist, App}),
        {error, noexist};
      Handler ->
        ok = Handler:client_disconnected(app_client(App,Client)),
        ?ACCESS("DISCONNECT ~p ~p",[App,Client#de_client.path]),
        ets:match_delete(?APP, #de_client{app=App, socket=Socket, _='_'}),
        {ok, Client#de_client{mpx=lists:delete(App, Apps), data=maps:remove(App, AppData)}}
    end,
  {reply, Reply, State};

handle_call({client_disconnected, #de_client{mpx=Apps} = Client}, _, State) ->
  lists:map(
    fun(App) ->
      Handler = deliverly_server:find_handler(App),
      Handler:client_disconnected(app_client(App,Client))
    end,
    Apps
  ),
  {reply, ok, State};

handle_call({handle_client_message, #de_client{data=AppData}=Client, App, Message}, _, State) ->
  Handler = deliverly_server:find_handler(App),
  AppClient = app_client(App, Client),
  Reply =
    case Handler:handle_client_message(AppClient, de_client:decode(AppClient, Message)) of
      Atom when is_atom(Atom) -> Atom;
      {error, _}=Error -> Error;
      Tuple ->

        #de_client{data=NewData}=NewClient = element(2, Tuple),
        
        Type = 
          if tuple_size(Tuple) =:= 3 ->
            gen_server:cast(?SERVER, {reply_to_client, NewClient, element(3, Tuple)}),
            ok;
            true -> element(1,Tuple)
          end,
        {Type, Client#de_client{data=set_client_data(App,AppData,data,NewData)}}
    end,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast({reply_to_client, Client, Data}, State) ->
  send(Client, Data),
  {noreply, State};

handle_cast({handle_close, #de_client{mpx=Module, app=App}=Client}, State) ->
  %% first, request current client real state
  Module:info(Client, self()),
  Info = 
    receive
      {client_info, Info_} -> Info_
    after 100
      -> false
    end,
  %% update client state and send unsub message
  %% or close client
  if not Info 
    ->  
      ?D({failed_to_unsubscribe, Client});
    true ->
      case Info of 
        #de_client{mpx = [App]} ->
          Module:close(Info);
        #de_client{mpx = Apps, socket=Socket} ->
          Module:update(Info#de_client{mpx = lists:delete(App, Apps)}),
          ets:match_delete(?APP, #de_client{app=App, socket=Socket, _='_'}),
          gen_server:cast(?SERVER, {reply_to_client, Client, close})
      end
  end,
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% Client functions

%%% @doc
%%% Send make send call from original module but replace encoder with
%%% mpx (and store original encoder as mpx)
%%% @end

-spec send(client(), Data::any()) -> ok.

send(#de_client{mpx=Module, encoder=Encoder}=Client, Data) ->
  Module:send(Client#de_client{encoder=mpx, mpx=Encoder}, Data).

%%% @doc
%%% Encode message with original encoder and prepend with app name
%%% @end

-spec encode(client(), Data::any()) -> {Type::atom(), Msg::any()}.


encode(#de_client{app=App}, open) ->
  {text, prepend_msg(App, <<"open">>)};

encode(#de_client{app=App}, close) ->
  {text, prepend_msg(App, <<"close">>)};

encode(#de_client{mpx=Encoder, app=App}=Client, Data) ->
  case Encoder:encode(Client, Data) of
    {Type, Msg} -> {Type, prepend_msg(App, Msg)};
    List when is_list(List) -> encode_list(List, App);
    _ -> close
  end.

%% @doc
%% Unsubscribe client from app. If there is no more apps - close client.
%% @end

-spec close(client()) -> ok.

close(Client) ->
  gen_server:cast(?SERVER, {handle_close, Client}).

%% Encoder funtions
%% 
%% We need only decode function for mpx, 
%% because mpx itself doesn't send any messages.
%% 
%% The only case when we need encode is when app replies to client's message. 
%% But we handle it within 'hanle_client_message' calls. 
%% 
%% All messages are send within apps and use mpx:send/2 method.

-spec decode(client(), Msg::any()) -> {App::atom(), Msg::any()} | {App::atom(), Msg::any(), Data::any()}.

decode(_, {text,Data}) ->
  ?D({in, Data}),
  [App, Msg] = split_message(Data),
  parse_msg(binary_to_atom(App,utf8), Msg, text);

decode(_, {binary, Data}) ->
  [App, Msg] = split_message(Data),
  parse_msg(binary_to_atom(App,utf8), Msg, binary);

decode(_,_) -> false.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec encode_list(List::list(), App::atom()) -> list().

encode_list(List, App) ->
  [{Type, prepend_msg(App, Msg)} || {Type, Msg} <- List].


-spec prepend_msg(App::atom(), Msg::binary()) -> binary().

prepend_msg(App, Msg) ->
  BApp = atom_to_binary(App,utf8),
  <<BApp/binary,",",Msg/binary>>.

-spec parse_msg(App::atom(), Msg::binary(), Type::atom()) ->
  {App::atom(), Msg::any()} | {App::atom(), Msg::any(), Data::any()}.

parse_msg(AppName, <<"sub">>, _) ->
  {AppName, sub, []};

parse_msg(AppName, <<"sub?", QS/binary>>, _) ->
  {AppName, sub, cow_qs:parse_qs(QS)};

parse_msg(AppName, <<"unsub">>, _) ->
  {AppName, unsub};

parse_msg(App, Msg, Type) ->
  {App, {Type, Msg}}.

%%% @doc
%%% Split incoming message by app prefix "app,msg"->["app","msg"]
%%% @end

-spec split_message(binary()) -> list(binary()).

split_message(Bin) ->
  binary:split(Bin,<<",">>).

%%% @doc
%%% Create app client from mpx client (set app's data and encoder, and store client's module to mpx)
%%% @end

-spec app_client(atom(), client()) -> client().

app_client(App, #de_client{module=Mod, data=AppData}=Client) ->
  Client#de_client{mpx=Mod, module = mpx, data=get_client_data(App,AppData,data), encoder=get_client_data(App,AppData,encoder, raw_encoder), app=App}.

%%% @doc
%%% Get client-app data by key
%%% @end


get_client_data(App, AppData, Key) ->
  get_client_data(App, AppData, Key, undefined).

-spec get_client_data(atom(), map(), Key::atom(), Default::any()) -> atom().

get_client_data(App, AppData, Key, Default) ->
  maps:get(
    Key,
    maps:get(App, AppData, #{}),
    Default
  ).

%% @doc
%% Update client's data for app for a given key
%% @end

-spec set_client_data(App::atom(), AppData::map(), Key::atom(), Val::any()) -> NewAppData::map().

set_client_data(App, AppData, Key, Val) ->
  maps:put(
    App,
    maps:put(Key, Val, maps:get(App, AppData, #{})),
    AppData
  ).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_client_data_test() ->
  ?assertEqual('x', get_client_data(app, #{app => #{ data => 'x'}}, data)),
  ?assertEqual(undefined, get_client_data(app, #{}, data)).

set_client_data_test() ->
  ?assertMatch(#{app := #{data := 2}}, set_client_data(app, #{app => #{ data => 'x'}}, data, 2)),
  ?assertMatch(#{app:= #{data := 0}}, set_client_data(app, #{}, data, 0)).

decode_test() ->
  ?assertEqual({app, {text, <<"binary">>}}, decode(#de_client{}, {text, <<"app,binary">>})),
  ?assertEqual({'app/1/2', {binary, <<"binary string, very long, bla">>}}, decode(#de_client{}, {binary, <<"app/1/2,binary string, very long, bla">>})).

encode_test() ->
  C1 = #de_client{mpx=raw_encoder, app=app},
  ?assertEqual({text, <<"app,binary">>}, encode(C1, {text, <<"binary">>})),

  C2 = #de_client{mpx=json_encoder, app=app},
  ?assertEqual({text, <<"app,{\"test\":\"blabla\"}">>}, encode(C2, #{<<"test">> => <<"blabla">>})).

-endif.