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
-export([decode/1]).

%%% module functions
-export([send/2, close/1, encode/2]).

-record(state,{
  handlers=#{} ::map()
}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

deliverly_handler() ->
  ?SERVER.

init([]) ->
  ?D(<<"Starting multiplex application">>),
  {ok, #state{}}.

authorize(Client,_) -> {reply, Client#de_client{data=#{}, encoder=mpx, meta=[]}}.

handle_message(Data, Context) -> gen_server:call(?SERVER, {handle_message, Data, Context}).

handle_client_message(Client, {App, sub}) -> 
  gen_server:call(?SERVER, {subscribe, Client, App});

handle_client_message(Client, {App, unsub}) -> 
  gen_server:call(?SERVER, {unsubscribe, Client, App});

handle_client_message(#de_client{meta=Apps}=Client, {App, Msg}) ->
  case lists:member(App, Apps) of
    true -> gen_server:call(?SERVER, {handle_message, Client, App, Msg});
    false -> unsubscribed
  end.

client_disconnected(Client) -> gen_server:call(?SERVER, {client_disconnected, Client}).

handle_call({subscribe, _Client, _App}, _, State) ->
  {reply, ok, State};

handle_call({unsubscribe, _Client, _App}, _, State) ->
  {reply, ok, State};

handle_call({client_disconnected, #de_client{meta=Apps} = Client}, _, #state{handlers=Handlers} = State) ->
  lists:map(
    fun(App) ->
      Handler = maps:get(App,Handlers),
      Handler:client_disconnected(app_client(App,Client))
    end,
    Apps
  ),
  {reply, ok, State};

handle_call({handle_client_message, #de_client{data=AppData}=Client, App, Message}, _, #state{handlers=Handlers}=State) ->
  Handler = maps:get(App, Handlers),
  Reply = 
    case Handler:handle_client_message(app_client(App, Client), Message) of
      Atom when is_atom(Atom) -> Atom;
      Tuple ->

        #de_client{data=NewData}=NewClient = element(2, Tuple),
        
        if tuple_size(Tuple) =:= 3 ->
          send(NewClient, element(3, Tuple));
          true -> pass
        end,
        {element(1,Tuple), Client#de_client{data=set_client_data(App,AppData,data,NewData)}}
    end,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

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
%%% mpx (and store original encoder as meta)
%%% @end

-spec send(client(), Data::any()) -> ok.

send(#de_client{meta=Module, encoder=Encoder}=Client, Data) ->
  Module:send(Client#de_client{encoder=mpx, meta=Encoder}, Data).  

%%% @doc
%%% Encode message with original encoder and prepend with app name
%%% @end

-spec encode(client(), Data::any()) -> {Type::atom(), Msg::any()}.

encode(#de_client{meta=Encoder,app=App}, Data) ->
  {Type, Msg} = Encoder:encode(Data),
  {Type, prepend_msg(App, Msg)}.

%% @doc
%% Note: Close really disconnects client, not unsubscribes!
%% @end

-spec close(client()) -> ok.

close(#de_client{meta=Module}=Client) ->
  Module:close(Client).

%% Encoder funtions
%% 
%% We need only decode function for mpx, 
%% because mpx itself doesn't send any messages.
%% 
%% All messages are send within apps and use mpx:send/2 method.

-spec decode(Msg::any()) -> {App::atom(), Msg::any()}.

decode({text,Data}) ->
  [App, Msg] = split_message(Data),
  parse_msg(binary_to_atom(App,utf8), Msg, text);

decode({binary, Data}) ->
  [App, Msg] = split_message(Data),
  parse_msg(binary_to_atom(App,utf8), Msg, binary);

decode(_) -> false.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


-spec prepend_msg(App::atom(), Msg::binary()) -> binary().

prepend_msg(App, Msg) ->
  BApp = atom_to_binary(App,utf8),
  <<BApp/binary,",",Msg/binary>>.

-spec parse_msg(App::atom(), Msg::binary(), Type::atom()) -> {App::atom(), Msg::any()}.

parse_msg(sub, AppName, _) ->
  {binary_to_atom(AppName,utf8), sub};

parse_msg(unsub, AppName, _) ->
  {binary_to_atom(AppName,utf8), unsub};

parse_msg(App, Msg, Type) ->
  {App, {Type, Msg}}.

%%% @doc
%%% Split incoming message by app prefix "app,msg"->["app","msg"]
%%% @end

-spec split_message(binary()) -> list(binary()).

split_message(Bin) ->
  binary:split(Bin,<<",">>).

%%% @doc
%%% Create app client from mpx client (set app's data and encoder, and store client's module to meta)
%%% @end

-spec app_client(atom(), client()) -> client().

app_client(App, #de_client{module=Mod, data=AppData}=Client) ->
  Client#de_client{meta=Mod, data=get_client_data(App,AppData,data), encoder=get_client_data(App,AppData,encoder), app=App}.

%%% @doc
%%% Get client-app data by key
%%% @end

-spec get_client_data(atom(), map(), Key::atom()) -> atom().

get_client_data(App, AppData, Key) ->
  maps:get(
    Key,
    maps:get(App, AppData)
  ).

%% @doc
%% Update client's data for app for a given key
%% @end

-spec set_client_data(App::atom(), AppData::map(), Key::atom(), Val::any()) -> NewAppData::map().

set_client_data(App, AppData, Key, Val) ->
  maps:update(
    App,
    maps:update(Key, Val, maps:get(App, AppData)),
    AppData
  ).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_client_data_test() ->
  ?assertEqual('x', get_client_data(app, #{app => #{ data => 'x'}}, data)),
  ?assertError(_, get_client_data(app, #{}, data)).

set_client_data_test() ->
  ?assertMatch(#{app := #{data := 2}}, set_client_data(app, #{app => #{ data => 'x'}}, data, 2)),
  ?assertError(_, set_client_data(app, #{}, data, 0)).

decode_test() ->
  ?assertEqual({app, {text, <<"binary">>}}, decode({text, <<"app,binary">>})),
  ?assertEqual({'app/1/2', {binary, <<"binary string, very long, bla">>}}, decode({binary, <<"app/1/2,binary string, very long, bla">>})).

encode_test() ->
  C1 = #de_client{meta=raw_encoder, app=app},
  ?assertEqual({text, <<"app,binary">>}, encode(C1, {text, <<"binary">>})),

  C2 = #de_client{meta=json_encoder, app=app},
  ?assertEqual({text, <<"app,{\"test\":\"blabla\"}">>}, encode(C2, #{<<"test">> => <<"blabla">>})).

-endif.