%%% @doc 
%%% This module is used to send data to clients using Client:send(Data) or too close 
%%% connection using Client:close(), where Client is #de_client{} (client()).
%%% Also contains `broadcast_to` and `close_all` functions to manipulate with lists of connections.
%%% @end
-module(de_client).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-export([
  send/2,
  close/1,
  encode/2,
  decode/2,
  broadcast_to/2,
  broadcast_to/3,
  close_all/1,
  close_all/2
]).

%% @doc
%% Close client connection
%% @end

-spec close(Client::client()) -> ok.

close(#de_client{module = M} = Client) ->
  M:close(Client).

%% @doc
%% Send data to client
%% @end

-spec send(Client::client(), Data::any()) -> ok.
send(#de_client{module = M} = Client, Data) ->
  M:send(Client, Data).

%% @doc
%% Encode data to send to client
%% @end

-spec encode(Client::client(), Data::any()) -> any().
encode(#de_client{encoder = M}=Client, Data) ->
  M:encode(Client, Data).

%% @doc
%% Decode data received from client
%% @end

-spec decode(Client::client(), Data::any()) -> any().
decode(#de_client{encoder = M}=Client, Data) ->
  M:decode(Client, Data).


%% @doc
%% Send data to many clients.
%% @end

-spec broadcast_to(Clients ::list(client()) | #{any() => client()}, Data::any()) -> ok.

broadcast_to(Clients, Data) ->
  broadcast_to(Clients, Data, undefined).


%% @doc
%% Send data to many clients with filter conditions. Returns {error, badarg} if bad conditions.
%% Filter conditions can be:
%% - Function (client()) -> true|false;
%% - {except, Client | Socket | Clients | Sockets}
%% @end

-spec broadcast_to(Clients ::list(client()) | #{any() => client()}, Data::any(), Conditions::any()) -> ok | {error, badarg}.

broadcast_to(Clients, Data, undefined) ->
  lists:foreach(
      fun(Client) -> send(Client, Data) end,
      clients_to_list(Clients));

broadcast_to(Clients, Data, Fun) when is_function(Fun,1) ->
  lists:foreach(
      fun(Client) -> 
        case Fun(Client) of
          true -> send(Client, Data);
          false -> pass
        end
      end,
      clients_to_list(Clients));


broadcast_to(Clients, Data, {except, ClientsFilter}) ->
  Fun = except_fun(ClientsFilter),
  broadcast_to(Clients, Data, Fun);

broadcast_to(_,_,_Filter) ->
  ?D({wrong_filter, _Filter}),
  {error, badarg}.


%% @doc
%% Close all clients
%% @end

-spec close_all(Clients ::list(client()) | #{any() => client()}) -> ok.

close_all(Clients) ->
  close_all(Clients, undefined).


%% @doc
%% Close all clients with filter conditions. Returns {error, badarg} if bad conditions.
%% Filter conditions can be:
%% - Function (client()) -> true|false;
%% - {except, Client | Socket | Clients | Sockets}
%% @end

-spec close_all(Clients ::list(client()) | #{any() => client()}, Conditions::any()) -> ok | {error, badarg}.

close_all(Clients, undefined) ->
  lists:foreach(
      fun(Client) -> close(Client) end,
      clients_to_list(Clients));

close_all(Clients, Fun) when is_function(Fun,1) ->
  lists:foreach(
      fun(Client) -> 
        case Fun(Client) of
          true -> close(Client);
          false -> pass
        end
      end,
      clients_to_list(Clients));


close_all(Clients, {except, Clients}) ->
  Fun = except_fun(Clients),
  close_all(Clients, Fun);

close_all(_,_Filter) ->
  ?D({wrong_filter, _Filter}),
  {error, badarg}.


%%% Internal

clients_to_list(Clients) when is_list(Clients) ->
  Clients;

clients_to_list(Clients) when is_map(Clients) ->
  maps:values(Clients);

clients_to_list(_) -> [].

except_fun(#de_client{socket=Socket}) ->
  except_one_socket(Socket);

except_fun(Socket) when is_pid(Socket) ->
  except_one_socket(Socket);

except_fun(Clients) when is_list(Clients) ->
  except_many_sockets(Clients);

except_fun(_Clients) ->
  ?D({wrong_except, _Clients}),
  {error, badarg}.

except_one_socket(Socket) ->
  fun(#de_client{socket=Socket_}) ->
    Socket_ =/= Socket
  end.

except_many_sockets(Clients) ->
  Sockets = lists:map(fun client_to_socket/1, Clients),
  fun(#de_client{socket=Socket}) ->
    not lists:member(Socket, Sockets)
  end.

client_to_socket(#de_client{socket = Socket}) ->
  Socket;

client_to_socket(Socket) when is_pid(Socket) ->
  Socket.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(Client(Id), #de_client{module=test_app_app, socket=list_to_pid("<0.0."++Id++">")}).
-define(Pid(Id), list_to_pid(Id)).

clients_to_list_test() ->
  Clients = [?Client("1"), ?Client("2")],
  Map = #{1 => ?Client("1"), 2 => ?Client("2")},
  ?assertEqual(Clients, clients_to_list(Clients)),
  ?assertEqual(Clients, clients_to_list(Map)),
  ?assertEqual([], clients_to_list({clients, Clients})).

client_to_socket_test() ->
  ?assertEqual(?Pid("<0.0.1>"), client_to_socket(?Client("1"))),
  ?assertEqual(?Pid("<0.0.1>"), client_to_socket(?Pid("<0.0.1>"))).

except_fun_test() ->
  Client = ?Client("1"),
  Client2 = ?Client("2"), 
  Client3 = ?Client("3"),

  F1 = except_fun(?Pid("<0.0.1>")),
  ?assertNot(F1(Client)),
  ?assert(F1(Client2)),

  F2 = except_fun(Client),
  ?assertNot(F2(Client)),
  ?assert(F2(Client2)),

  F3 = except_fun([Client2, Client3]),
  ?assertNot(F3(Client3)),
  ?assert(F3(Client)),
  ?assertNot(F3(Client2)),
 
  Pids = [?Pid("<0.0.2>"), ?Pid("<0.0.3>")],
  F4 = except_fun(Pids),
  ?assertNot(F4(Client2)),
  ?assertNot(F4(Client3)),
  ?assert(F4(Client)).

decode_test() ->
  C1 = #de_client{encoder=raw_encoder},
  ?assertEqual({test, <<"binary">>}, decode(C1, {test, <<"binary">>})),

  C2 = #de_client{encoder=json_encoder},
  ?assertEqual(#{<<"test">> => <<"blabla">>}, decode(C2, {text, <<"{\"test\":\"blabla\"}">>})).


encode_test() ->
  C1 = #de_client{encoder=raw_encoder},
  ?assertEqual({test, <<"binary">>}, encode(C1, {test, <<"binary">>})),

  C2 = #de_client{encoder=json_encoder},
  ?assertEqual({text, <<"{\"test\":\"blabla\"}">>}, encode(C2, #{<<"test">> => <<"blabla">>})).

-endif.