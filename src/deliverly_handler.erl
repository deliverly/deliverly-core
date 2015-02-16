-module(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").

%% @doc
%% Authorize connection to app. Data is additional information provided by client during connection phase.
%% @end

-callback authorize(Client::client(), Data::any()) -> {ok, Client2::client()} | {reply, Client2::client(), Data::any()} | {error, Reason::atom()}.

%% @doc
%% Handle incoming data from client.
%% @end
 
-callback handle_client_message(Client::client(), Data::any()) -> ok | broadcast | {ok, Client2::client()} | {broadcast, Client2::client()} | {reply, Client2::client(), Data::any()} | {error, Reason::atom()}.

%% @doc
%% Handle other kind of data (messages from other nodes, external applications)
%% @end

-callback handle_message(Data::any(), Context::any()) -> ok | broadcast.

%% @doc
%% Handle client disconnected event
%% @end

-callback client_disconnected(Client::client()) -> ok.