[![Build Status](https://travis-ci.org/deliverly/deliverly-core.svg?branch=master)](https://travis-ci.org/deliverly/deliverly-core)

DelivErly core lib
======

DelivErly is a real-time applications platform.

Currently supports the following communication protocols:
- WebSockets [Cowboy](https://github.com/ninenines/cowboy).

## Usage

### Application Handler

DelivErly provides an abstraction over communication protocols through [`deliverly_handler`](blob/master/src/deliverly_handler.erl) behaviour.

Example handler:

```erlang
-module(example).
-behaviour(deliverly_handler).

%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).


%% This method is called when client is trying to connect to your application.
%% This is the place to put authentication logic and register clients.
%%
%% `Data` is proplist containing query string parameters from connection string.
authorize(Client, Data) ->
  %% Use referer check verification
  case de_auth_referer:verify(Client, Data, ["example.com"]) of
    true -> ok;
    false -> {error, 3401}
  end.

%% Incoming message from client
handle_client_message(Client, Message) ->
 %% Simply broadcast message to all application's clients
 de_client:broadcast_to(deliverly:connections_list(example), Message),
 %% Return `broadcast` to broadcast this message to connected nodes
 broadcast.


%% Handle other messages (e.g. from other nodes or applications)
handle_message(_,_) -> ok.

%% Handle disconnected client
client_disconnected(_Client) -> ok.

```

Register your handler with `deliverly:register_handler(sample, example)` and your application will be served from `/ws/sample`path.


### Authentication

TBD

### Distributed applications

TBD

### Multiplexing

TBD

