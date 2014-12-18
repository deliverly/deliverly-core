-module(deliverly_server_test).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup_/0, fun cleanup_/1, F}).

setup_() ->
  lager:start(),
  ulitos_app:set_var(?APP, default_app, true),
  deliverly:start(),
  test_app_app:start([],[]).

cleanup_(_) ->
  application:stop(lager),
  test_app_app:stop([]),
  deliverly:stop().

register_handler_test_() ->
  [{"Register app",
    ?setup(
      fun(_) ->
        {inorder,
          [
            ensure_default_started_t_(),
            add_app_to_state_t_(),
            app_connections_list_t_()
          ]
        }
      end
    )
  }].

ensure_default_started_t_() ->
  ?_assert(lists:member(default, deliverly:apps_list())).

add_app_to_state_t_() ->
  ?_assert(lists:member(test_app, deliverly:apps_list())).


app_connections_list_t_() ->
  deliverly:register_handler(test_app_bad, test_app_app),
  deliverly_server:auth_client(#de_client{socket=1, app = test_app},[]),
  deliverly_server:auth_client(#de_client{socket=2, app = test_app_bad},[]),
  [
    ?_assertEqual(2, length(deliverly:connections_list())),
    ?_assertEqual(1, length(deliverly:connections_list(test_app)))
  ].


clients_test_() ->
  [{"Clients auth and disconnect tests",
    ?setup(
      fun(_) ->
        {inorder,
          [
            auth_success_t_(),
            auth_failed_t_(),
            client_disconnected_t_()
          ]
        }
      end
    )
  }].

auth_success_t_() ->
  Res = deliverly_server:auth_client(#de_client{socket=1, app = test_app},[]),
  Size = length(deliverly:connections_list()),
  [
    ?_assertEqual(ok, Res),
    ?_assertEqual(1, Size)
  ].

auth_failed_t_() ->
  Res = deliverly_server:auth_client(#de_client{socket=2, app = test_app},[{fake, fake}]),
  Size = length(deliverly:connections_list()),
  [
    ?_assertMatch({error, _}, Res),
    ?_assertEqual(1, Size)
  ].

client_disconnected_t_() ->
  deliverly_server:client_disconnected(#de_client{socket=1, app = test_app}),
  Size = length(deliverly:connections_list()),
  [
    ?_assertEqual(0, Size)
  ].

