-module(auth_handler).
-include_lib("deliverly/include/log.hrl").

-export([
  init/2,
  allowed_methods/2,
  is_authorized/2,
  content_types_accepted/2,
  request_token/2
]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
  User = ulitos_app:get_var(deliverly, api_user, <<"">>),
  Pass = ulitos_app:get_var(deliverly, api_pass, <<"">>),
  case {User, Pass} of
    {<<"">>, <<"">>} ->
      {true, Req, State};
    _ ->
      case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Pass} ->
          {true, Req, State};
        _ ->
          {{false, <<"">>}, Req, State}
      end
  end.

content_types_accepted(Req, State) ->
  {[{'*', request_token}], Req, State}.

request_token(Req, State) ->
  {ok, QS, _} = cowboy_req:body_qs(Req),
  Answer = de_auth_token:request_token(QS),
  Req1 = cowboy_req:set_resp_header(<<"Content-Type">>, <<"application/json">>, Req),
  Req2 = cowboy_req:set_resp_body(jsx:encode(Answer), Req1),
  {true, Req2, State}.
