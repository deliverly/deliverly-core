-module(probe_handler).
-export([
  init/2
]).

init(Req, Opts) ->
  {ok, cowboy_req:reply(200, Req), Opts}.