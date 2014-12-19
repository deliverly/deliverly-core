-module(notfound_handler).
-include_lib("deliverly/include/log.hrl").
-export([
  init/2
]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  ?D({route_not_found, Path}),
  Body = <<"<h1>404 Page Not Found</h1>">>,
  {ok, cowboy_req:reply(404, [], Body, Req), Opts}.