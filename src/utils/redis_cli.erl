-module(redis_cli).
-include_lib("deliverly/include/priv.hrl").
%% API
-export([q/1]).

q(Command) ->
  poolboy:transaction(?R_POOL, fun(Worker) ->
    eredis:q(Worker, Command)
  end).