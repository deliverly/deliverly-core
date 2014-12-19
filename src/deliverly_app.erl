-module(deliverly_app).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("deliverly/include/priv.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ?I("Starting application: deliverly"),
  ConfigPath = case ?Config(config,undefined) of
    undefined -> "deliverly.config";
    Else -> Else
  end,
  ?D({config_path, ConfigPath}),
  ulitos_app:load_config(?APP, ConfigPath, ["etc"]),
  ?D({config, application:get_all_env(?APP)}),
  case ?Config(use_sync, false) of
    true -> sync:go();
    false -> pass
  end,
  deliverly_sup:start_link().

stop(_State) ->
  ok.
