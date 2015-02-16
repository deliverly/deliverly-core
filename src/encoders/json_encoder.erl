-module(json_encoder).

-export([
  encode/2,
  decode/2
]).

encode(_, Bin) when is_binary(Bin) ->
  {binary, Bin};

encode(_, Data) ->
  {text, jsx:encode(Data)}.

decode(_, {text, Data}) ->
  jsx:decode(Data, [return_maps]);

decode(_, {binary, Data}) ->
  Data;

decode(_, _) ->
  false.