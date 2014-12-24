-module(json_encoder).

-export([
  encode/1,
  decode/1
]).

encode(Bin) when is_binary(Bin) ->
  {binary, Bin};

encode(Data) ->
  {text, jsx:encode(Data)}.

decode({text,Data}) ->
  jsx:decode(Data, [return_maps]);

decode({binary, Data}) ->
  Data;

decode(_) ->
  #{}.