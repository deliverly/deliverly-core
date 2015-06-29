%% Copyright
-author("palkan").

-record(de_client,{
  connected_at :: non_neg_integer() | atom(),
  module :: atom(),
  encoder :: atom(),
  socket ::pid() | atom(),
  host :: string() | atom(),
  path :: string() | atom(),
  app :: atom(),
  meta ::any(),
  data ::any()
}).

-type(client() ::#de_client{}).

-export_type([client/0]).