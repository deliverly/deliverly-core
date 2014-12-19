%% Copyright
-author("palkan").

-record(de_client,{
  connected_at ::non_neg_integer() | atom(),
  module ::atom(),
  socket ::pid() | atom(),
  path ::string() | atom(),
  app ::atom()
}).

-type(client() ::#de_client{}).

-export_type([client/0]).