%% Copyright
-author("palkan").

-define(APP, deliverly).
-define(Config(X,Y),ulitos_app:get_var(?APP,X,Y)).

-record(de_client,{
  connected_at ::non_neg_integer() | atom(),
  module ::atom(),
  socket ::pid() | atom(),
  path ::string() | atom(),
  app ::atom()
}).

-type client() ::#de_client{}.