-module(server_supervisor).
-behavior(supervisor).
-author("Joshua").

%% API
-export([init/1]).


init([]) ->
  ServerOneChild = {server1, {server1, }}