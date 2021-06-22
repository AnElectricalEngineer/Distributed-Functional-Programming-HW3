-module(loadBalance).
-author("Joshua").

%% API
-export([startServers/0, stopServers/0, numberOfRunningFunctions/1, calcFun/3]).

% Starts and initializes three servers, and supervises them.
startServers() ->
  server_supervisor:start_link().

stopServers() ->
  % TODO check that this is proper way to terminate children
  % TODO check that calling these on server_supervisor works - ref used name of server
  supervisor:terminate_child(server_supervisor, server1),
  supervisor:delete_child()
  supervisor:terminate_child(server_supervisor, server2),
  supervisor:terminate_child(server_supervisor, server3),

