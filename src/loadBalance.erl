-module(loadBalance).
-author("Joshua").

%% API
%%-export([startServers/0, stopServers/0, numberOfRunningFunctions/1, calcFun/3]).
-export([startServers/0, numberOfRunningFunctions/1, calcFun/3]).

% Starts and initializes three servers, and supervises them.
startServers() ->
  server_supervisor:start_link().

%%stopServers() ->
%%  % TODO check that this is proper way to terminate children
%%  % TODO check that calling these on server_supervisor works - ref used name of server
%%  supervisor:terminate_child(server_supervisor, server1),
%%  supervisor:delete_child()
%%  supervisor:terminate_child(server_supervisor, server2),
%%  supervisor:terminate_child(server_supervisor, server3),


numberOfRunningFunctions(1) ->
  gen_server:call(server1, {numRunningJobs});
numberOfRunningFunctions(2) ->
  gen_server:call(server2, {numRunningJobs});
numberOfRunningFunctions(3) ->
  gen_server:call(server3, {numRunningJobs}).

calcFun(ClientPID, F, MsgRef) ->
  NumJobsServer1 = {server1, numberOfRunningFunctions(1)},
  NumJobsServer2 = {server2, numberOfRunningFunctions(2)},
  NumJobsServer3 = {server3, numberOfRunningFunctions(3)},
  ChosenServer = chooseBestServer(NumJobsServer1, NumJobsServer2, NumJobsServer3),
  gen_server:cast(ChosenServer, {addTask, ClientPID, F, MsgRef}),
  ok.

% Function that chooses the server with the lowest load
% Parameters: Server1Data - tuple of server1 name, and current load
%             Server2Data - tuple of server2 name, and current load
%             Server3Data - tuple of server3 name, and current load
% Returns:    ChosenServerName - Chosen server name
chooseBestServer(Server1Data, Server2Data, Server3Data) ->
  % First choose better server: server1 or server2
  IntermChosenServer =
    case element(2, Server1Data) < element(2, Server2Data) of
      true -> Server1Data;
      false -> Server2Data
    end,

  % Now choose overall best server
  ChosenServer =
    case element(2, IntermChosenServer) < element(2, Server3Data) of
      true -> IntermChosenServer;
      false -> Server3Data
    end,

  % Return name of chosen server (with lowest load)
  element(1, ChosenServer).

