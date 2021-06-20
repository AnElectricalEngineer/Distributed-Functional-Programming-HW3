-module(loadBalance).
-author("Joshua").

%% API
-export([startServers/0, stopServers/0, numberOfRunningFunctions/1, calcFun/3]).

startServers() ->
