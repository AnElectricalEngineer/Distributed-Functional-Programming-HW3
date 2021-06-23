-module(server_supervisor).
-behavior(supervisor).
-author("Joshua").

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  ServerOneChild = {server1, {my_server, start_link, [server1]},
    permanent, infinity, worker, [my_server]}, %TODO change infinity -> brutal_kill maybe
  ServerTwoChild = {server2, {my_server, start_link, [server2]},
    permanent, infinity, worker, [my_server]},
  ServerThreeChild = {server3, {my_server, start_link, [server3]},
    permanent, infinity, worker, [my_serve3]},
  % TODO change 50, 50 to something else
  {ok, {{one_for_one, 50, 50}, [ServerOneChild, ServerTwoChild, ServerThreeChild]}}.
