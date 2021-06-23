-module(server_supervisor).
-behavior(supervisor).
-author("Joshua").

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  ServerOneChild = {server1, {my_server, start_link, [server1]},
    permanent, brutal_kill, worker, [my_server]},
  ServerTwoChild = {server2, {my_server, start_link, [server2]},
    permanent, brutal_kill, worker, [my_server]},
  ServerThreeChild = {server3, {my_server, start_link, [server3]},
    permanent, brutal_kill, worker, [my_server]},
  {ok, {{one_for_one, 1, 1}, [ServerOneChild, ServerTwoChild, ServerThreeChild]}}.
