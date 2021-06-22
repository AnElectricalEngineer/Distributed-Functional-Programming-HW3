-module(my_server).
-behavior(gen_server).
-author("Joshua").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Function that starts a server.
% Parameters: Name - the name of the server
% Effects: Starts a new gen server, with the name Name, and module for callback code from the current module.
start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

% Init function.
% Parameters: [Name] - a list containing the name of the server
% Returns: ok, the name of the server, and 0 - the initial number of running tasks
init([Name]) ->
  {ok, {Name, 0}}.

% Implements handle_call in case request received is numRunningJobs
% Parameters: numRunningJobs - atom describing message type
%             Name           - server name
%             Running Jobs   - current state of server
% Returns:    Reply - tuple of server name and number of current running jobs
%             RunningJobs - current server state
handle_call({numRunningJobs, Name}, _From, RunningJobs) ->
  Reply = {Name, RunningJobs},
  {reply, Reply, RunningJobs}.

handle_cast(Request, State) ->
  erlang:error(not_implemented).

% Default implementation
handle_info(_Info, State) ->
  {noreply, State}.

% Default implementation
terminate(_Reason, _State) ->
  ok.

% Default implementation
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.