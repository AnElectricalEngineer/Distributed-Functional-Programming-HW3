-module(my_server).
-behavior(gen_server).
-author("Joshua").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Function that starts a server.
% Parameters: Name - the name of the server
% Effects: Starts a new gen server, with the name Name, and module for callback code from the current module.
% Server state is defined as: 1) Name
%                             2) Number of running tasks
start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

% Init function.
% Parameters: [Name] - a list containing the name of the server
% Returns: ok, the name of the server, and 0 - the initial number of running tasks
init([Name]) ->
  {ok, {Name, 0}}.

% Implements handle_call in case request received is numRunningJobs
% Parameters: numRunningJobs - atom describing message type
%             Running Jobs   - current tasks of server - part of server state
%             Name           - name of Server - part of server state
% Returns:    Reply          - number of current running jobs
%             State          - current server state
handle_call({numRunningJobs}, _From, {Name, RunningJobs}) ->
  Reply = RunningJobs,
  State = {Name, RunningJobs},
  {reply, Reply, State}.

handle_cast({addTask, From, Function, MsgRef}, {Name, RunningJobs}) ->
  spawn(fun() -> perform_task(From, Function, MsgRef, Name) end),
  NewRunningJobs = RunningJobs + 1,
  {noreply, {Name, NewRunningJobs}};

handle_cast({notifyTaskFinished}, {Name, RunningJobs}) ->
  NewRunningJobs = RunningJobs - 1,
  {noreply, {Name, NewRunningJobs}}.

perform_task(From, Function, MsgRef, ServerName) ->
  F_result = Function(),
  From ! {MsgRef, F_result},
  gen_server:cast(ServerName, {notifyTaskFinished}).

% Default implementation
handle_info(_Info, State) ->
  {noreply, State}.

% Default implementation
terminate(_Reason, _State) ->
  ok.

% Default implementation
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.