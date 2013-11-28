-module(worker).
-behaviour(gen_server).
-define(TIMEOUT, 2000).

-export([start_link/1, process/2, init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link(State) -> gen_server:start_link(?MODULE, State, []).
init(State) ->
  io:format("worker started ~p ~n", [self()]),
  {ok, State, ?TIMEOUT}.

terminate(normal, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(_Message, _From, State) -> {reply, State, ?TIMEOUT}.

handle_cast({process, Data}, BalanserPid) ->
  io:format("processing ~p by ~p ~n", [Data, self()]),
  timer:sleep(500),
  balancer:ready(BalanserPid),
  {noreply, BalanserPid, ?TIMEOUT}.

handle_info(timeout, _State) ->
  exit(normal);
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State, ?TIMEOUT}.

process(Pid, Data) ->
  gen_server:cast(Pid, {process, Data}).
