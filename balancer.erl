-module(balancer).
-behaviour(gen_server).

-export([ready/1, process/2, start_link/0, init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link() -> gen_server:start_link(?MODULE, [], []).

handle_call({process, Data}, _From, [H | T]) ->
  worker:process(H, Data),
  {reply, ok, T};
handle_call({process, Data}, _From, []) ->
  {ok, Pid} = worker:start_link(self()),
  worker:process(Pid, Data),
  {reply, ok, []};
handle_call(ready, {Pid, _Tag}, List) ->
  io:format("ready ~p ~n", [Pid]),
  {reply, ok, [Pid | List]}.

handle_cast(Msg, State) -> handle_info(Msg, State).

handle_info({'EXIT', Pid, _Reason}, State) ->
  io:format("worker ~p stopped ~n", [Pid]),
  {noreply, lists:delete(Pid, State)};
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

process(Pid, Data) ->
  gen_server:call(Pid, {process, Data}).

ready(Pid) ->
  io:format("send ready to ~p ~n", [Pid]),
  gen_server:call(Pid, ready).

init(State) ->
  process_flag(trap_exit, true),
  {ok, State}.

terminate(normal, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
