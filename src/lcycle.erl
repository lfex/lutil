-module(lcycle).
-behaviour(gen_server).

%% API - works with both stateful (pid) and stateless (list) versions
-export([new/1, next/1, last/1, take/2, lastn/2, reset/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([example/0]).

%%%===================================================================
%%% API - Polymorphic functions
%%%===================================================================

%% Create a new cycle gen_server with the given list (stateful only)
new(List) when is_list(List), List =/= [] ->
    {ok, Pid} = gen_server:start_link(?MODULE, List, []),
    Pid.

%% Get the next element - works with both pid (stateful) and list (stateless)
next(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, next);
next(List) when is_list(List), List =/= [] ->
    next_stateless(List).

%% Get the last element - works with both pid (stateful) and list (stateless)  
last(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, last);
last(List) when is_list(List), List =/= [] ->
    last_stateless(List).

%% Take N elements - works with both pid (stateful) and list (stateless)
take(Count, State) when is_integer(Count), Count >= 0 ->
    take_acc(Count, State, fun next_adapter/1, result_fun_for(State)).

%% Take N elements from end - works with both pid (stateful) and list (stateless)
lastn(Count, State) when is_integer(Count), Count >= 0 ->
    lastn_acc(Count, State, fun last_adapter/1, result_fun_for(State)).

%% Reset the cycle to its initial state (stateful only)
reset(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, reset).

%% Stop the cycle gen_server (stateful only)
stop(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(List) ->
    State = #{init => List, current => List},
    {ok, State}.

handle_call(next, _From, #{current := CurrentList} = State) ->
    {Value, NewCurrentList} = next_stateless(CurrentList),
    NewState = State#{current := NewCurrentList},
    {reply, Value, NewState};

handle_call(last, _From, #{current := CurrentList} = State) ->
    {Value, NewCurrentList} = last_stateless(CurrentList),
    NewState = State#{current := NewCurrentList},
    {reply, Value, NewState};

handle_call(reset, _From, #{init := InitList} = State) ->
    NewState = State#{current := InitList},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Core stateless functions
%%%===================================================================

next_stateless([Head|Tail]) ->
    NewList = Tail ++ [Head],
    {Head, NewList}.

last_stateless(List) when is_list(List), List =/= [] ->
    [LastElem|Rest] = lists:reverse(List),
    NewList = [LastElem] ++ lists:reverse(Rest),
    {LastElem, NewList}.

%% Result function selector
result_fun_for(State) when is_pid(State) -> fun stateful_result_fun/2;
result_fun_for(State) when is_list(State) -> fun stateless_result_fun/2.

%%%===================================================================
%%% Polymorphic accumulator functions
%%%===================================================================

%% Generic accumulator that works for both stateful and stateless
take_acc(Count, State, NextFun, ResultFun) ->
    take_acc_loop(Count, State, NextFun, ResultFun, []).

take_acc_loop(0, State, _NextFun, ResultFun, Acc) ->
    ResultFun(lists:reverse(Acc), State);
take_acc_loop(Count, State, NextFun, ResultFun, Acc) ->
    {Value, NewState} = NextFun(State),
    take_acc_loop(Count - 1, NewState, NextFun, ResultFun, [Value|Acc]).

lastn_acc(Count, State, LastFun, ResultFun) ->
    lastn_acc_loop(Count, State, LastFun, ResultFun, []).

lastn_acc_loop(0, State, _LastFun, ResultFun, Acc) ->
    ResultFun(Acc, State);
lastn_acc_loop(Count, State, LastFun, ResultFun, Acc) ->
    {Value, NewState} = LastFun(State),
    lastn_acc_loop(Count - 1, NewState, LastFun, ResultFun, [Value|Acc]).

%% Adapter functions that normalize stateful/stateless interfaces
next_adapter(Pid) when is_pid(Pid) ->
    Value = gen_server:call(Pid, next),
    {Value, Pid};
next_adapter(List) when is_list(List) ->
    next_stateless(List).

last_adapter(Pid) when is_pid(Pid) ->
    Value = gen_server:call(Pid, last),
    {Value, Pid};
last_adapter(List) when is_list(List) ->
    last_stateless(List).

stateful_result_fun(Acc, _State) -> Acc.
stateless_result_fun(Acc, State) -> {Acc, State}.

%%%===================================================================
%%% Example usage
%%%===================================================================

example() ->
    io:format("=== Stateful version ===~n"),
    % Create a cycle
    Cycle = new([1, 2, 3, 4]),
    
    io:format("Taking 6 elements forward: ~p~n", [take(6, Cycle)]),
    io:format("Current next: ~p~n", [next(Cycle)]),
    io:format("Current next: ~p~n", [next(Cycle)]),
    io:format("Resetting...~n"),
    reset(Cycle),
    io:format("After reset, taking 10: ~p~n", [take(10, Cycle)]),
    stop(Cycle),
    
    io:format("~n=== Stateless version ===~n"),
    % Stateless usage
    List1 = [1, 2, 3, 4],
    {Value1, List2} = next(List1),
    io:format("Next from ~p: ~p, new state: ~p~n", [List1, Value1, List2]),
    
    {Value2, List3} = last(List2),
    io:format("Last from ~p: ~p, new state: ~p~n", [List2, Value2, List3]),
    
    {Values, FinalList} = take(21, List3),
    io:format("Take 21 from ~p: ~p, final state: ~p~n", [List3, Values, FinalList]).
