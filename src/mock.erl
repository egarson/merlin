%%%-----------------------------------------------------------------------------
%%% @author Edward Garson <egarson@gmail.com>
%%% @copyright (C) 2012 Edward Garson
%%% @doc A mocking library for Erlang.
%%% @end
%%%
%%% This source file is distributed under the terms of the MIT license as
%%% described by the file MIT-LICENSE included with this software.
%%%------------------------------------------------------------------------------
-module(mock).
-author('Edward Garson <egarson@gmail.com>').
-compile(export_all).
-import(util, [log/1,log/2]).
-import(lists,[nth/2,seq/3]).

expect({ModuleName,FuncName},{Times,Value},Response) ->
    Mock = list_to_atom(util:uuid()),
    register(Mock, spawn(mock, loop, [[]])),
    Mock ! {expect,Times,Value},
    ResponseVal = erl_parse:abstract(Response),
    Forms = [{attribute,1,module,ModuleName},
             {function,5,FuncName,1,
              [{clause,5,
                [{var,5,'Actual'}],
                [],
                [{op,6,'!',
                  {atom,6,Mock},
                  {tuple,6,
                   [{atom,6,actual},{var,6,'Actual'}]}},
                 ResponseVal]}]}],
    {ok,M,Bin} = compile:forms(Forms, [verbose,export_all]),
    code:purge(M),
    code:load_binary(M, atom_to_list(ModuleName)++".erl", Bin),
    Mock;

expect({ModuleName,FuncName},Value,Response) ->
    expect({ModuleName,FuncName},{once,Value},Response).

verify([]) ->
    ok;

verify(Mock) when not is_list(Mock) ->
    Mock ! {verify,self()},
    receive
        {ok,Data} ->
            %% log("** verify: ~p~n", [Data]),
            verify(Data);
        Error ->
            log("Error: ~p~n", [Error])
    end;

verify(MockData) ->
    log("Verifying: ~p~n", [MockData]),
    {NextActuals,[Expected|Rest]} = lists:splitwith(fun(E) -> element(1,E) =:= actual end, MockData),
    {actual,ActualCalls,ActualValue} = combine_next_actuals(NextActuals),
    case Expected of
        {expect,anytime,ExpectedValue} ->
            log("ActualValue: ~p, ExpectedValue: ~p, Rest: ~p~n", [ActualValue, ExpectedValue, Rest]),
            verify(ActualValue, ExpectedValue, Rest);
        {expect,ExpectedCallValue,ExpectedValue} -> %% We always have an expected: we put it there
            ExpectedCalls = parse_num:parse(ExpectedCallValue),
            log("ActualCalls: ~p, ExpectedCalls: ~p, ActualValue: ~p, ExpectedValue: ~p, Rest: ~p~n", [ActualCalls, ExpectedCalls, ActualValue, ExpectedValue, Rest]),
            verify(ActualCalls, ExpectedCalls, ActualValue, ExpectedValue, Rest)
    end.

verify(0,_,_,_,_) ->
	{error,no_calls_received};
verify(ActualCalls, ExpectedCalls, ActualValue, ExpectedValue, Rest) when ActualCalls == ExpectedCalls ->
    verify(ActualValue, ExpectedValue, Rest);
verify(ActualCalls, ExpectedCalls,_,_,_) when ActualCalls < ExpectedCalls ->
    {error,unrealized_expectation};
verify(_ActualCalls, _ExpectedCalls, irrelevant, ExpectedValue, Rest) ->
    verify(irrelevant, ExpectedValue, Rest);
verify(_,_,_,_,_) ->
    {error,unexpected_call}.

verify(ActualValue, any_number, Rest) ->
    case is_number(ActualValue) of
        true -> verify(Rest);
        _ -> {error,type_mismatch}
    end;

verify(irrelevant,_,Rest) ->
    verify(Rest);
verify(ActualValue, ExpectedValue, Rest) when ActualValue == ExpectedValue ->
    verify(Rest);
verify(_,_,_) ->
    {error, expected_actual_mismatch}.

combine_next_actuals([]) ->
    {actual,0,irrelevant};
combine_next_actuals(Actuals=[{actual,Value}|_]) ->
    {NextSameActuals,_} = lists:splitwith(fun({Kind,Val}) -> Kind == actual andalso Val =:= Value end, Actuals),
    {actual,length(NextSameActuals),Value}.

loop(Data) ->
    receive
        {expect,Times,Value} ->
            %% log("Expect ~p: ~p~n", [Times,Value]),
            loop([{expect,Times,Value}|Data]);

        {actual,ActualValue} ->
            %% log("Actual: ~p, Data: ~p~n", [ActualValue, Data]),
            loop([{actual,ActualValue}|Data]);

        Else -> self() ! Else
    end,
    receive
        {verify,From} ->
            From ! {ok,Data}
    end.

where(ModuleName,FunctionName) ->
    {ModuleName,FunctionName}.

with(Value) ->
    Value.

is_called(Value) ->
	Value.

is_called(Times,Value) ->
    {Times,Value}.

respond_with(Response) ->
    Response.
