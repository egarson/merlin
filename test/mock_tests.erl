%%%-----------------------------------------------------------------------------
%%% @author Edward Garson <egarson@gmail.com>
%%% @copyright (C) 2012 Edward Garson
%%%
%%% This source file is distributed under the terms of the MIT license as
%%% described by the file MIT-LICENSE included with this software.
%%%-----------------------------------------------------------------------------
-module(mock_tests).
-author('Edward Garson <egarson@gmail.com>').
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-import(lists, [seq/2]).
-import(util, [log/1,log/2]).
-import(mock, [expect/3,where/2,is_called/1,is_called/2,with/1,respond_with/1,verify/1,call_on/2]).

basic_mock_test() ->
    Mock = expect(where(calc,double),is_called(with(Value=5)),respond_with(Expected=10)),
    Actual = calc:double(Value),
    ?assertEqual(ok, verify(Mock)),
    ?assertEqual(Expected, Actual).

times_once_ok_test() ->
    Mock = expect(where(calc,double),is_called(once,with(Value=5)),respond_with(Expected=10)),
    Actual = calc:double(Value),
    ?assertEqual(Expected, Actual),
    ?assertEqual(ok, verify(Mock)).

times_twice_ok_test() ->
    Mock = expect(where(calc,double),is_called(twice,with(Value=5)),respond_with(Expected=10)),
    [?assertEqual(Expected, calc:double(Value))||_<-seq(1,2)],
    ?assertEqual(ok, verify(Mock)).

anytime_test() ->
    Mock = expect(where(foo,bar),is_called(anytime,with("foo")),respond_with("bar")),
    [?assertEqual("bar", foo:bar("foo"))||_<-seq(1,2)],
    ?assertEqual(ok, verify(Mock)).

anytime_no_calls_test() ->
    Mock = expect(where(foo,bar),is_called(anytime,with("foo")),respond_with("bar")),
    ?assertEqual(ok, verify(Mock)).

no_actuals_test() ->
    Mock = expect(where(calc,double),is_called(once,with(5)),respond_with(10)),
    ?assertEqual({error,no_calls_received}, verify(Mock)).

unrealized_expectation_test() ->
    Mock = expect(where(calc,double),is_called(twice,with(Value=5)),respond_with(10)),
    calc:double(Value),
    ?assertEqual({error,unrealized_expectation}, verify(Mock)).

too_many_actuals_test() ->
    Mock = expect(where(calc,double),is_called(once,with(Value=5)),respond_with(Expected=10)),
    calc:double(Value),
    Actual = calc:double(Value), %% Oops, superfluous call
    ?assertEqual(Expected, Actual),
    ?assertEqual({error,unexpected_call}, verify(Mock)).

expected_actual_value_mismatch_test() ->
    Mock = expect(where(calc,double),is_called(once,with(5)),respond_with(10)),
    calc:double(36),
    ?assertEqual({error,expected_actual_mismatch}, verify(Mock)).

any_number_test() ->
    Mock = expect(where(calc,double),is_called(once,with(any_number)),respond_with(some_atom)), %% anyFloat, anyInt, anyAtom...
    calc:double(17),
    ?assertEqual(ok, verify(Mock)).

any_number_error_test() ->
    Mock = expect(where(calc,double),is_called(once,with(any_number)),respond_with(not_a_number)),
    calc:double(oops),
    ?assertEqual({error,type_mismatch}, verify(Mock)).
