%%%-----------------------------------------------------------------------------
%%% @author Edward Garson <egarson@gmail.com>
%%% @copyright (C) 2012 Edward Garson
%%%
%%% This source file is distributed under the terms of the MIT license as
%%% described by the file MIT-LICENSE included with this software.
%%%------------------------------------------------------------------------------
-module(util).
-compile(export_all).

log(M) -> io:format(M, []).
log(M,A) -> io:format(M,A).

%% log(M) -> error_logger:info_msg(M, []).
%% log(M,A) -> error_logger:info_msg(M,A).

error(M) -> error_logger:error_msg(M).
error(M,A) -> error_logger:error_msg(M,A).

uuid() ->
    string:strip(os:cmd("uuidgen"), right, $\n).

list_to_atom_list(List) ->
    lists:map(fun(L) -> list_to_atom(L) end, List).

%% Gee whiz, no index method for list - and caveat emptor: this is O(n)
index(Member, List) ->
    index(Member, List, 1).

index(Member, List, Index) when Index =< length(List) ->
    case lists:nth(Index, List) of
        Member -> Index;
        _ -> index(Member, List, Index + 1)
    end;
index(_,_,_) ->
    not_a_member.
