%%

-module(genlib_map_tests).

-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    [
        ?_assertEqual("oh", genlib_map:get(hey, Map)),
        ?_assertEqual(undefined, genlib_map:get(what_nigga, Map)),
        ?_assertEqual(babe, genlib_map:get(who, Map, babe)),
        ?_assertEqual(["say oh", "what", undefined, 42], genlib_map:mget([i, listen, dont, {know, 42}], Map))
    ].

truemap_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    InvMap = #{"oh" => hey, "what" => listen, "say oh" => i, "back and..." => come},
    [
        ?_assertEqual(InvMap, genlib_map:truemap(fun (K, V) -> {V, K} end, Map))
    ].

deepput_test_() ->
    [
        ?_assertError(_, genlib_map:deepput(answ, 42, #{})),
        ?_assertEqual(#{answ => 42, foo => fighters}, genlib_map:deepput([answ], 42, #{foo => fighters})),
        ?_assertEqual(
            #{$b => #{$a => #{$n => #{$j => #{$o => 42}}}}, ba => zinga},
            genlib_map:deepput("banjo", 42, #{ba => zinga})
        )
    ].

diff_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    [
        ?_assertEqual(#{}, genlib_map:diff(Map, Map)),
        ?_assertEqual(Map, genlib_map:diff(Map, #{})),
        ?_assertEqual(#{poo => hoo}, genlib_map:diff(Map#{poo => hoo}, Map)),
        ?_assertEqual(#{hey => hoo}, genlib_map:diff(Map#{hey => hoo}, Map)),
        ?_assertEqual(#{this_is => 'undefined'}, genlib_map:diff(#{this_is => 'undefined'}, #{this_is => 'not'})),
        ?_assertEqual(#{this_is => 'not'      }, genlib_map:diff(#{this_is => 'not'}, #{this_is => 'undefined'}))
    ].

get_in_test_() ->
    M = #{a => 1, b => 2, c => #{d => 3, e => 4}, g => false},
    R = make_ref(),
    [
        ?_assertEqual(M, genlib_map:get_in([], M)),
        ?_assertEqual(1, genlib_map:get_in([a], M)),
        ?_assertEqual(false, genlib_map:get_in([g], M)),
        ?_assertEqual(4, genlib_map:get_in([c, e], M)),

        ?_assertError({badkey, e}, genlib_map:get_in([e], M)),
        ?_assertError({badkey, x}, genlib_map:get_in([c, x], M)),
        ?_assertError(function_clause, genlib_map:get_in(notlist, M)),
        ?_assertError(function_clause, genlib_map:get_in(notlist, M, R)),

        ?_assertEqual(M, genlib_map:get_in([], M, 0)),
        ?_assertEqual(R, genlib_map:get_in([e], M, R)),
        ?_assertEqual(R, genlib_map:get_in([c, x], M, R))
    ].

mget_in_test_() ->
    M = #{a => 1, b => 2, c => #{d => 3, e => 4}, g => false},
    R = make_ref(),
    [
        ?_assertEqual([1, 4], genlib_map:mget_in([ [a], [c, e]], M)),
        ?_assertEqual([1, R], genlib_map:mget_in([ [a], {[c, x], R}], M)),

        ?_assertEqual(
            #{k1 => 1, k2 => R},
            genlib_map:mget_in(#{k1 => [a], k2 =>{[c, x], R}}, M))
    ].

update_in_test_() ->
    [
        ?_assertEqual(
            #{a => #{b => #{c => 42}}},
            genlib_map:update_in([a, b, c], fun(C) -> C+1 end, #{a => #{b => #{c => 41}}})),

        ?_assertError(
            {badkey, c},
            genlib_map:update_in([a, b, c], fun(C) -> C+1 end, #{})),
        ?_assertError(
            {badmap, 44},
            genlib_map:update_in([a, b, c], fun(C) -> C+1 end, #{a => #{b => 44}})),


        ?_assertEqual(
            #{a => #{b => #{c => 42}}},
            genlib_map:update_in([a, b, c], fun(C) -> C+1 end, 42, #{}))
    ].
