%%%
%%% Genlib

-module(genlib_map).

%%

-export([get/2]).
-export([get/3]).
-export([deepput/3]).
-export([mget/2]).
-export([foreach/2]).
-export([truemap/2]).
-export([compact/1]).
-export([atomize/1]).
-export([atomize/2]).
-export([binarize/1]).
-export([binarize/2]).
-export([diff/2]).

-export([get_in/2, get_in/3, mget_in/2]).
-export([put_in/3]).
-export([update_in/3, update_in/4]).
%%

-spec get(any(), map()) -> undefined | term().

get(Key, Map) ->
    get(Key, Map, undefined).

-spec get(any(), map(), Default) -> Default | term() when
    Default :: term().

get(Key, Map = #{}, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

-spec mget([Key | {Key, Default}], map()) -> [Default | term()] when
    Key :: atom() | binary() | number(),
    Default :: term().

mget([{Key, Default} | Rest], Map) ->
    [get(Key, Map, Default) | mget(Rest, Map)];

mget([Key | Rest], Map) ->
    [get(Key, Map) | mget(Rest, Map)];

mget([], _Map) ->
    [].

-spec deepput(KeyPath :: [term()], Value :: term(), map()) -> map().

deepput([Key], Value, Map) ->
    maps:put(Key, Value, Map);

deepput([Key | Rest], Value, Map) ->
    maps:put(Key, deepput(Rest, Value, get(Key, Map, #{})), Map).

-spec truemap(Function, map()) -> #{any() => any()}
    when
        Function :: fun((Key :: any(), Value :: any()) -> {Key :: any(), Value :: any()}).

truemap(F, Map = #{}) ->
    maps:fold(fun (K, V, M) -> {Kn, Vn} = F(K, V), maps:put(Kn, Vn, M) end, #{}, Map).

-spec foreach(Function, map()) -> ok when
    Function :: fun((Key :: any(), Value :: any()) -> any()).

foreach(F, Map = #{}) ->
    maps:fold(fun (K, V, _) -> F(K, V), ok end, ok, Map).

-spec compact(map()) -> map().

compact(Map = #{}) ->
    maps:fold(fun (K, undefined, M) -> maps:remove(K, M); (_, _, M) -> M end, Map, Map).

-spec atomize(#{binary() => any()}) -> #{atom() => any()}.

atomize(Map) ->
    truemap(fun (K, V) -> {binary_to_atom(K, utf8), V} end, Map).

-spec binarize(#{atom() => any()}) -> #{binary() => any()}.

binarize(Map) ->
    truemap(fun (K, V) -> {atom_to_binary(K, utf8), V} end, Map).

-spec atomize(Map, pos_integer() | infinity) -> AtomicMap when
    Map       :: #{binary() => Map | term()},
    AtomicMap :: #{atom() | binary() => AtomicMap | term()}.

atomize(Map, 1) ->
    atomize(Map);

atomize(Map, N) ->
    atomize(maps:map(fun (_, V) when is_map(V) -> atomize(V, decrement(N)); (_, V) -> V end, Map)).

-spec binarize(AtomicMap, pos_integer() | infinity) -> Map when
    Map       :: #{binary() => Map | term()},
    AtomicMap :: #{atom() | binary() => AtomicMap | term()}.

binarize(Map, 1) ->
    binarize(Map);

binarize(Map, N) ->
    binarize(maps:map(fun (_, V) when is_map(V) -> binarize(V, decrement(N)); (_, V) -> V end, Map)).

decrement(infinity) -> infinity;
decrement(N) -> N - 1.

-spec diff(#{}, Since :: #{}) -> Diff :: #{}.

diff(Map, Since) ->
    maps:fold(fun (K, V, M) -> case get(K, M, make_ref()) of V -> maps:remove(K, M); _ -> M end end, Map, Since).


%% clojure-style accessors

-spec get_in(KeyPath::[term()], Map::map()) -> term().

get_in(Path, Map) when is_list(Path), is_map(Map) ->
    lists:foldl(fun maps:get/2, Map, Path).


-spec get_in(KeyPath::[term()], Map::map(), Default::term()) -> term().

get_in(Path, Map, Default) when is_list(Path), is_map(Map) ->
    get_in(make_ref(), Path, Map, Default).

get_in(Ref, _, Ref, Default) -> Default;
get_in(_, [], Val, _) -> Val;
get_in(_, _, NotMap, Default) when not is_map(NotMap) -> Default;
get_in(Ref, [H | T], M, D) -> get_in(Ref, T, maps:get(H, M, Ref), D).


-type path() :: [any()] | {[any()], Default::any()}.
-spec mget_in(Paths, map()) -> [any()] when
    Paths :: [path()] | #{any() => path()}.

mget_in(Paths, Map) when is_list(Paths), is_map(Map) ->
    lists:map(fun
        ({K, D}) -> get_in(K, Map, D);
        (K) -> get_in(K, Map)
    end, Paths);

mget_in(Paths, Map) when is_map(Paths), is_map(Map) ->
    maps:map(fun
        (_, {Path, D}) -> get_in(Path, Map, D);
        (_, Path) -> get_in(Path, Map)
    end, Paths).


-spec put_in(KeyPath::[term()], Value::term(), map()) -> map().

put_in(Path, Value, Map) ->
    deepput(Path, Value, Map).


-spec update_in(Key::[any()], Fun :: fun((any()) -> any()), map()) -> map().

update_in([Key], Fun, Map) ->
    maps:put(Key, Fun(maps:get(Key, Map)), Map);

update_in([Key | Rest], Fun, Map) ->
    maps:put(Key, update_in(Rest, Fun, get(Key, Map, #{})), Map).


-spec update_in(Key::[any()], Fun::fun((any()) -> any()), Init::term(), map()) -> map().

update_in([Key], Fun, Init, Map) ->
    maps:put(Key, case maps:find(Key, Map) of
        {ok, Value} -> Fun(Value);
        _ -> Init
    end, Map);

update_in([Key | Rest], Fun, Init, Map) ->
    maps:put(Key, update_in(Rest, Fun, Init, get(Key, Map, #{})), Map).
