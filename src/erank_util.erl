%%%-------------------------------------------------------------------
%%% @author goofansu <goofan.su@gmail.com>
%%% @copyright (C) 2014, goofansu
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2014 by goofansu <goofan.su@gmail.com>
%%%-------------------------------------------------------------------
-module(erank_util).

%% API
-export([clear_rank/1]).
-export([fix_consume_rank/1]).

%%%===================================================================
%%% API
%%%===================================================================
clear_rank(RankType) ->
    Key = eredis_keygen:rank_key(RankType),
    eredis_api:del(Key).

fix_consume_rank(L) ->
    RankKey = eredis_keygen:rank_key(consume),
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["DEL", RankKey]),
    fix_consume_rank_1(L, C, RankKey).

fix_consume_rank_1([], _C, _RankKey) -> ok;
fix_consume_rank_1([{Identity, Score}|T], C, RankKey) ->
    RedisScore = erank_misc:redis_score(Score),
    {AccName, SN} = Identity,
    IdentityTerm = {binary_to_list(AccName), SN},
    {ok, <<"1">>} = eredis:q(C, ["ZADD", RankKey, RedisScore, IdentityTerm]),
    fix_consume_rank_1(T, C, RankKey).

%%%===================================================================
%%% Internal functions
%%%===================================================================
