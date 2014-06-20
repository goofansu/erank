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
    L1 = prepare_consume_ranks(L, []),
    {ok, Saved} = eredis:q(C, ["ZADD", RankKey | L1]),
    lager:info("Total saved count:~s", [Saved]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 准备批量插入的排行数据
prepare_consume_ranks([], Acc) -> Acc;
prepare_consume_ranks([{Identity, Score}|T], Acc) ->
    {AccName, SN} = Identity,
    IdentityTerm = {binary_to_list(AccName), SN},
    RedisScore = erank_misc:redis_score(Score),
    Acc1 = [RedisScore, IdentityTerm | Acc],
    prepare_consume_ranks(T, Acc1).
