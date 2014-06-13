%%%-------------------------------------------------------------------
%%% @author goofansu <goofan.su@gmail.com>
%%% @copyright (C) 2014, goofansu
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2014 by goofansu <goofan.su@gmail.com>
%%%-------------------------------------------------------------------
-module(erank_api).

%% API
-export([incr_score/3, save_nickname/2]).
-export([get_score/2, get_rank/2, get_rank_score/2]).
-export([get_score_by_rank/2]).
-export([get_previous_member/2]).
-export([list_identity_above_min_score/2]).
-export([list_member_limited_above_min_score/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% 保存玩家昵称
save_nickname(Identity, Nickname) ->
    eredis_api:set_nickname(Identity, Nickname).

%% 增加指定排行榜的指定玩家的分数
incr_score(RankType, Identity, Increment) ->
    Score = new_score(RankType, Identity, Increment),
    eredis_api:zadd(RankType, Score, Identity).

%% 获得指定排行榜的指定玩家的分数
get_score(RankType, Identity) ->
    case eredis_api:zscore(RankType, Identity) of
        {ok, undefined} -> 0;
        {ok, Val} -> erank_misc:realworld_score(Val)
    end.

%% 获得指定排行榜的指定玩家的排名
-spec get_rank(atom(), term()) -> integer() | not_found.
get_rank(RankType, Identity) ->
    case eredis_api:zrevrank(RankType, Identity) of
        {ok, undefined} -> 0;
        {ok, Val} -> erank_misc:realworld_rank(Val)
    end.

%% 获得指定排行榜的指定玩家的排名和分数
get_rank_score(RankType, Identity) ->
    [{ok,Rank}, {ok,Score}] = eredis_api:pipeline_rank_score(RankType, Identity),
    [erank_misc:realworld_rank(Rank),
     erank_misc:realworld_score(Score)].

%% 获得前一个玩家的信息
get_previous_member(RankType, Identity) ->
    case get_rank(RankType, Identity) of
        0 -> {error, rank_not_found};
        1 -> {error, rank_first};
        Rank ->
            RedisRank = Rank-1,
            {ok, [Val]} = eredis_api:zrevrange(RankType, RedisRank-1),
            {ok, binary_to_term(Val)}
    end.

%% 获得指定排名的分数
get_score_by_rank(_RankType, Rank) when Rank =< 0 -> 0;
get_score_by_rank(RankType, Rank) ->
    case eredis_api:zrevrange_withscores(RankType, Rank-1, Rank-1) of
        {ok, [_, Score]} -> erank_misc:realworld_score(Score);
        _ -> 0
    end.

%% 获得指定排行榜的满足最低分数要求的玩家身份
list_identity_above_min_score(RankType, MinScore) ->
    {ok, L} = eredis_api:zrevrange_withscores(RankType, 0, -1),
    L1 = make_identity_scores(L, []),
    F = fun(E, Acc)-> filter_by_min_score(E, MinScore, Acc) end,
    L2 = lists:foldl(F, [], L1),
    lists:reverse(L2).

%% 获得指定排名段，并且分数高于指定值的玩家信息(身份+昵称)
list_member_limited_above_min_score(_RankType, _MinScore, Limit) when Limit =< 0 -> [];
list_member_limited_above_min_score(RankType, MinScore, Limit) ->
    {ok, L} = eredis_api:zrevrange_withscores(RankType, 0, Limit-1),
    L1 = make_identity_scores(L, []),
    F = fun(E, Acc)-> filter_by_min_score(E, MinScore, Acc) end,
    case lists:foldl(F, [], L1) of
        [] -> [];
        L2 ->
            L3 = lists:reverse(L2),
            {ok, Nicknames} = eredis_api:mget_nicknames(L3),
            lists:zip(L3, Nicknames)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_score(RankType, Identity, Increment) ->
    {ok, Val} = eredis_api:zscore(RankType, Identity),
    Score = erank_misc:realworld_score(Val),
    erank_misc:redis_score(Score+Increment).

filter_by_min_score({I, Score}, MinScore, Acc) ->
    case Score >= MinScore of
        true -> [I|Acc];
        false -> Acc
    end.

make_identity_scores([], Acc) -> lists:reverse(Acc);
make_identity_scores([Identity, Score|T], Acc) ->
    Acc1 = [{binary_to_term(Identity),
             erank_misc:realworld_score(Score)}
            |Acc],
    make_identity_scores(T, Acc1).
