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
-export([incr_score/3]).
-export([get_score/2, get_rank/2, get_rank_score/2]).
-export([get_score_by_rank/2]).
-export([get_previous_member/2]).
-export([list_member_range_by_score/3]).
-export([list_member_range_by_score/4]).

%%%===================================================================
%%% API
%%%===================================================================

%% 增加指定排行榜的指定玩家的分数
incr_score(RankType, Member, Increment) ->
    Score = new_score(RankType, Member, Increment),
    eredis_api:zadd(RankType, Score, Member).

%% 获得指定排行榜的指定玩家的分数
get_score(RankType, Member) ->
    case eredis_api:zscore(RankType, Member) of
        {ok, undefined} -> 0;
        {ok, Val} -> erank_misc:realworld_score(Val)
    end.

%% 获得指定排行榜的指定玩家的排名
-spec get_rank(atom(), term()) -> integer() | not_found.
get_rank(RankType, Member) ->
    case eredis_api:zrevrank(RankType, Member) of
        {ok, undefined} -> 0;
        {ok, Val} -> erank_misc:realworld_rank(Val)
    end.

%% 获得指定排行榜的指定玩家的排名和分数
get_rank_score(RankType, Member) ->
    [{ok,Rank}, {ok,Score}] = eredis_api:pipeline_rank_score(RankType, Member),
    [erank_misc:realworld_rank(Rank),
     erank_misc:realworld_score(Score)].

%% 获得前一个玩家的信息
get_previous_member(RankType, Member) ->
    case get_rank(RankType, Member) of
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

%% 获得指定分数段内的玩家信息列表
list_member_range_by_score(RankType, Max, Min) ->
    {ok, L} = eredis_api:zrevrangebyscore(RankType, Max, Min),
    lists:map(fun(Member)-> binary_to_term(Member) end, L).

%% 获得指定分数段内的玩家信息列表，限制返回个数
list_member_range_by_score(RankType, Max, Min, Limit) ->
    {ok, L} = eredis_api:zrevrangebyscore(RankType, Max, Min, Limit),
    lists:map(fun(Member)-> binary_to_term(Member) end, L).

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_score(RankType, Member, Increment) ->
    {ok, Val} = eredis_api:zscore(RankType, Member),
    Score = erank_misc:realworld_score(Val),
    erank_misc:redis_score(Score+Increment).
