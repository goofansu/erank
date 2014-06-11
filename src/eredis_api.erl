%%%-------------------------------------------------------------------
%%% @author goofansu <goofan.su@gmail.com>
%%% @copyright (C) 2014, goofansu
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2014 by goofansu <goofan.su@gmail.com>
%%%-------------------------------------------------------------------
-module(eredis_api).

%% API
-export([zincrby/3, zrevrank/2, zscore/2]).
-export([zrevrange/2, zrevrange/3]).
-export([zrevrange_withscores/2, zrevrange_withscores/3]).
-export([zrevrangebyscore/3]).
-export([zrevrangebyscore/4]).
-export([pipeline_rank_score/2]).

-define(POOL, rank_pool).

%%%===================================================================
%%% API
%%%===================================================================

zincrby(RankType, Increment, Member) ->
    Score = new_score(RankType, Member, Increment),
    eredis_pool:q(?POOL, ["ZADD", rank_key(RankType), Score, Member]).

zrevrank(RankType, Member) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZREVRANK", Key, Member]).

zscore(RankType, Member) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZSCORE", Key, Member]).

zrevrange(RankType, Rank) ->
    zrevrange(RankType, Rank, Rank).

zrevrange(RankType, Start, Stop) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZREVRANGE", Key, Start, Stop]).

zrevrange_withscores(RankType, Rank) ->
    zrevrange_withscores(RankType, Rank, Rank).

zrevrange_withscores(RankType, Start, Stop) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZREVRANGE", Key, Start, Stop, "WITHSCORES"]).

zrevrangebyscore(RankType, Max, Min) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZREVRANGEBYSCORE", Key, Max, Min]).

zrevrangebyscore(RankType, Max, Min, Limit) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZREVRANGEBYSCORE", Key, Max, Min,
                          "LIMIT", 0, Limit]).

pipeline_rank_score(RankType, Member) ->
    Key = rank_key(RankType),
    Pipeline = [["ZREVRANK", Key, Member],
                ["ZSCORE", Key, Member]],
    eredis_pool:qp(?POOL, Pipeline).

%%%===================================================================
%%% Internal functions
%%%===================================================================

rank_key(RankType) ->
    io_lib:format("rank:~p", [RankType]).

new_score(RankType, Member, Increment) ->
    {ok, Val} = zscore(RankType, Member),
    Score = erank_misc:realworld_score(Val),
    erank_misc:redis_score(Score+Increment).
