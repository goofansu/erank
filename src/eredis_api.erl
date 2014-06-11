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
-export([zadd/3, zincrby/3, zscore/2]).
-export([zrevrank/2]).
-export([zrevrange/2, zrevrange/3]).
-export([zrevrange_withscores/2, zrevrange_withscores/3]).
-export([zrevrangebyscore/3]).
-export([zrevrangebyscore/4]).
-export([pipeline_rank_score/2]).

-define(POOL, rank_pool).

%%%===================================================================
%%% API
%%%===================================================================

zadd(RankType, Score, Member) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZADD", Key, Score, Member]).

zincrby(RankType, Increment, Member) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZINCRBY", Key, Increment, Member]).

zscore(RankType, Member) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZSCORE", Key, Member]).

zrevrank(RankType, Member) ->
    Key = rank_key(RankType),
    eredis_pool:q(?POOL, ["ZREVRANK", Key, Member]).

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
