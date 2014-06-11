%%%-------------------------------------------------------------------
%%% @author goofansu <goofan.su@gmail.com>
%%% @copyright (C) 2014, goofansu
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2014 by goofansu <goofan.su@gmail.com>
%%%-------------------------------------------------------------------
-module(erank_misc).

%% API
-export([unixtime/0]).
-export([redis_score/1, realworld_score/1]).
-export([realworld_rank/1]).

-define(MAX_TIMESTAMP, 2000000000).
-define(TIMESTAMP_MOD, 1000000000).

%%%===================================================================
%%% API
%%%===================================================================

redis_score(Score) ->
    Score * ?TIMESTAMP_MOD + (?MAX_TIMESTAMP - unixtime()).

realworld_score(undefined) -> 0;
realworld_score(Bin) ->
    Score = binary_to_integer(Bin),
    Float = (Score - Score rem ?TIMESTAMP_MOD) / ?TIMESTAMP_MOD,
    trunc(Float).

%% redis中排名从0开始，这里转换为从1开始，0表示没有排名
realworld_rank(undefined) -> 0;
realworld_rank(Bin) -> 
    Rank = binary_to_integer(Bin),
    Rank + 1.

unixtime() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

%%%===================================================================
%%% Internal functions
%%%===================================================================
