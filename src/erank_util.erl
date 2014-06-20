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

fix_consume_rank([]) -> ok;
fix_consume_rank([{Identity, InvalidScore}|T]) ->
    {AccName, SN} = Identity,
    IdentityTerm = {binary_to_list(AccName), SN},
    erank_api:incr_score(consume, IdentityTerm, -InvalidScore),
    fix_consume_rank(T).

%%%===================================================================
%%% Internal functions
%%%===================================================================
