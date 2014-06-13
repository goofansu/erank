%%%-------------------------------------------------------------------
%%% @author goofansu <goofan.su@gmail.com>
%%% @copyright (C) 2014, goofansu
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2014 by goofansu <goofan.su@gmail.com>
%%%-------------------------------------------------------------------
-module(eredis_keygen).

%% API
-export([rank_key/1, identity_key/1]).

%%%===================================================================
%%% API
%%%===================================================================

rank_key(RankType) ->
    io_lib:format("rank:~p", [RankType]).

identity_key({Accname, SN}) ->
    io_lib:format("account:~p:~p", [SN, Accname]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
