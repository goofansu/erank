%%%-------------------------------------------------------------------
%%% @author goofansu <goofan.su@gmail.com>
%%% @copyright (C) 2014, goofansu
%%% @doc 个人排名服务
%%%
%%% @end
%%% Created : 11 Jun 2014 by goofansu <goofan.su@gmail.com>
%%%-------------------------------------------------------------------
-module(erank_player_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([add_consume_rank/4, get_consume_rank/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(POOL, ?MODULE).

%% 消费排行榜
-define(RANK_CONSUME, consume).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% 更新消费排行
add_consume_rank(Identity, Nickname, ServerId, AddScore) ->
    poolboy:transaction(?POOL, fun(Worker) ->
        gen_server:call(Worker, {'add_consume_rank', Identity, Nickname, ServerId, AddScore})
    end).

%% 获得我以及前一名的消费排名和分数
get_consume_rank(Identity) ->
    poolboy:transaction(?POOL, fun(Worker) ->
        gen_server:call(Worker, {'get_consume_rank', Identity})
    end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({'add_consume_rank', Identity, Nickname, ServerId, AddScore}, _From, State) ->
    {ok, Reply} = erank_api:incr_score(?RANK_CONSUME, Identity, AddScore),
    erank_api:save_nickname_serverid(Identity, Nickname, ServerId),
    {reply, Reply, State};
handle_call({'get_consume_rank', Identity}, _From, State) ->
    [MyRank, MyScore] = erank_api:get_rank_score(?RANK_CONSUME, Identity),
    case MyRank =< 1 of
        true ->
            Reply = [{MyRank, MyScore}, {0, 0}],
            {reply, Reply, State};
        false ->
            PrevRank = MyRank - 1,
            PrevScore = erank_api:get_score_by_rank(?RANK_CONSUME, PrevRank),
            Reply = [{MyRank, MyScore}, {PrevRank, PrevScore}],
            {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
