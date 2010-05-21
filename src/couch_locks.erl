-module(couch_locks).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, list/0, get/2, create/3, delete/1, delete_all/0, delete_all/1, expire_all/0]).

-record(state, {locks, scopes, monitors}).

-include("couch_db.hrl").

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list() ->
    gen_server:call(?MODULE, list).

get(Token, Pid) ->
    gen_server:call(?MODULE, {get, Token, Pid}).

create(DbName, Scopes, Timeout) when is_list(Scopes) ->
    gen_server:call(?MODULE, {create, DbName, Scopes, Timeout});
create(DbName, Scope, Timeout) ->
    create(DbName, [Scope], Timeout).

delete(Token) ->
    gen_server:cast(?MODULE, {delete, Token}).

delete_all() ->
    gen_server:cast(?MODULE, delete_all).

delete_all(DbName) ->
    gen_server:cast(?MODULE, {delete_all, DbName}).

expire_all() ->
	?MODULE ! expire.

%%
%% Callbacks
%%

init([]) ->
    %% Sweep expired locks every 3 seconds
	ExpireInterval = list_to_integer(couch_config:get("locks", "expire_interval", "3000")),
    timer:send_interval(ExpireInterval, expire),
    {ok, #state{locks = ets:new(locks, []), scopes = ets:new(scopes, []), monitors = ets:new(monitors, [])}}.

handle_call(list, _From, #state{scopes = Scopes} = State) ->
    {reply, {ok, ets:tab2list(Scopes)}, State};
handle_call({get, Token, Pid}, _From, #state{locks = Locks, monitors = Monitors} = State) ->
    case ets:member(Locks, Token) of
        true ->
            MonitorRef = erlang:monitor(process, Pid),
            ets:insert(Monitors, {MonitorRef, Token}),
            ets:update_element(Locks, Token, {4, infinity}),
            {reply, true, State};
        false ->
            {reply, false, State}
    end;
handle_call({create, DbName, ScopeList, Timeout}, _From, #state{locks = Locks, scopes = Scopes} = State) ->
    Fun = fun(Scope) -> ets:member(Scopes, {DbName, Scope}) end,
    case lists:any(Fun, ScopeList) of
        false ->
            Token = couch_uuids:new(),
            ExpiresAt = now_msec() + Timeout,
            ScopeObjects = lists:map(fun(Scope) -> {{DbName, Scope}, Token} end, ScopeList),
            ets:insert(Locks, {Token, DbName, ScopeObjects, ExpiresAt}),
            ets:insert(Scopes, ScopeObjects),
            {reply, {ok, Token}, State};
        true ->
            {reply, {error, conflict}, State}
    end.

handle_cast({delete, Token}, State) ->
    delete_lock_ets(Token, State),
    {noreply, State};
handle_cast(delete_all, #state{locks = Locks, scopes = Scopes} = State) ->
    MatchSpec = [{{'_', '_', '_', '$1'}, [{'=/=', '$1', infinity}], ['$_']}],
    Result = ets:select(Locks, MatchSpec),
    lists:foreach(fun(Lock) -> delete_lock_ets(Lock, Locks, Scopes) end, Result),
    {noreply, State};
handle_cast({delete_all, DbName}, #state{locks = Locks, scopes = Scopes} = State) ->
    MatchSpec = [{{'_', '$1', '_', '$2'}, [{'==', '$1', DbName}, {'=/=', '$2', infinity}], ['$_']}],
    Result = ets:select(Locks, MatchSpec),
    lists:foreach(fun(Lock) -> delete_lock_ets(Lock, Locks, Scopes) end, Result),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(expire, #state{locks = Locks, scopes = Scopes} = State) ->
    Now = now_msec(),
    MatchSpec = [{{'_', '_', '_', '$1'}, [{'=<', '$1', Now}, {'=/=', '$1', infinity}], ['$_']}],
    Result = ets:select(Locks, MatchSpec),
    lists:foreach(fun(Lock) -> delete_lock_ets(Lock, Locks, Scopes) end, Result),
    {noreply, State};
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, #state{monitors = Monitors} = State) ->
    case ets:lookup(Monitors, MonitorRef) of
        [{MonitorRef, Token}] ->
            ets:delete(Monitors, MonitorRef),
            delete_lock_ets(Token, State),
            {noreply, State};
        [] ->
            {noreply, State}
    end;
handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%

delete_lock_ets(Token, #state{locks = Locks, scopes = Scopes} = _State) ->
    case ets:lookup(Locks, Token) of
        [Lock] ->
            delete_lock_ets(Lock, Locks, Scopes);
        [] ->
            true
    end.

delete_lock_ets({Token, _DbName, ScopeObjects, _ExpiresAt}, Locks, Scopes) ->
    lists:foreach(fun({Key, _Token}) -> ets:delete(Scopes, Key) end, ScopeObjects),
    ets:delete(Locks, Token).

now_msec() ->
	{Mega, Sec, Micro} = erlang:now(),
	Mega * 1000000000 + Sec * 1000 + Micro div 1000.
