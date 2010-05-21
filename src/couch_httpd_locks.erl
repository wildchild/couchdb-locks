-module(couch_httpd_locks).
-include("couch_db.hrl").

-export([handle_locks_req/2, handle_all_locks_req/1]).
-import(couch_httpd, [send_json/3]).

%%
%%  HTTP API
%%

%% GET /_locks
handle_locks_req(#httpd{method = 'GET', path_parts = [_, _]} = Req, _Db) ->
    {ok, Locks} = couch_locks:list(),
    send_json(Req, 200, {[{total_rows, length(Locks)}, {rows, list_to_json(Locks, [])}]});

%% POST /_locks/?scope=MyScope
%% POST /_locks/?scope=MyScope&timeout=3000
%% POST /_locks/?timeout=3000 -d ["Scope1", "Scope2", "ScopeN"]
handle_locks_req(#httpd{method = 'POST', path_parts = [_, _]} = Req, #db{name = DbName} = _Db) ->
    Scope =
        case couch_httpd:body_length(Req) of
            undefined ->
                list_to_binary(couch_httpd:qs_value(Req, "scope", "global"));
            _ ->
                couch_httpd:json_body(Req)
        end,
    Timeout =
        case couch_httpd:qs_value(Req, "timeout") of
            Value when is_list(Value) ->
                list_to_integer(Value);
            _Any ->
                list_to_integer(couch_config:get("locks", "timeout", "5000"))
        end,
    case couch_locks:create(DbName, Scope, Timeout) of
        {ok, Token} ->
            send_json(Req, 201, {[{ok, true}, {id, Token}]});
        {error, conflict} ->
            send_json(Req, 409, {[{error, conflict}, {<<"reason">>, <<"Already exist">>}]})
    end;

%% /_locks/_db
handle_locks_req(#httpd{path_parts = [DbName, _, <<"_db">> | PathParts]} = Req, _Db) ->
    case couch_httpd:qs_value(Req, "lock") of
        Token when is_list(Token) ->
            TokenBin = list_to_binary(Token),
            case couch_locks:get(TokenBin, self()) of
                true ->
                    NewPath = binary_to_list(normalize_path([DbName, PathParts])),
                    NewQueryList = lists:keydelete("lock", 1, couch_httpd:qs(Req)),
                    Reply = rewrite_path(Req, NewPath, NewQueryList),
                    couch_locks:delete(TokenBin),
                    Reply;
                false ->
                    throw({bad_request, "Lock has expired or does not exist"})
            end;
        _Any ->
            throw({bad_request, "Missing lock"})
    end;

%% DELETE /_locks/Token
handle_locks_req(#httpd{method = 'DELETE', path_parts = [_, _, Token]} = Req, _Db) ->
    couch_locks:delete(Token),
    send_json(Req, 200, {[{ok, true}]});

%% DELETE /_locks
handle_locks_req(#httpd{method = 'DELETE', path_parts = [_, _]} = Req, #db{name = DbName} = _Db) ->
    couch_locks:delete_all(DbName),
    send_json(Req, 200, {[{ok, true}]}).

%% DELETE /_all_locks
handle_all_locks_req(#httpd{method = 'DELETE', path_parts = [_]} = Req) ->
    couch_locks:delete_all(),
    send_json(Req, 200, {[{ok, true}]});

%% PUT /_all_locks/_expire
handle_all_locks_req(#httpd{method = 'PUT', path_parts = [_, <<"_expire">>]} = Req) ->
    couch_locks:expire_all(),
    send_json(Req, 200, {[{ok, true}]}).

%%
%% Internal functions
%%

rewrite_path(#httpd{mochi_req = MochiReq} = Req, Path, QueryList) ->
    Query =
        case QueryList of
            [] ->
                [];
            _ ->
                [$?, mochiweb_util:urlencode(QueryList)]
        end,
    NewRawPath = lists:flatten(Path, lists:flatten(Query)),
    NewMochiReq = mochiweb_request:new(
        MochiReq:get(socket),
        MochiReq:get(method),
        NewRawPath,
        MochiReq:get(version),
        MochiReq:get(headers)
    ),
    NewMochiReq:cleanup(),
    #httpd{
        db_url_handlers = DbUrlHandlers,
        design_url_handlers = DesignUrlHandlers,
        default_fun = DefaultFun,
        url_handlers = UrlHandlers
    } = Req,
    couch_httpd:handle_request_int(
        NewMochiReq,
        DefaultFun,
        UrlHandlers,
        DbUrlHandlers,
        DesignUrlHandlers
    ).

normalize_path(Fragments) ->
    Result = join_path(lists:flatten(Fragments), <<"/">>, []),
    list_to_binary(lists:reverse(Result)).

join_path([Head | []], Sep, Acc) ->
    [Head, Sep | Acc];
join_path([Head | Tail], Sep, Acc) ->
    join_path(Tail, Sep, [Head, Sep | Acc]).

list_to_json([], Acc) ->
    Acc;
list_to_json([I | Tail], Acc) ->
    list_to_json(Tail, [lock_to_json(I) | Acc]).

lock_to_json({{_DbName, Scope}, Token}) ->
    {[{<<"scope">>, Scope}, {<<"id">>, Token}]}.
