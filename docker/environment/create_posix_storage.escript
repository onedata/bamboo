#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, MountPoint, StoragePathType, ReadonlyStr]) ->
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = #{
        <<"uid">> => <<"0">>,
        <<"gid">> => <<"0">>
    },
    {ok, Helper} = safe_call(NodeAtom, helper, new_helper, [
        <<"posix">>,
        #{<<"mountPoint">> => list_to_binary(MountPoint)},
        UserCtx,
        false,
        list_to_binary(StoragePathType)
    ]),

    StorageConfig = safe_call(NodeAtom, storage_config, new, [list_to_binary(Name), [Helper],
        list_to_atom(string:lowercase(ReadonlyStr)), undefined]),
    {ok, StorageId} = safe_call(NodeAtom, storage_config, save_doc, [StorageConfig]),
    safe_call(NodeAtom, storage_config, on_storage_created, [StorageId]).

safe_call(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args) of
        {badrpc, X} ->
            io:format(standard_error, "ERROR: in module ~p:~n {badrpc, ~p} in rpc:call(~p, ~p, ~p, ~p).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        {error, X} ->
            io:format(standard_error, "ERROR: in module ~p:~n {error, ~p} in rpc:call(~p, ~p, ~p, ~p).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        X ->
            X
    end.
