#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, Volume, Hostname, Port, Transport, MountPoint, XlatorOptions,
    Insecure, StoragePathType, LumaUrl]) ->

    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = #{
        <<"uid">> => <<"0">>,
        <<"gid">> => <<"0">>
    },
    {ok, Helper} = safe_call(NodeAtom, helper, new_helper, [
        <<"glusterfs">>,
        #{
            <<"volume">> => list_to_binary(Volume),
            <<"hostname">> => list_to_binary(Hostname),
            <<"port">> => list_to_binary(Port),
            <<"transport">> => list_to_binary(Transport),
            <<"mountPoint">> => list_to_binary(MountPoint),
            <<"xlatorOptions">> => list_to_binary(XlatorOptions)
        },
        UserCtx,
        list_to_atom(Insecure),
        list_to_binary(StoragePathType)
    ]),

    % use storage name as its id
    StorageId = list_to_binary(Name),
    LumaConfig = maybe_setup_luma(NodeAtom, LumaUrl),
    {ok, StorageId} = safe_call(NodeAtom, storage_config, create, [StorageId, Helper, false, LumaConfig, false]),
    safe_call(NodeAtom, storage, on_storage_created, [StorageId]).


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

maybe_setup_luma(_NodeAtom, "None") -> undefined;
maybe_setup_luma(NodeAtom, LumaUrl) when is_list(LumaUrl) ->
    safe_call(NodeAtom, luma_config, new, [list_to_binary(LumaUrl), undefined]).