#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, Volume, Hostname, Port, Transport, MountPoint, XlatorOptions, StoragePathType]) ->

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
            <<"xlatorOptions">> => list_to_binary(XlatorOptions),
            <<"skipStorageDetection">> => <<"false">>,
            <<"storagePathType">> => list_to_binary(StoragePathType)
        },
        UserCtx
    ]),

    % use storage name as its id
    StorageId = safe_call(NodeAtom, initializer, normalize, [list_to_binary(Name)]),
    {ok, StorageId} = safe_call(NodeAtom, storage_config, create, [StorageId, Helper, undefined]),
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