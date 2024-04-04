#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, Hostname, Scheme, BucketName, AccessKey, SecretKey,
    BlockSize, StoragePathType]) ->

    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = #{
        <<"accessKey">> => list_to_binary(AccessKey),
        <<"secretKey">> => list_to_binary(SecretKey)
    },
    {ok, Helper} = safe_call(NodeAtom, helper, new_helper, [
        <<"s3">>,
        #{
            <<"hostname">> => list_to_binary(Hostname),
            <<"bucketName">> => list_to_binary(BucketName),
            <<"scheme">> => case Scheme of
                "https" -> <<"https">>;
                _ -> <<"http">>
            end,
            <<"blockSize">> => list_to_binary(BlockSize),
            <<"skipStorageDetection">> => <<"false">>,
            <<"storagePathType">> => list_to_binary(StoragePathType)
        },
        UserCtx
    ]),

    % use storage name as its id
    StorageId = safe_call(NodeAtom, initializer, normalize_storage_name, [list_to_binary(Name)]),
    {ok, StorageId} = safe_call(NodeAtom, storage_config, create, [StorageId, Helper, undefined]),
    safe_call(NodeAtom, storage, on_storage_created, [StorageId]).


safe_call(Node, Module, Function, Args) ->
    true = net_kernel:hidden_connect_node(Node),
    case rpc:call(Node, Module, Function, Args) of
        {badrpc, X} ->
            io:format(standard_error, "ERROR: in module ~tp:~n {badrpc, ~tp} in rpc:call(~tp, ~tp, ~tp, ~tp).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        {error, X} ->
            io:format(standard_error, "ERROR: in module ~tp:~n {error, ~tp} in rpc:call(~tp, ~tp, ~tp, ~tp).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        X ->
            X
    end.
