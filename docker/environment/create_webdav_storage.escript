#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, Endpoint, CredentialsType,
      Credentials, VerifyServerCertificate, AuthorizationHeader,
      RangeWriteSupport, ConnectionPoolSize, MaximumUploadSize, StoragePathType]) ->

    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = #{
        <<"credentialsType">> => list_to_binary(CredentialsType),
        <<"credentials">> => list_to_binary(Credentials)
    },
    {ok, Helper} = safe_call(NodeAtom, helper, new_helper, [
        <<"webdav">>,
        #{
            <<"endpoint">> => list_to_binary(Endpoint),
            <<"verifyServerCertificate">> => list_to_binary(VerifyServerCertificate),
            <<"authorizationHeader">> => list_to_binary(AuthorizationHeader),
            <<"rangeWriteSupport">> => list_to_binary(RangeWriteSupport),
            <<"connectionPoolSize">> => list_to_binary(ConnectionPoolSize),
            <<"maximumUploadSize">> => list_to_binary(MaximumUploadSize),
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
            io:format(standard_error,
                "ERROR: in module ~p:~n {badrpc, ~p} in rpc:call(~p, ~p, ~p, ~p).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        {error, X} ->
            io:format(standard_error,
                "ERROR: in module ~p:~n {error, ~p} in rpc:call(~p, ~p, ~p, ~p).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        X ->
            X
    end.
