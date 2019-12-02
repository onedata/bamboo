#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, Endpoint, CredentialsType,
      Credentials, VerifyServerCertificate, AuthorizationHeader,
      RangeWriteSupport, ConnectionPoolSize, MaximumUploadSize,
      Insecure, StoragePathType]) ->

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
            <<"maximumUploadSize">> => list_to_binary(MaximumUploadSize)
        },
        UserCtx,
        list_to_atom(Insecure),
        list_to_binary(StoragePathType)
    ]),

    StorageId = safe_call(NodeAtom, datastore_utils, gen_key, []),
    StorageConfig = safe_call(NodeAtom, storage_config, create, [StorageId, list_to_binary(Name), Helper, false, undefined, false]),
    safe_call(NodeAtom, storage, on_storage_created, [StorageId]).


safe_call(Node, Module, Function, Args) ->
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
