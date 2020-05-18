#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, LatencyMin, LatencyMax, TimeoutProbability, Filter,
    SimulatedFilesystemParameters, SimulatedFilesystemGrowSpeed, StoragePathType]) ->

    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = #{
        <<"uid">> => <<"0">>,
        <<"gid">> => <<"0">>
    },
    {ok, Helper} = safe_call(NodeAtom, helper, new_helper, [
        <<"nulldevice">>,
        #{
            <<"latencyMin">> => list_to_binary(LatencyMin),
            <<"latencyMax">> => list_to_binary(LatencyMax),
            <<"timeoutProbability">> => list_to_binary(TimeoutProbability),
            <<"filter">> => list_to_binary(Filter),
            <<"simulatedFilesystemParameters">> =>
                list_to_binary(SimulatedFilesystemParameters),
            <<"simulatedFilesystemGrowSpeed">> =>
                list_to_binary(SimulatedFilesystemGrowSpeed),
            <<"skipStorageDetection">> => <<"false">>,
            <<"storagePathType">> => list_to_binary(StoragePathType)
        },
        UserCtx
    ]),

    % use storage name as its id
    StorageId = list_to_binary(Name),
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