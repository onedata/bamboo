%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for Global Registry node installation.
%% @end
%% ===================================================================
-module(installer_globalregistry).
-behaviour(installer_behaviour).

-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_start/1, local_stop/0, local_restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc This callback is unsupported. Returns always ok.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok.
%% ====================================================================
install(_Args) ->
    ok.


%% uninstall/1
%% ====================================================================
%% @doc This callback is unsupported. Returns always ok.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok.
%% ====================================================================
uninstall(_Args) ->
    ok.


%% start/1
%% ====================================================================
%% @doc Starts Global Registry node on given hosts. Arguments list should contain
%% host where Global Registry node was installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        GR = case proplists:get_value(gr, Args) of
                 undefined -> throw(nothing_to_start);
                 Host -> Host
             end,

        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} -> throw("Database nodes not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined, dbs = Dbs}} -> Dbs;
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = _}} -> throw("Global Registry node already configured.");
                            _ -> throw("Cannot get Global Registry node configuration.")
                        end,

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts([GR], ?MODULE, local_start, [ConfiguredDbs], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{gr, GR}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update Global Registry node configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts([GR], ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                        {error, {hosts, [GR]}}
                end;
            _ ->
                ?error("Cannot start Global Registry node on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error("Cannot start Global Registry node: ~p", [Reason]),
            {error, Reason}
    end.


%% stop/1
%% ====================================================================
%% @doc Stops Global Registry node.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(_) ->
    try
        {ConfiguredGR, ConfiguredDbs} = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                                            {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined}} ->
                                                throw("Global Registry node not configured.");
                                            {ok, #?GLOBAL_CONFIG_RECORD{gr = GR, dbs = Dbs}} -> {GR, Dbs};
                                            _ -> throw("Cannot get Global Registry node configuration.")
                                        end,

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts([ConfiguredGR], ?MODULE, local_stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{gr, undefined}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update Global Registry node configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts([ConfiguredGR], ?MODULE, local_start, [ConfiguredDbs], ?RPC_TIMEOUT),
                        {error, {hosts, [ConfiguredGR]}}
                end;
            _ ->
                ?error("Cannot stop Global Registry node on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start, [ConfiguredDbs], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop Global Registry node: ~p", [Reason]),
            {error, Reason}
    end.


%% restart/1
%% ====================================================================
%% @doc Restarts Global Registry node.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(_) ->
    try
        ConfiguredGR = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                           {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined}} ->
                               throw("Global Registry node not configured.");
                           {ok, #?GLOBAL_CONFIG_RECORD{gr = GR}} -> GR;
                           _ -> throw("Cannot get Global Registry node configuration.")
                       end,

        case stop([]) of
            ok -> start([{gr, ConfiguredGR}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart Global Registry node: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% API functions
%% ====================================================================

%% local_start/1
%% ====================================================================
%% @doc Starts Global Registry node on local host.
%% @end
-spec local_start(Dbs :: [string()]) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start([FirstDb | Dbs]) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting Global Registry node: ~p"),

        Name = <<(list_to_binary(?DEFAULT_GLOBALREGISTRY_NAME))/binary, "@", (list_to_binary(Host))/binary>>,

        DbNames = lists:foldl(fun(Db, Acc) ->
            <<"'", (list_to_binary(?DEFAULT_DB_NAME))/binary, "@", (list_to_binary(Db))/binary, "', ", Acc/binary>>
        end, <<"'", (list_to_binary(FirstDb))/binary, "'">>, Dbs),

        ok = installer_utils:overwrite_config_args(?APP_CONFIG_PATH, <<"db_nodes, ">>, <<"[^\]]*">>, <<"[", DbNames/binary>>),
        ok = installer_utils:overwrite_config_args(?APP_CONFIG_PATH, <<"rest_cert_domain, \"">>, <<"[^\"]*">>, <<"onedata.org">>),
        ok = installer_utils:overwrite_config_args(?VM_CONFIG_PATH, <<"-name ">>, <<"[^\n]*">>, Name),
        ok = installer_utils:add_node_to_config(gr_node, list_to_atom(?DEFAULT_GLOBALREGISTRY_NAME), ?DEFAULT_NODES_INSTALL_PATH),

        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        ServiceAnswer = os:cmd(SetUlimitsCmd ++ " ; service globalregistry start"),
        {match, ["OK"]} = re:run(ServiceAnswer, "OK", [{capture, first, list}]),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot start Global Registry node: ~p", [Reason]),
            {error, Host}
    end.


%% local_stop/0
%% ====================================================================
%% @doc Stops Global Registry node on local host.
%% @end
-spec local_stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping Global Registry node"),

        "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?APP_CONFIG_PATH ++ " | awk '{print $2}'`"),
        ok = installer_utils:remove_node_from_config(gr_node),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop Global Registry node: ~p", [Reason]),
            {error, Host}
    end.


%% local_restart/0
%% ====================================================================
%% @doc Restarts Global Registry node on local host.
%% @end
-spec local_restart() -> Result when
    Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% ====================================================================
local_restart() ->
    Host = onepanel_utils:get_host(node()),
    try
        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined}} ->
                                throw("Global Registry node not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = _, dbs = Dbs}} -> Dbs;
                            _ -> throw("Cannot get Global Registry node configuration.")
                        end,

        case local_stop() of
            {ok, _} -> local_start(ConfiguredDbs);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart Global Registry node: ~p", [Reason]),
            {error, Host}
    end.