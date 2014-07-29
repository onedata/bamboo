%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page displays update summary and starts updater process.
%% @end
%% ===================================================================

-module(page_update_summary).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/updater/state.hrl").
-include("onepanel_modules/updater/stages.hrl").
-include_lib("ctool/include/logging.hrl").

%% Default time in miliseconds for next progress bar update
-define(DEFAULT_NEXT_UPDATE, 1000).

%% Current 'force_restart_checkbox' state
-define(FORCE_RELOAD, force_reload).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, state).
-record(?STATE, {stage_index = 1, job_index, job_progress, stages_count, action_type = install, update_time}).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case onepanel_gui_utils:maybe_redirect(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUMMARY, ?PAGE_UPDATE) of
                true ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
            end;
        false ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Update summary">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #version{major = Major, minor = Minor, patch = Patch} = Version = gui_ctx:get(?CHOSEN_VERSION),
    ChosenVersionName = <<(integer_to_binary(Major))/binary, ".", (integer_to_binary(Minor))/binary, ".", (integer_to_binary(Patch))/binary>>,
    State = updater:get_state(),
    {UpdatePanelDisplay, UpdateProgressDisplay} = case updater_state:get_stage_and_job(State) of
                                                      {?STAGE_IDLE, _} -> {<<"">>, <<" display: none;">>};
                                                      _ -> {<<" display: none;">>, <<"">>}
                                                  end,
    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(update_tab),

            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                id = <<"ok_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-success">>
            },
            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = [
                    #h6{
                        style = <<"font-size: 18px;">>,
                        body = <<"Step 2: Update summary.">>
                    },
                    #h6{
                        style = <<"font-size: 16px; margin-top: 30px; margin-bottom: 30px;">>,
                        body = <<"Software version to update to: <b>", ChosenVersionName/binary, "</b>">>
                    },
                    #panel{
                        id = <<"update_panel">>,
                        style = UpdatePanelDisplay,
                        body = [
                            #panel{
                                style = <<"margin-top: 30px; width: 270px; height: 100px; margin: 0 auto;">>,
                                body = [
                                    #button{
                                        id = <<"update_button">>,
                                        class = <<"btn btn-primary">>,
                                        postback = {update, Version},
                                        style = <<"float: left; width: 80px;">>,
                                        body = <<"Update">>
                                    },
                                    #panel{
                                        class = <<"btn-group pull-right">>,
                                        style = <<"float: right; padding: 7px 14px 0px;">>,
                                        body = [
                                            #label{
                                                id = <<"force_restart_checkbox">>,
                                                class = <<"checkbox">>,
                                                for = <<"force_restart_checkbox">>,
                                                style = <<"display: block; font-weight: bold;">>,
                                                actions = gui_jq:postback_action(<<"force_restart_checkbox">>, force_restart_checkbox_toggled),
                                                body = [
                                                    #span{
                                                        class = <<"icons">>
                                                    },
                                                    #checkbox{
                                                        id = <<"force_restart_checkbox">>,
                                                        data_fields = [{<<"data-toggle">>, <<"checkbox">>}]
                                                    },
                                                    <<"Force nodes restart">>
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            },
                            #button{
                                id = <<"back_button">>,
                                class = <<"btn btn-inverse">>,
                                postback = back,
                                style = <<"width: 80px; margin-top: 30px;">>,
                                body = <<"Back">>
                            }
                        ]
                    },
                    #panel{
                        id = <<"update_progress">>,
                        style = UpdateProgressDisplay,
                        body = [
                            #panel{
                                id = <<"stage_progress">>,
                                style = <<"text-align: left; margin-top: 30px; width: 50%; margin: 0 auto;">>,
                                body = [
                                    #p{
                                        id = <<"stage_progress_text">>,
                                        style = <<"font-weight: 300;">>,
                                        body = <<"">>
                                    },
                                    #panel{
                                        class = <<"progress">>,
                                        body = #panel{
                                            id = <<"stage_bar">>,
                                            class = <<"bar">>,
                                            style = <<"width: 0%; background-color: darkcyan;">>
                                        }
                                    }
                                ]
                            },
                            #panel{
                                id = <<"job_progress">>,
                                style = <<"text-align: left; margin-top: 30px; width: 50%; margin: 0 auto;">>,
                                body = [
                                    #p{
                                        id = <<"job_progress_text">>,
                                        style = <<"font-weight: 300;">>,
                                        body = <<"">>
                                    },
                                    #panel{
                                        class = <<"progress">>,
                                        body = #panel{
                                            id = <<"job_bar">>,
                                            class = <<"bar">>,
                                            style = <<"width: 0%;">>
                                        }
                                    }
                                ]
                            },
                            #button{
                                id = <<"abort_button">>,
                                class = <<"btn btn-danger">>,
                                postback = abort,
                                style = <<"width: 80px; margin-top: 30px;">>,
                                body = <<"Abort">>
                            }
                        ]
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% translate_stage/1
%% ====================================================================
%% @doc Translates stage ID to human-readable version
-spec translate_stage(StageId :: atom()) -> Result when
    Result :: binary().
%% ====================================================================
translate_stage(?STAGE_IDLE) -> <<"Idle">>;
translate_stage(?STAGE_INIT) -> <<"Initializing">>;
translate_stage(?STAGE_DAO_UPDATER_LOAD) -> <<"Loading database updater">>;
translate_stage(?STAGE_DAO_SETUP_VIEWS) -> <<"Setting up database views">>;
translate_stage(?STAGE_DAO_REFRESH_VIEWS) -> <<"Refreshing database views">>;
translate_stage(?STAGE_DEPLOY_FILES) -> <<"Deploying files">>;
translate_stage(?STAGE_SOFT_RELOAD) -> <<"Applying soft reload on remote nodes">>;
translate_stage(?STAGE_HARD_RELOAD) -> <<"Applying hard reload on remote nodes">>;
translate_stage(?STAGE_FORCE_RELOAD) -> <<"Applying force reload on remote nodes">>;
translate_stage(?STAGE_NODE_RESTART) -> <<"Restarting remote nodes">>;
translate_stage(?STAGE_ROLLBACK) -> <<"Rollbacking">>;
translate_stage(?STAGE_DAO_POST_SETUP_VIEWS) -> <<"Applying post-update database views setup">>;
translate_stage(?STAGE_REPAIR_NODES) -> <<"Repairing remote nodes">>;
translate_stage(_) -> <<"">>.


%% translate_job/1
%% ====================================================================
%% @doc Translates job ID to human-readable version
-spec translate_job(JobId :: atom()) -> Result when
    Result :: binary().
%% ====================================================================
translate_job(?JOB_DOWNLOAD_BINARY) -> <<"Downloading package">>;
translate_job(?JOB_LOAD_EXPORTS) -> <<"Loading updater modules to remote nodes">>;
translate_job(?JOB_RELOAD_EXPORTS) -> <<"Reloading updater modules">>;
translate_job(?JOB_INSTALL_PACKAGE) -> <<"Installing package">>;
translate_job(?JOB_DEFAULT) -> <<"Default">>;
translate_job(?JOB_MOVE_BEAMS) -> <<"Moving binary files">>;
translate_job(?JOB_LOAD_BEAMS) -> <<"Loading binary files">>;
translate_job(?JOB_PRE_UPDATE) -> <<"Performing pre-update tasks">>;
translate_job(?JOB_INSTALL_VIEW_SOURCES) -> <<"Preparing database views sources">>;
translate_job(?JOB_INSTALL_VIEWS) -> <<"Installing database views sources">>;
translate_job(?JOB_BACKUP) -> <<"Backing up files">>;
translate_job(?JOB_DEPLOY) -> <<"Deploying files">>;
translate_job(?JOB_CLEANUP_VIEWS) -> <<"Cleaning up database views">>;
translate_job(?JOB_CHECK_CONNECTIVITY) -> <<"Checking connection to remote nodes">>;
translate_job(_) -> <<"">>.


%% get_stage_index/2
%% ====================================================================
%% @doc Returns index of current update stage.
-spec get_stage_index(Job :: atom(), State :: #?u_state{}) -> Result when
    Result :: integer().
%% ====================================================================
get_stage_index(Stage, State) ->
    Stages = updater_state:get_all_stages(State),
    length(Stages) - length(lists:dropwhile(fun({S, _}) -> S =/= Stage end, Stages)) + 1.


%% get_job_index_and_jobs_count/3
%% ====================================================================
%% @doc Returns index of current update job.
-spec get_job_index_and_jobs_count(Stage :: atom(), Job :: atom(), State :: #?u_state{}) -> Result when
    Result :: {JobIndex :: integer(), JobsCount :: integer()}.
%% ====================================================================
get_job_index_and_jobs_count(Stage, Job, State) ->
    Stages = updater_state:get_all_stages(State),
    case lists:dropwhile(fun({S, _}) -> S =/= Stage end, Stages) of
        [{_, Jobs} | _] ->
            JobsCount = length(Jobs),
            {JobsCount - length(lists:dropwhile(fun(J) -> J =/= Job end, Jobs)) + 1, JobsCount};
        _ ->
            {1, 1}
    end.


%% get_job_progress/4
%% ====================================================================
%% @doc Returns overall jobs progress in current stage and also progress
%% of current job.
-spec get_job_progress(JobProgress :: float(), JobIndex :: integer(), JobsCount :: integer(), ActionType :: atom()) -> Result
    when Result :: {JobsProgress :: float(), NewJobProgress :: float()}.
%% ====================================================================
get_job_progress(JobProgress, JobIndex, JobsCount, install) ->
    NewProgress = (JobProgress + 1) / 2,
    {100 * (JobIndex + JobProgress - 1) / JobsCount, NewProgress};
get_job_progress(JobProgress, JobIndex, JobsCount, _) ->
    NewProgress = (JobProgress + 1) / 2,
    {100 * (JobIndex - JobProgress) / JobsCount, NewProgress}.


%% update_progress/3
%% ====================================================================
%% @doc Updater callback.
-spec update_progress(Pid :: pid(), Event :: atom(), State :: atom()) -> no_return().
%% ====================================================================
update_progress(Pid, Event, State) ->
    {Stage, Job} = updater_state:get_stage_and_job(State),

    StageIndex = get_stage_index(Stage, State),
    {JobIndex, JobsCount} = get_job_index_and_jobs_count(Stage, Job, State),

    case Event of
        error -> Pid ! error;
        abort -> Pid ! abort;
        rollback_stage ->
            ActionType = updater_state:get_action_type(State),
            Pid ! {set_action_type, ActionType};
        _ -> ok
    end,

    Pid ! {set_abortable, updater_state:is_abortable(State)},

    case Stage of
        ?STAGE_IDLE ->
            Pid ! {finish, State};
        _ ->
            StageName = translate_stage(Stage),
            JobName = case Stage of
                          ?STAGE_NODE_RESTART ->
                              <<"Restarting node ", (atom_to_binary(Job, latin1))/binary>>;
                          _ -> translate_job(Job)
                      end,
            Pid ! {set_stage_and_job, StageIndex, StageName, JobIndex, JobsCount, JobName}
    end.


%% comet_loop/1
%% ====================================================================
%% @doc Handles updater process messages and updates progress bar.
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{stage_index = SIndex, job_index = JIndex, job_progress = JProgress, stages_count = SCount, action_type = AType, update_time = UTime} = State) ->
    NewState = try
        receive
            {set_action_type, NewAType} ->
                State#?STATE{action_type = NewAType};

            {set_stages_count, NewSCount} ->
                State#?STATE{stages_count = NewSCount};

            {set_abortable, true} ->
                gui_jq:prop(<<"abort_button">>, <<"disabled">>, <<"">>),
                gui_comet:flush(),
                State;

            {set_abortable, false} ->
                gui_jq:prop(<<"abort_button">>, <<"disabled">>, <<"disabled">>),
                gui_comet:flush(),
                State;

            {set_stage_and_job, SIndex, _, JIndex, _, _} ->
                State;

            {set_stage_and_job, StageIndex, StageName, JobIndex, JobsCount, JobName} ->
                gui_jq:hide(<<"ok_message">>),
                case StageIndex of
                    SIndex ->
                        gui_jq:css(<<"job_bar">>, <<"transition-property">>, <<"">>),
                        gui_jq:css(<<"job_bar">>, <<"-moz-transition-property">>, <<"">>),
                        gui_jq:css(<<"job_bar">>, <<"-webkit-transition-property">>, <<"">>),
                        gui_jq:css(<<"job_bar">>, <<"-o-transition-property">>, <<"">>);
                    _ ->
                        gui_jq:css(<<"job_bar">>, <<"transition-property">>, <<"none">>),
                        gui_jq:css(<<"job_bar">>, <<"-moz-transition-property">>, <<"none">>),
                        gui_jq:css(<<"job_bar">>, <<"-webkit-transition-property">>, <<"none">>),
                        gui_jq:css(<<"job_bar">>, <<"-o-transition-property">>, <<"none">>)
                end,
                gui_comet:flush(),

                {StagePrefix, JobPrefix} = case AType of
                                               install -> {<<"Current stage: ">>, <<"Current job: ">>};
                                               _ -> {<<"Stage rollback: ">>, <<"Job rollback: ">>}
                                           end,

                StageProgress = 100 * (StageIndex * JobsCount - JobsCount + JobIndex) / (SCount * JobsCount),
                StageProgressBinary = <<(integer_to_binary(round(StageProgress)))/binary, "%">>,
                gui_jq:update(<<"stage_progress_text">>, <<StagePrefix/binary, "<b>", StageName/binary, " ( ", StageProgressBinary/binary, " )</b>">>),
                gui_jq:set_width(<<"stage_bar">>, StageProgressBinary),

                {JobsProgress, NewJProgress} = get_job_progress(0, JobIndex, JobsCount, AType),
                JobsProgressBinary = <<(integer_to_binary(round(JobsProgress)))/binary, "%">>,
                gui_jq:update(<<"job_progress_text">>, <<JobPrefix/binary, "<b>", JobName/binary, " ( ", JobsProgressBinary/binary, " )</b>">>),
                gui_jq:set_width(<<"job_bar">>, JobsProgressBinary),

                timer:send_after(?DEFAULT_NEXT_UPDATE, {update, StageIndex, JobIndex, JobPrefix, JobName, JobsCount}),
                gui_comet:flush(),
                State#?STATE{stage_index = StageIndex, job_index = JobIndex, job_progress = NewJProgress, update_time = ?DEFAULT_NEXT_UPDATE};

            {update, SIndex, JIndex, JobPrefix, JobName, JobsCount} ->
                gui_jq:css(<<"job_bar">>, <<"transition-property">>, <<"">>),
                gui_jq:css(<<"job_bar">>, <<"-moz-transition-property">>, <<"">>),
                gui_jq:css(<<"job_bar">>, <<"-webkit-transition-property">>, <<"">>),
                gui_jq:css(<<"job_bar">>, <<"-o-transition-property">>, <<"">>),

                {JobsProgress, NewJProgress} = get_job_progress(JProgress, JIndex, JobsCount, AType),
                JobsProgressBinary = <<(integer_to_binary(round(JobsProgress)))/binary, "%">>,
                gui_jq:update(<<"job_progress_text">>, <<JobPrefix/binary, "<b>", JobName/binary, " ( ", JobsProgressBinary/binary, " )</b>">>),
                gui_jq:set_width(<<"job_bar">>, JobsProgressBinary),

                timer:send_after(2 * UTime, {update, SIndex, JIndex, JobPrefix, JobName, JobsCount}),
                gui_comet:flush(),
                State#?STATE{job_progress = NewJProgress, update_time = 2 * UTime};

            {update, _, _, _, _, _} ->
                State;

            abort ->
                gui_jq:prop(<<"abort_button">>, <<"disabled">>, <<"disabled">>),
                onepanel_gui_utils:message(<<"error_message">>, <<"Aborting update process.<br>Please wait while rollbacking changes...">>),
                gui_comet:flush(),
                State;

            error ->
                onepanel_gui_utils:message(<<"error_message">>, <<"An error occurred during update process.<br>Rollbacking changes.">>),
                gui_comet:flush(),
                State;

            {finish, UpdaterState} ->
                gui_jq:update(<<"job_progress_text">>, <<"">>),
                gui_jq:update(<<"stage_progress_text">>, <<"">>),
                case AType of
                    install ->
                        gui_jq:set_width(<<"job_bar">>, <<"100%">>),
                        gui_jq:set_width(<<"stage_bar">>, <<"100%">>),
                        gui_comet:flush(),
                        onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUCCESS);
                    _ ->
                        gui_jq:set_width(<<"job_bar">>, <<"0%">>),
                        gui_jq:set_width(<<"stage_bar">>, <<"0%">>),
                        gui_jq:hide(<<"update_progress">>),
                        gui_jq:show(<<"update_panel">>),
                        case updater_state:get_error_stack(UpdaterState) of
                            {[], _} ->
                                gui_jq:hide(<<"error_message">>),
                                onepanel_gui_utils:message(<<"ok_message">>, <<"Update process aborted successfully.">>);
                            _ ->
                                onepanel_gui_utils:message(<<"error_message">>, <<"An error occurred during update process.">>)
                        end
                end,
                gui_comet:flush(),
                #?STATE{stages_count = SCount, action_type = install}
        end
               catch Type:Reason ->
                   ?error("Comet process exception: ~p:~p", [Type, Reason]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Reason}
               end,
    comet_loop(NewState).


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"update_button">>),
    gui_jq:hide(<<"ok_message">>),
    gui_jq:hide(<<"error_message">>),

    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    updater:set_callback(fun(Event, State) -> update_progress(Pid, Event, State) end),
    put(?COMET_PID, Pid),

    State = updater:get_state(),
    ActionType = updater_state:get_action_type(State),
    Pid ! {set_stages_count, length(updater_state:get_all_stages(State))},

    case updater_state:get_stage_and_job(State) of
        {?STAGE_IDLE, _} ->
            Pid ! {set_action_type, install},
            case ActionType of
                install -> ok;
                _ ->
                    case updater_state:get_error_stack(State) of
                        {[], _} ->
                            onepanel_gui_utils:message(<<"ok_message">>, <<"Previous update process aborted successfully.">>);
                        _ ->
                            onepanel_gui_utils:message(<<"error_message">>, <<"An error occurred during previous update process.">>)
                    end
            end;
        _ ->
            onepanel_gui_utils:message(<<"ok_message">>, <<"Getting update process state. Please wait.">>),
            gui_jq:hide(<<"update_panel">>),
            gui_jq:show(<<"update_progress">>)
    end,

    put(?FORCE_RELOAD, false),
    ok;

event(force_restart_checkbox_toggled) ->
    ForceReload = get(?FORCE_RELOAD),
    put(?FORCE_RELOAD, not ForceReload);

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_VERSION_SELECTION);

event(abort) ->
    updater:abort();

event({update, Version}) ->
    gui_jq:hide(<<"update_panel">>),
    gui_jq:hide(<<"error_message">>),
    gui_jq:hide(<<"ok_message">>),
    gui_jq:show(<<"update_progress">>),
    ForceReload = get(?FORCE_RELOAD),
    Pid = get(?COMET_PID),
    updater:update_to(Version, ForceReload, fun(Event, State) -> update_progress(Pid, Event, State) end);

event(terminate) ->
    ok.