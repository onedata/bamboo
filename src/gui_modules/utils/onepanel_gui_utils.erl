%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================
-module(onepanel_gui_utils).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([body/1, body/2, body/3, top_menu/1, top_menu/2, top_menu/3, top_menu/4, account_settings_tab/1,
    logotype_footer/0, nav_buttons/1, nav_buttons/2]).
-export([collapse_button/1, collapse_button/2, expand_button/1, expand_button/2]).
-export([get_session_config/0, format_list/1, message/2, message/3]).
-export([change_page/2, maybe_redirect/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% body/1
%% ====================================================================
%% @doc Template function to render page body, without header and with
%% default page footer.
%% @end
-spec body(Main :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Main) ->
    body([], Main).


%% body/2
%% ====================================================================
%% @doc Template function to render page body, with default page footer.
%% @end
-spec body(Header :: term(), Main :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Main) ->
    body(Header, Main, logotype_footer()).


%% body/3
%% ====================================================================
%% @doc Template function to render page body.
%% @end
-spec body(Header :: term(), Main :: term(), Footer :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Main, Footer) ->
    [
        #header{id = <<"page-header">>, class = <<"page-row">>, body = Header},
        #main{id = <<"page-main">>, class = <<"page-row page-row-expanded">>, body = Main},
        #footer{id = <<"page-footer">>, class = <<"page-row">>, body = Footer}
    ].


%% logotype_footer/0
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer() -> Result when
    Result :: #panel{}.
%% ====================================================================
logotype_footer() ->
    #panel{style = <<"text-align: center; display: flex; justify-content: space-around; padding: 2em; margin-top: 3em;">>,
        body = [
            #image{class = <<"pull-left">>, image = <<"/images/innow-gosp-logo.png">>},
            #image{image = <<"/images/plgrid-plus-logo.png">>},
            #image{class = <<"pull-right">>, image = <<"/images/unia-logo.png">>}
        ]
    }.


%% nav_buttons/1
%% ====================================================================
%% @doc Convienience function to render navigation buttons.
%% @end
-spec nav_buttons(Buttons :: [{Id, Event, Disabled, Body}]) -> Result when
    Id :: binary(),
    Event :: {postback, term()} | {actions, term()} | undefined,
    Disabled :: boolean(),
    Body :: binary(),
    Result :: #panel{}.
%% ====================================================================
nav_buttons(Buttons) ->
    nav_buttons(Buttons, <<"50%">>).


%% nav_buttons/2
%% ====================================================================
%% @doc Convienience function to render navigation buttons with custom
%% width.
%% @end
-spec nav_buttons(Buttons :: [{Id, Event, Disabled, Body}], Width :: binary()) -> Result when
    Id :: binary(),
    Event :: {postback, term()} | {actions, term()} | undefined,
    Disabled :: boolean(),
    Body :: binary(),
    Result :: #panel{}.
%% ====================================================================
nav_buttons(Buttons, Width) ->
    ButtonClass = <<"btn btn-inverse btn-small">>,
    ButtonStyle = <<"min-width: 8em; margin-left: 1em; margin-right: 1em; font-weight: bold;">>,
    #panel{
        style = <<"width: ", Width/binary, "; margin: 0 auto; margin-top: 3em;">>,
        body = lists:map(fun
            ({Id, {postback, Postback}, Disabled, Body}) ->
                #button{
                    id = Id,
                    postback = Postback,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            ({Id, {actions, Actions}, Disabled, Body}) ->
                #button{
                    id = Id,
                    actions = Actions,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            ({Id, _, Disabled, Body}) ->
                #button{
                    id = Id,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            (_) ->
                #button{}
        end, Buttons)
    }.


%% collapse_button/1
%% ====================================================================
%% @doc Renders collapse button.
%% @end
-spec collapse_button(Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
collapse_button(Postback) ->
    collapse_button(<<"Collapse">>, Postback).


%% collapse_button/2
%% ====================================================================
%% @doc Renders collapse button.
%% @end
-spec collapse_button(Title :: binary(), Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
collapse_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large; vertical-align: top;">>,
            class = <<"fui-triangle-up">>
        }
    }.


%% expand_button/1
%% ====================================================================
%% @doc Renders expand button.
%% @end
-spec expand_button(Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
expand_button(Postback) ->
    expand_button(<<"Expand">>, Postback).


%% expand_button/2
%% ====================================================================
%% @doc Renders expand button.
%% @end
-spec expand_button(Title :: binary(), Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
expand_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large;  vertical-align: top;">>,
            class = <<"fui-triangle-down">>
        }
    }.


%% top_menu/1
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% @end
-spec top_menu(ActiveTabID :: atom()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID) ->
    top_menu(ActiveTabID, undefined).


%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% Main page spinner will not be shown.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID) ->
    top_menu(ActiveTabID, ActiveLinkID, []).


%% top_menu/3
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% It allows to add submenu. Main page spinner will not be shown.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom(), Submenu :: term()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID, Submenu) ->
    top_menu(ActiveTabID, ActiveLinkID, Submenu, false).


%% top_menu/4
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% It allows to show or hide main page spinner and add submenu.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom(), Submenu :: term(), Spinner :: boolean()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID, Submenu, Spinner) ->
    Process = fun(ActiveItem, List) ->
        lists:map(fun({ItemID, ListItem}) ->
            case ItemID of
                ActiveItem -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, List)
    end,

    % Define menu items with ids, so that proper tab can be made active via function parameter
    MenuCaptions = Process(ActiveTabID, [
        {brand_tab, #li{body = #link{style = <<"padding: 18px;">>, url = ?PAGE_ROOT,
            body = [
                #span{style = <<"font-size: xx-large;">>, class = <<"fui-gear">>},
                #b{style = <<"font-size: x-large;">>, body = <<"onepanel">>}
            ]}
        }},
        {software_tab, #li{body = [
            #link{style = "padding: 18px;", url = ?PAGE_INSTALLATION, body = <<"Software">>},
            #list{style = "top: 37px; width: 120px;", body = Process(ActiveLinkID, [
                {installation_link, #li{body = #link{url = ?PAGE_INSTALLATION, body = <<"Installation">>}}},
                {update_link, #li{body = #link{url = ?PAGE_UPDATE, body = <<"Update">>}}}
            ])}
        ]}},
        {spaces_tab, #li{body = [
            #link{style = "padding: 18px;", url = ?PAGE_SPACES_ACCOUNT, body = <<"Spaces">>},
            #list{style = "top: 37px; width: 120px;", body = Process(ActiveLinkID, [
                {spaces_account_link, #li{body = #link{url = ?PAGE_SPACES_ACCOUNT, body = <<"Account">>}}},
                {spaces_settings_link, #li{body = #link{url = ?PAGE_SPACES_SETTINGS, body = <<"Settings">>}}}
            ])}
        ]}}
    ]),

    MenuIcons = Process(ActiveTabID, [
        {account_settings_tab, #li{id = <<"account_settings_tab">>, body = account_settings_tab(gui_ctx:get_user_id())}},
        {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
            url = ?PAGE_ABOUT, body = #span{class = <<"fui-info">>}}}},
        {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
            url = ?PAGE_LOGOUT, body = #span{class = <<"fui-power">>}}}}
    ]),

    SpinnerDisplay = case Spinner of
                         true -> <<"">>;
                         _ -> <<" display: none;">>
                     end,

    MessagesTop = case Submenu of
                      [] -> <<"55px">>;
                      _ -> <<"110px">>
                  end,

    [
        #panel{
            id = <<"main_spinner">>,
            style = <<"position: absolute; top: 15px; left: 15px; z-index: 1234; width: 32px;", SpinnerDisplay/binary>>,
            body = #image{
                image = <<"/images/spinner.gif">>
            }
        },
        #panel{class = <<"navbar navbar-fixed-top">>, body = [
            #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 2px solid gray;">>, body = [
                #panel{class = <<"container">>, body = [
                    #list{class = <<"nav pull-left">>, body = MenuCaptions},
                    #list{class = <<"nav pull-right">>, body = MenuIcons}
                ]}
            ]}
        ] ++ Submenu},
        #panel{
            id = <<"ok_message">>,
            style = <<"position: fixed; width: 100%; top: ", MessagesTop/binary, "; display: none;">>,
            class = <<"dialog dialog-success">>
        },
        #panel{
            id = <<"error_message">>,
            style = <<"position: fixed; width: 100%; top: ", MessagesTop/binary, "; display: none;">>,
            class = <<"dialog dialog-danger">>
        }
    ] ++ gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY).


%% account_settings_tab/1
%% ====================================================================
%% @doc Renders body of account settings tab.
%% @end
-spec account_settings_tab(Username :: binary()) -> Result when
    Result :: #link{}.
%% ====================================================================
account_settings_tab(Username) ->
    #link{
        style = <<"padding: 18px;">>,
        title = <<"Account settings">>,
        url = ?PAGE_ACCOUNT_SETTINGS,
        body = [
            Username,
            #span{
                class = <<"fui-user">>,
                style = <<"margin-left: 10px;">>
            }
        ]
    }.


%% get_session_config/0
%% ====================================================================
%% @doc Returns current installation state read in first place from session
%% and in second place from database.
%% @end
-spec get_session_config() -> Result when
    Result :: #?GLOBAL_CONFIG_RECORD{} | undefined.
%% ====================================================================
get_session_config() ->
    case gui_ctx:get(?CONFIG_ID) of
        undefined ->
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, Record} -> {ok, Record};
                _ -> undefined
            end;
        Record -> {ok, Record}
    end.


%% change_page/2
%% ====================================================================
%% @doc Redirects to given page and saves it in user session.
%% @end
-spec change_page(Env :: atom(), Page :: string()) -> Result when
    Result :: ok.
%% ====================================================================
change_page(Env, Page) ->
    gui_ctx:put(Env, Page),
    gui_jq:redirect(Page).


%% maybe_redirect/3
%% ====================================================================
%% @doc Redirects to appropriate page read from user session.
%% @end
-spec maybe_redirect(Env :: atom(), Page :: string(), DefaultPage :: string()) -> Result when
    Result :: true | false.
%% ====================================================================
maybe_redirect(Env, CurrentPage, DefaultPage) ->
    case gui_ctx:get(Env) of
        CurrentPage ->
            false;
        undefined ->
            gui_jq:redirect(DefaultPage),
            true;
        Page ->
            gui_jq:redirect(Page),
            true
    end.


%% format_list/1
%% ====================================================================
%% @doc Returns list elements as a comma-delimited binary.
%% @end
-spec format_list(List :: [string()]) -> Result when
    Result :: binary().
%% ====================================================================
format_list([]) ->
    <<"">>;
format_list(Hosts) ->
    list_to_binary(string:join(Hosts, ", ")).


%% message/2
%% ====================================================================
%% @doc Renders a message in given element and allows to hide it with
%% default postback.
%% @end
-spec message(Id :: binary(), Message :: binary()) -> Result when
    Result :: ok.
%% ====================================================================
message(Id, Message) ->
    message(Id, Message, {close_message, Id}).


%% message/3
%% ====================================================================
%% @doc Renders a message in given element and allows to hide it with
%% custom postback.
%% @end
-spec message(Id :: binary(), Message :: binary(), Postback :: term()) -> Result when
    Result :: ok.
%% ====================================================================
message(Id, Message, Postback) ->
    Body = [
        Message,
        #link{
            title = <<"Close">>,
            style = <<"position: absolute; top: 1em; right: 1em;">>,
            class = <<"glyph-link">>,
            postback = Postback,
            body = #span{
                class = <<"fui-cross">>
            }
        }
    ],
    gui_jq:update(Id, Body),
    gui_jq:fade_in(Id, 300).
