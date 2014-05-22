%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file handles rendering jquery actions to javascript.
%% @end
%% ===================================================================

-module(action_jquery).
-include_lib("common.hrl").
-export([render_action/1]).

render_action(Record = #jquery{property = undefined, target = Target, method = Methods, args = Args1}) ->
  Args2 = case Record#jquery.format of "'~s'" -> [wf:render(Args1)]; _ -> Args1 end,
  PreRenderedArgs = string:join([case A of
                                   A when is_tuple(A) -> wf:render(A);
                                   A when is_list(A) -> A;
                                   A when is_integer(A) -> gui_utils:to_list(A);
                                   A -> A end || A <- Args2], ","),
  RenderedArgs = case Record#jquery.format of
                   "'~s'" -> wf:js_escape(PreRenderedArgs);
                   _ -> PreRenderedArgs
                 end,
  string:join([wf:f("$('#~s').~s(" ++ Record#jquery.format ++ ");",
    [gui_utils:to_list(Target), gui_utils:to_list(Method), RenderedArgs]) || Method <- Methods], []);

render_action(#jquery{target = Target, method = undefined, property = Property, args = simple, right = Right}) ->
  wf:f("~s.~s = ~s;", [gui_utils:to_list(Target), gui_utils:to_list(Property), wf:render(Right)]);

render_action(#jquery{target = Target, method = undefined, property = Property, right = undefined}) ->
  wf:f("$('#~s').~s;", [gui_utils:to_list(Target), gui_utils:to_list(Property)]);

render_action(#jquery{target = Target, method = undefined, property = Property, right = Right}) ->
  wf:f("$('#~s').~s = ~s", [gui_utils:to_list(Target), gui_utils:to_list(Property), wf:render(Right)]).
