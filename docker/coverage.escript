#!/usr/bin/env escript

%%%--------------------------------------------------------------------
%%% @author Piotr Ociepka
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This script analyses and merges test coverage reports generated by
%%% EUnit and CT frameworks.
%%% @end
%%%--------------------------------------------------------------------

%%%===================================================================
%%% Main function
%%%===================================================================


%%%===================================================================
%%% Main function
%%%===================================================================

main([BaseDir]) ->
  % gettint lists of excluded modules and ebin directories
  % from cover.spec file
  {ok, Terms} = file:consult(filename:join([BaseDir, "test_distributed/cover.spec"])),
  ExcludedModules = lists:map(fun(X) -> atom_to_list(X) end, get_excluded_modules(Terms)),
  EbinDirs = get_ebin_directories(Terms),
  % lists of module names
  AllModules = [get_modules_list(filename:join([BaseDir, X]), ".beam") || X <- EbinDirs],
  {_, CtModules} = get_modules_list(filename:join([BaseDir, "test_distributed"]), ".erl"),
  OmittedModules = CtModules ++ ExcludedModules,

  % loading .beam files for cover server
  cover:start(),
  [[cover:compile_beam(filename:join([Ebin, M]))
      || M <- Modules, not lists:member(M, OmittedModules)]
    || {Ebin, Modules} <- AllModules],

  % getting directory name in which coverage reports from CT are
  {ok, LS} = file:list_dir(filename:join([BaseDir, "test_distributed", "logs"])),

  % loading CT coverage reports (if any)
  CT_dirs = lists:filter(fun(X) -> lists:prefix("ct_run", X) end, LS),
  case CT_dirs of
    [CT_dir | _] -> ok = cover:import(filename:join([BaseDir, "test_distributed", "logs", CT_dir, "all.coverdata"]));
    [] -> ok
  end,

  % loading eunit coverage reports
  ok = cover:import(filename:join([BaseDir, "_build", "test", "cover", "eunit.coverdata"])),

  % output directory; if exists, we re-create it
  CoverDirPath = filename:join([BaseDir, "test_coverage"]),
  case file:make_dir(CoverDirPath) of
    ok -> ok;
    {error, eexist} -> file:del_dir(CoverDirPath),
      file:make_dir(CoverDirPath)
  end,

  % generating reports for single modules
  [cover:analyze_to_file(
      Mod,
      filename:join([BaseDir, "test_coverage", atom_to_list(Mod) ++ ".COVER.html"]),
      [html])
    || Mod <- cover:modules()],
  % generating .coverdata, just in case
  cover:export(filename:join([BaseDir, "test_coverage", "merged.coverdata"])),

  % generating coverage stats for each module
  % [{module, {covered, noncovered}}]
  RawCoverage = [cover:analyse(Mod, module) || Mod <- cover:modules()],
  ModulesCoverage = lists:map(fun({ok, X}) -> X end, RawCoverage),
  ModulesCoverage_Sorted = lists:sort(
    fun({ModuleA, _CoverA}, {ModuleB, _CoverB}) ->
      ModuleA < ModuleB
    end,
    ModulesCoverage),
  % generating index.html
  generate_html_report(ModulesCoverage_Sorted, BaseDir),

  cover:stop().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of module names in Path directory provided by
%%% files with Extension.
%%% @end
%%%-------------------------------------------------------------------
get_modules_list(Path, Extension) ->
  {ok, AllFiles} = file:list_dir(Path),
  AllBeams = lists:filter(
    fun(X) -> filename:extension(X) =:= Extension end,
    AllFiles),
  AllModules = lists:map(fun(X) -> filename:basename(X, Extension) end, AllBeams),
  {Path, AllModules}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Converts the tuple represrnting coverage stats for some module
%%% into coresponding HTML line for index.html report file.
%%% @end
%%%-------------------------------------------------------------------
generate_html_line({Module, {Covered, Uncovered}}) ->
  Percentage = case (Covered + Uncovered) of
    0 -> 0.0;
    _ -> 100.0 * Covered / (Covered + Uncovered)
  end,
  ModuleHtml = "<td><a href=\"" ++ atom_to_list(Module) ++ ".COVER.html\">" ++
    atom_to_list(Module) ++ "</a></td>",
  PercentHtml = "<td>" ++ float_to_list(Percentage, [{decimals, 0}]) ++ " %</td>",
  CoveredHtml = "<td>" ++ integer_to_list(Covered) ++ "</td>",
  UncoveredHtml = "<td>" ++ integer_to_list(Uncovered) ++ "</td>",
  "<tr>" ++ ModuleHtml ++ PercentHtml ++ CoveredHtml ++ UncoveredHtml ++ "</tr>\n".

%%%-------------------------------------------------------------------
%%% @doc
%%% Generates the summary HTML line from coverage stats list.
%%% @end
%%%-------------------------------------------------------------------
generate_summary_line(ModulesCoverage) ->
  {Covered, Uncovered} = total_coverage(ModulesCoverage),
  Percentage = case (Covered + Uncovered) of
    0 -> 0.0;
    _ -> 100.0 * Covered / (Covered + Uncovered)
  end,
  PercentHtml = "<th>" ++ float_to_list(Percentage, [{decimals, 0}]) ++ " %</th>",
  CoveredHtml = "<th>" ++ integer_to_list(Covered) ++ "</th>",
  UncoveredHtml = "<th>" ++ integer_to_list(Uncovered) ++ "</th>",
  "<tr>" ++ "<th>TOTAL</th>" ++ PercentHtml ++ CoveredHtml ++ UncoveredHtml ++ "</tr>\n".

%%%-------------------------------------------------------------------
%%% @doc
%%% Generates HTML report describing test coverage for each module
%%% and for whole project and saves it in index.html file.
%%% @end
%%%-------------------------------------------------------------------
generate_html_report(ModulesCoverage, BaseDir) ->
  {ok, Report} = file:open(
    filename:join([BaseDir, "test_coverage", "index.html"]),
    [write]),
  file:write(Report, "<html>\n<body>\n"),
  file:write(Report, "<table border=3 cellpadding=5>\n"),
  file:write(Report, "<tr><th>Module</th><th>Covered (%)</th>
    <th>Covered (Lines)</th><th>Not covered (Lines)</th></tr>\n"),

  lists:map(fun(X) -> file:write(Report, generate_html_line(X)) end, ModulesCoverage),

  file:write(Report, generate_summary_line(ModulesCoverage)),

  file:write(Report, "</table>\n</body>\n</html>"),
  file:close(Report).

%%%-------------------------------------------------------------------
%%% @doc
%%% Computes the total count of covered and uncovered lines
%%% for modules in ModulesCoverage list.
%%% @end
%%%-------------------------------------------------------------------
total_coverage(ModulesCovarage) ->
  RawCoverage = lists:map(
    fun({_Mod, {Covered, Uncovered}}) -> {Covered, Uncovered} end,
    ModulesCovarage),
  TotalCoverage = lists:foldl(
    fun({Covered, Uncovered}, {TotalCovered, TotalUncovered}) ->
      {Covered + TotalCovered, Uncovered + TotalUncovered}
    end,
    {0,0},
    RawCoverage),
  TotalCoverage.

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of ebin directories (specified in cover.spec file).
%%% @end
%%%-------------------------------------------------------------------
get_ebin_directories([]) -> [];
get_ebin_directories([{incl_dirs_r, Mods}|_T]) -> Mods;
get_ebin_directories([_H|T]) -> get_ebin_directories(T).

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of excuded modules (specified in cover.spec file).
%%% @end
%%%-------------------------------------------------------------------
get_excluded_modules([]) -> [];
get_excluded_modules([{excl_mods, Mods}|_T]) -> Mods;
get_excluded_modules([_H|T]) -> get_excluded_modules(T).