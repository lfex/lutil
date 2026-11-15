-module(lutil_regression_tests).
-export([child_main/0]).
-include_lib("eunit/include/eunit.hrl").

collections_test() ->
    ?assertEqual([], zip_empty_bounded()),
    ?assertEqual([[a, 1], [b, 2]], 'lutil-list':zip([[a, b], [1, 2]])),
    ?assertEqual([[a, 1]], 'lutil-list':zip([[a], [1, 2]])),
    ?assertEqual({[k1, k2], [atom_value, "tail"]},
                 'lutil-list':partition([k1, atom_value, k2, "tail"])),
    ?assertEqual({[k1, k2, k3], [v1, v2, v3]},
                 'lutil-list':partition([k3, v3], [k2, k1], [v2, v1])),
    Dict = 'lutil-list':'->dict'([k1, atom_value, k2, "tail"]),
    ?assertEqual(atom_value, dict:fetch(k1, Dict)),
    ?assertEqual("tail", dict:fetch(k2, Dict)),
    ?assertEqual({[1, 2], 3}, 'lutil-tuple':cat({[1, 2]}, {3})).

zip_empty_bounded() ->
    Parent = self(),
    {Pid, Ref} = spawn_opt(
                   fun() -> Parent ! {self(), 'lutil-list':zip([])} end,
                   [monitor, {max_heap_size,
                              #{size => 100000, kill => true,
                                error_logger => false}}]),
    try
        receive
            {Pid, Result} ->
                receive
                    {'DOWN', Ref, process, Pid, normal} -> Result;
                    {'DOWN', Ref, process, Pid, Reason} ->
                        erlang:error({zip_worker_failed, Reason})
                after 1000 ->
                    erlang:error(zip_worker_exit_missing)
                end;
            {'DOWN', Ref, process, Pid, Reason} ->
                erlang:error({zip_worker_down_without_result, Reason})
        after 2000 ->
            erlang:error(zip_empty_timeout)
        end
    after
        case is_process_alive(Pid) of
            true ->
                exit(Pid, kill),
                receive
                    {'DOWN', Ref, process, Pid, _} -> ok
                after 1000 ->
                    erlang:error(zip_worker_shutdown_missing)
                end;
            false ->
                receive
                    {'DOWN', Ref, process, Pid, _} -> ok
                after 0 -> ok
                end
        end,
        erlang:demonitor(Ref, [flush])
    end.

trim_trailing_whitespace_test() ->
    ?assertEqual(<<"foo">>,
                 'lutil-bin':'trim-trailing-ws'(<<102, 111, 111, 32, 9, 13, 10, 32>>)),
    ?assertEqual(<<"value---">>,
                 'lutil-bin':'trim-trailing'(<<"value---">>, <<"->">>)),
    ?assertEqual(<<"value">>,
                 'lutil-bin':'trim-trailing'(<<"value---">>, <<"-">>)).

math_contracts_test() ->
    ?assertEqual(50.0, 'lutil-math':scale(15, {10, 20}, {0, 100})),
    ?assertEqual(4.0, 'lutil-math':scale(-2, {-4, 0}, {0, 8})),
    ?assertEqual(0.0, 'lutil-math':scale(-4, {-4, 0}, {0, 8})),
    ?assertEqual(8.0, 'lutil-math':scale(0, {-4, 0}, {0, 8})),
    ?assertEqual(4, 'lutil-math':gcd(-8, 12)),
    ?assertEqual(4, 'lutil-math':gcd(8, -12)),
    ?assertEqual(4, 'lutil-math':gcd(-8, -12)),
    ?assertEqual(0, 'lutil-math':gcd(0, 0)),
    ?assertEqual(false, 'lutil-math':'prime?'(0)),
    ?assertEqual(false, 'lutil-math':'prime?'(-1)),
    ?assertEqual(false, 'lutil-math':'prime?'(1)),
    ?assertEqual(2, 'lutil-math':'get-next-prime'(0)),
    ?assertEqual([2, 3, 5, 7, 11],
                 ['lutil-math':'get-next-prime'(N) || N <- [1, 2, 3, 5, 7]]),
    FloatText = 'lutil-math':'float->str'(1.5),
    ?assert(is_list(FloatText)),
    ?assertEqual(1.5, list_to_float(FloatText)),
    ?assertEqual(2.0, list_to_float('lutil-math':'float->str'(2.0))).

text_contracts_test() ->
    ?assertEqual("", 'lutil-text':'wrap-text'("", 10)),
    ?assertEqual("", 'lutil-text':'wrap-text'("   ", 10)),
    ?assertEqual("01234\n56789", 'lutil-text':'wrap-text'("01234 56789", 10)),
    ?assertEqual("longword", 'lutil-text':'wrap-text'("longword", 3)),
    ?assertEqual(<<"\nhello">>,
                 iolist_to_binary('lutil-text':wrap("hello", 10))).

environment_names_test() ->
    Empty = lfe_env:new(),
    ?assertEqual([], lutil:'get-env-funcs'(Empty)),
    Env1 = lfe_env:add_fbinding(alpha, 1, fun(_) -> ok end, Empty),
    Env2 = lfe_env:add_fbinding(alpha, 2, fun(_, _) -> ok end, Env1),
    Env3 = lfe_env:add_fbinding(zeta, 0, fun() -> ok end, Env2),
    Env4 = lfe_env:add_mbinding(beta, {macro, []}, Env3),
    ?assertEqual([alpha, beta, zeta], lutil:'get-env-funcs'(Env4)).

lcycle_ordering_test() ->
    ?assertEqual({a, [b, c, a]}, lcycle:next([a, b, c])),
    ?assertEqual({c, [c, a, b]}, lcycle:last([a, b, c])),
    ?assertEqual({[], [a, b, c]}, lcycle:take(0, [a, b, c])),
    ?assertEqual({[a, b, c, a], [b, c, a]}, lcycle:take(4, [a, b, c])),
    ?assertEqual({[a, b, c], [a, b, c]}, lcycle:lastn(3, [a, b, c])),
    ?assertEqual({[b, c], [b, c, a]}, lcycle:lastn(2, [a, b, c])),
    Pid = lcycle:new([a, b, c]),
    unlink(Pid),
    Ref = monitor(process, Pid),
    try
        ?assertEqual(a, lcycle:next(Pid)),
        ok = lcycle:reset(Pid),
        ?assertEqual(c, lcycle:last(Pid)),
        ok = lcycle:reset(Pid),
        ?assertEqual([], lcycle:lastn(0, Pid)),
        ok = lcycle:reset(Pid),
        ?assertEqual([a, b, c, a], lcycle:take(4, Pid)),
        ok = lcycle:reset(Pid),
        ?assertEqual([b, c], lcycle:lastn(2, Pid))
    after
        case is_process_alive(Pid) of
            true -> lcycle:stop(Pid);
            false -> ok
        end,
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 1000 ->
            erlang:error(lcycle_stop_missing)
        end,
        demonitor(Ref, [flush])
    end.

compiler_boundaries_test_() ->
    {timeout, 45, fun compiler_boundaries/0}.

compiler_boundaries() ->
    Result = run_isolated("compiler"),
    assert_child_status(0, Result),
    assert_output_marker(<<"compiler_fixture_ok">>, Result).

mnesia_macro_test_() ->
    {timeout, 45, fun mnesia_macro/0}.

mnesia_macro() ->
    Result = run_isolated("mnesia"),
    assert_child_status(0, Result),
    assert_output_marker(<<"mnesia_fixture_ok">>, Result).

harness_success_test_() ->
    {timeout, 45, fun harness_success/0}.
harness_success() ->
    Result = run_isolated("control-success"),
    assert_child_status(0, Result),
    assert_output_marker(<<"control_success">>, Result),
    assert_cleanup_record(Result).

harness_error_test_() ->
    {timeout, 45, fun harness_error/0}.
harness_error() ->
    Result = run_isolated("control-error"),
    assert_child_status(1, Result),
    assert_output_marker(<<"S03_FAILURE=">>, Result),
    assert_cleanup_record(Result).

harness_watchdog_test_() ->
    {timeout, 45, fun harness_watchdog/0}.
harness_watchdog() ->
    Result = run_isolated("control-watchdog"),
    assert_child_status(124, Result),
    assert_cleanup_record(Result).

assert_cleanup_record(Result) ->
    ?assert(filelib:is_dir(maps:get(root, Result))),
    ?assert(filelib:is_file(maps:get(log, Result))),
    ?assertNot(filelib:is_dir(maps:get(work, Result))).

assert_child_status(Expected, Result) ->
    case {maps:get(status, Result), maps:get(shutdown, Result)} of
        {Expected, complete} -> ok;
        Other -> erlang:error({child_result_mismatch, Expected, Other, Result})
    end.

assert_output_marker(Marker, Result) ->
    case binary:match(maps:get(output, Result), Marker) of
        {_, _} -> ok;
        nomatch -> erlang:error({child_marker_missing, Marker, Result})
    end.

run_isolated(Case) ->
    Root = claim_private_root(),
    Work = owned_path(Root, filename:join(Root, "work")),
    ok = file:make_dir(Work),
    Log = owned_path(Root, filename:join(Root, "combined.log")),
    {ok, LogFile} = file:open(Log, [write, binary, exclusive]),
    ok = file:close(LogFile),
    Result0 = launch_child(Root, Work, Log, Case),
    case {maps:get(status, Result0), maps:get(shutdown, Result0)} of
        {Status, complete} when is_integer(Status) ->
            ok = file:del_dir_r(Work),
            Result0#{root => Root, work => Work, log => Log};
        Other ->
            erlang:error({child_shutdown_incomplete, Case, Other, Root,
                          Log, maps:get(os_pid, Result0)})
    end.

claim_private_root() ->
    Tmp = case os:getenv("TMPDIR") of
        Value when is_list(Value) ->
            case filename:pathtype(Value) =:= absolute andalso filelib:is_dir(Value) of
                true -> Value;
                false -> "/tmp"
            end;
        false -> "/tmp"
    end,
    claim_private_root(Tmp, 20).

claim_private_root(_Parent, 0) ->
    erlang:error(cannot_claim_private_root);
claim_private_root(Parent, Attempts) ->
    Suffix = binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12))),
    Root = filename:join(Parent, "lutil-s03-" ++ Suffix),
    case file:make_dir(Root) of
        ok -> filename:absname(Root);
        {error, eexist} -> claim_private_root(Parent, Attempts - 1);
        {error, Reason} -> erlang:error({cannot_claim_private_root, Root, Reason})
    end.

owned_path(Root, Path) ->
    AbsRoot = filename:absname(Root),
    AbsPath = filename:absname(Path),
    case lists:prefix(AbsRoot ++ "/", AbsPath) of
        true -> AbsPath;
        false -> erlang:error({path_outside_owned_root, AbsRoot, AbsPath})
    end.

launch_child(Root, Work, Log, Case) ->
    Parent = self(),
    Erl = filename:join([code:root_dir(), "bin", "erl"]),
    true = filelib:is_file(Erl),
    LfeEbin = filename:dirname(code:which(lfe_comp)),
    LutilEbin = filename:dirname(code:which(lutil)),
    TestEbin = filename:dirname(code:which(?MODULE)),
    Dump = owned_path(Root, filename:join(Root, "erl_crash.dump")),
    Args = ["-noshell", "-noinput", "-pa", LfeEbin, LutilEbin, TestEbin,
            "-s", "lutil_regression_tests", "child_main", "-extra", Root, Case],
    {Owner, OwnerMon} = spawn_monitor(fun() ->
        process_flag(trap_exit, true),
        Parent ! {self(), port_owner(Erl, Args, Work, Dump, Log)}
    end),
    receive
        {Owner, Result} ->
            demonitor(OwnerMon, [flush]),
            Result;
        {'DOWN', OwnerMon, process, Owner, Reason} ->
            erlang:error({port_owner_failed, Reason, Root, Log});
        {'EXIT', Owner, Reason} ->
            erlang:error({port_owner_exit, Reason, Root, Log})
    after 37000 ->
        erlang:error({port_owner_timeout, Root, Log})
    end.

port_owner(Erl, Args, Work, Dump, Log) ->
    Port = open_port({spawn_executable, Erl},
                     [binary, use_stdio, stderr_to_stdout, exit_status,
                      {cd, Work}, {env, [{"ERL_CRASH_DUMP", Dump}]},
                      {args, Args}]),
    OsPid = case erlang:port_info(Port, os_pid) of
        {os_pid, Value} -> Value;
        undefined -> undefined
    end,
    PortMon = erlang:monitor(port, Port),
    Deadline = erlang:monotonic_time(millisecond) + 35000,
    try collect_port(Port, PortMon, Deadline, Log, [], undefined, false, OsPid) of
        Result ->
            erlang:demonitor(PortMon, [flush]),
            Result
    catch
        Class:Reason:Stack ->
            close_port_after_failure(Port),
            await_port_down(Port, PortMon, OsPid),
            erlang:raise(Class, Reason, Stack)
    end.

collect_port(Port, PortMon, Deadline, Log, Chunks, ExitStatus, Down, OsPid) ->
    case {ExitStatus, Down} of
        {Code, true} when is_integer(Code) ->
            #{output => iolist_to_binary(lists:reverse(Chunks)), status => Code,
              shutdown => complete, os_pid => OsPid};
        _ ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true -> erlang:error({child_exit_timeout, OsPid, Log});
                false ->
                    receive
                        {Port, {data, Data}} ->
                            ok = file:write_file(Log, Data, [append]),
                            collect_port(Port, PortMon, Deadline, Log,
                                         [Data | Chunks], ExitStatus, Down, OsPid);
                        {Port, {exit_status, Code}} ->
                            collect_port(Port, PortMon, Deadline, Log, Chunks,
                                         Code, Down, OsPid);
                        {'DOWN', PortMon, port, Port, _Reason} ->
                            collect_port(Port, PortMon, Deadline, Log, Chunks,
                                         ExitStatus, true, OsPid);
                        {'EXIT', Port, _Reason} ->
                            collect_port(Port, PortMon, Deadline, Log, Chunks,
                                         ExitStatus, Down, OsPid)
                    after Deadline - Now ->
                        erlang:error({child_exit_timeout, OsPid, Log})
                    end
            end
    end.

close_port_after_failure(Port) ->
    case erlang:port_info(Port) of
        undefined -> ok;
        _ ->
            try erlang:port_close(Port)
            catch error:badarg -> ok
            end
    end.

await_port_down(Port, PortMon, OsPid) ->
    receive
        {'DOWN', PortMon, port, Port, _Reason} -> ok
    after 1000 ->
        erlang:error({port_shutdown_incomplete, OsPid})
    end.

child_main() ->
    [Root, Case] = init:get_plain_arguments(),
    WatchdogDelay = case Case of "control-watchdog" -> 100; _ -> 30000 end,
    Watchdog = spawn(fun() ->
        receive stop -> ok
        after WatchdogDelay -> erlang:halt(124, [{flush, false}])
        end
    end),
    try
        ok = file:set_cwd(owned_path(Root, filename:join(Root, "work"))),
        Result = run_child_case(Case, Root),
        io:format("S03_RESULT=~p~n", [Result]),
        Watchdog ! stop,
        erlang:halt(0, [{flush_timeout, 1000}])
    catch
        Class:Reason:Stack ->
            io:format("S03_FAILURE=~p~n", [{Class, Reason, Stack}]),
            Watchdog ! stop,
            erlang:halt(1, [{flush_timeout, 1000}])
    end.

run_child_case("compiler", Root) -> compiler_fixture(Root);
run_child_case("mnesia", Root) -> mnesia_fixture(Root);
run_child_case("control-success", _Root) -> "control_success";
run_child_case("control-error", _Root) -> erlang:error(controlled_child_error);
run_child_case("control-watchdog", _Root) -> receive never -> unreachable end.

compiler_fixture(Root) ->
    Project = owned_path(Root, filename:join(Root, "work/project")),
    Src = owned_path(Root, filename:join(Project, "src")),
    Custom = owned_path(Root, filename:join(Project, "custom")),
    Ebin = owned_path(Root, filename:join(Project, "ebin")),
    ok = filelib:ensure_dir(filename:join(Src, "placeholder")),
    ok = file:make_dir(Custom),
    ok = file:make_dir(Ebin),
    write_owned(Root, filename:join(Src, "s03_compiler_probe.lfe"),
                "(defmodule s03_compiler_probe (export (value 0)))\n"
                "(defun value () 42)\n"),
    write_owned(Root, filename:join(Src, "s03_fixture.app.src"),
                "{application,s03_fixture,[{vsn,\"0.1.0\"}]}.\n"),
    ok = file:set_cwd(owned_path(Root, Project)),
    CompileResult = 'lutil-file':'compile-src'("custom"),
    AppResult = {ok, {'app-src', "src/s03_fixture.app.src"},
                     {'app-dst', "custom/s03_fixture.app"}},
    true = lists:member({ok, [{ok, s03_compiler_probe}]}, CompileResult),
    true = lists:member(AppResult, CompileResult),
    true = filelib:is_file(owned_path(Root, filename:join(Custom, "s03_compiler_probe.beam"))),
    SourceApp = read_owned(Root, filename:join(Src, "s03_fixture.app.src")),
    CustomApp = read_owned(Root, filename:join(Custom, "s03_fixture.app")),
    ?assertEqual(SourceApp, CustomApp),
    {module, s03_compiler_probe} =
        code:load_abs(filename:rootname(filename:join(Custom, "s03_compiler_probe"))),
    42 = s03_compiler_probe:value(),
    DefaultResult = 'lutil-file':'compile-app-src'(),
    ?assertEqual({ok, {'app-src', "src/s03_fixture.app.src"},
                      {'app-dst', "ebin/s03_fixture.app"}}, DefaultResult),
    DefaultApp = read_owned(Root, filename:join(Ebin, "s03_fixture.app")),
    ?assertEqual(SourceApp, DefaultApp),
    NoApp = owned_path(Root, filename:join(Root, "work/no-app")),
    NoSrc = owned_path(Root, filename:join(NoApp, "src")),
    ok = filelib:ensure_dir(filename:join(NoSrc, "placeholder")),
    ok = file:set_cwd(owned_path(Root, NoApp)),
    ?assertEqual({error, {app_src_not_found, NoSrc}},
                 'lutil-file':'compile-app-src'("custom")),
    CopyProject = owned_path(Root, filename:join(Root, "work/copy-project")),
    CopySrc = owned_path(Root, filename:join(CopyProject, "src")),
    ok = filelib:ensure_dir(filename:join(CopySrc, "placeholder")),
    write_owned(Root, filename:join(CopySrc, "s03_fixture.app.src"),
                "{application,s03_fixture,[]}.\n"),
    ok = file:set_cwd(owned_path(Root, CopyProject)),
    ?assertEqual({error, {app_src_copy_failed, "src/s03_fixture.app.src",
                          "missing/s03_fixture.app", enoent}},
                 'lutil-file':'compile-app-src'("missing")),
    ok = file:set_cwd(owned_path(Root, Project)),
    Good = owned_path(Root, filename:join(Project, "deps/z_good")),
    GoodSrc = owned_path(Root, filename:join(Good, "src")),
    GoodEbin = owned_path(Root, filename:join(Good, "ebin")),
    ok = filelib:ensure_dir(filename:join(GoodSrc, "placeholder")),
    ok = file:make_dir(GoodEbin),
    write_owned(Root, filename:join(GoodSrc, "s03_good.lfe"),
                "(defmodule s03_good (export (value 0)))\n"
                "(defun value () 42)\n"),
    ?assertEqual(ok, 'lutil-file':'compile-deps'()),
    GoodBeam = owned_path(Root, filename:join(GoodEbin, "s03_good.beam")),
    true = filelib:is_file(GoodBeam),
    ok = file:delete(GoodBeam),
    Bad = owned_path(Root, filename:join(Project, "deps/a_bad")),
    BadSrc = owned_path(Root, filename:join(Bad, "src")),
    BadEbin = owned_path(Root, filename:join(Bad, "ebin")),
    ok = filelib:ensure_dir(filename:join(BadSrc, "placeholder")),
    ok = file:make_dir(BadEbin),
    write_owned(Root, filename:join(BadSrc, "s03_bad.lfe"),
                "(defmodule s03_bad (export (value 0)))\n(defun value ()\n"),
    Before = file:get_cwd(),
    DependencyResult = 'lutil-file':'compile-deps'(),
    ?assertEqual(Before, file:get_cwd()),
    BadAbs = filename:absname(Bad),
    GoodAbs = filename:absname(Good),
    ?assertEqual({error, {dependency_compile_failed,
                          [{BadAbs, [{error, "src/s03_bad.lfe"}]},
                           {GoodAbs, [{ok, [{ok, s03_good}]}]}]}},
                 DependencyResult),
    false = filelib:is_file(filename:join(BadEbin, "s03_bad.beam")),
    true = filelib:is_file(GoodBeam),
    ?assertEqual({ok, {s03_good, []}}, beam_lib:chunks(GoodBeam, [])),
    io:format("COMPILER_RESULTS=~p~n", [DependencyResult]),
    "compiler_fixture_ok".

mnesia_fixture(Root) ->
    Work = owned_path(Root, filename:join(Root, "work")),
    Source = owned_path(Root, filename:join(Work, "s03_mnesia_probe.lfe")),
    BeamDir = owned_path(Root, filename:join(Work, "mnesia-beams")),
    ok = file:make_dir(BeamDir),
    LutilEbin = filename:dirname(code:which(lutil)),
    IncludeDir = filename:absname(filename:join(LutilEbin, "../include")),
    true = filelib:is_dir(IncludeDir),
    SourceText = "(defmodule s03_mnesia_probe (export (make_empty 0) (make_explicit 0)))\n"
                 "(include-file \"mnesia-macros.lfe\")\n"
                 "(defrecord s03_mnesia_empty key value)\n"
                 "(defrecord s03_mnesia_explicit id value)\n"
                 "(defun make_empty () (create-table s03_mnesia_empty ()))\n"
                 "(defun make_explicit () (create-table s03_mnesia_explicit "
                 "(list (tuple 'ram_copies (list (node))) (tuple 'type 'set))))\n",
    write_owned(Root, Source, SourceText),
    {ok, [{ok, s03_mnesia_probe}]} =
        lfe_comp:file(Source, [report, {outdir, BeamDir}, {i, IncludeDir}]),
    Beam = owned_path(Root, filename:join(BeamDir, "s03_mnesia_probe.beam")),
    {module, s03_mnesia_probe} = code:load_abs(filename:rootname(Beam)),
    DataDir = owned_path(Root, filename:join(Work, "mnesia-data")),
    ok = file:make_dir(DataDir),
    ok = application:set_env(mnesia, dir, DataDir),
    try
        ok = mnesia:create_schema([node()]),
        ok = application:start(mnesia),
        {atomic, ok} = s03_mnesia_probe:make_empty(),
        {atomic, ok} = s03_mnesia_probe:make_explicit(),
        ?assertEqual([key, value], mnesia:table_info(s03_mnesia_empty, attributes)),
        ?assertEqual([id, value], mnesia:table_info(s03_mnesia_explicit, attributes)),
        mnesia:dirty_write({s03_mnesia_empty, key1, value1}),
        ?assertEqual([{s03_mnesia_empty, key1, value1}],
                     mnesia:dirty_read(s03_mnesia_empty, key1)),
        mnesia:dirty_write({s03_mnesia_explicit, id1, value2}),
        ?assertEqual([{s03_mnesia_explicit, id1, value2}],
                     mnesia:dirty_read(s03_mnesia_explicit, id1)),
        "mnesia_fixture_ok"
    after
        catch mnesia:delete_table(s03_mnesia_empty),
        catch mnesia:delete_table(s03_mnesia_explicit),
        catch application:stop(mnesia),
        catch mnesia:delete_schema([node()]),
        application:unset_env(mnesia, dir)
    end.

read_owned(Root, Path) ->
    {ok, Bytes} = file:read_file(owned_path(Root, Path)),
    Bytes.

write_owned(Root, Path, Data) ->
    ok = file:write_file(owned_path(Root, Path), Data).
