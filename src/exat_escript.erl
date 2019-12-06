-module(exat_escript).
-export([main/1]).

main(["pt", Target, ModulePath])
  when (Target =:= "erl" orelse Target =:= "ast") ->

	{ok, Forms} = epp:parse_file(ModulePath, []),
	ModAst = exat_pt:parse_transform(Forms, [{exat_opts, #{}}]),
	ast_to_target(ModAst, Target),
    erlang:halt(0);

main(Args) ->
    io:format("Unknown Arguments: ~p~n", [Args]),
    usage(),
    erlang:halt(0).

ast_to_target(ModAst, Target) ->
    case Target of
        "erl" ->
            ModAstNoFileAttr = tl(ModAst),
            ErlCode = exat_compiler:ast_to_erl(ModAstNoFileAttr),
            io:format("~s~n", [ErlCode]);
        "ast" ->
            io:format("~p~n", [ModAst])
    end.

usage() ->
    io:format("Usage:~n"),
    io:format("  exat pt erl|ast <path/to/module.erl>~n"),
    io:format("    % do the same the parse transform would do"),
    io:format("    % parse module, modify it to expose proto functions"),
    io:format("    % write proto metadata in <output-dir> and print"),
    io:format("    % updated module in erl or ast format").
