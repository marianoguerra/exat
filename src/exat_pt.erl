-module(exat_pt).
-export([parse_transform/2, format_error/1]).

parse_transform(Forms0, _Options) ->
    State = #{aliases => #{}, warnings => []},
    {Forms, NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    report_problems(NewState),
    remove_eof_and_ignore(Forms).

format_error(Error) -> atom_to_list(Error).

walker(State=#{aliases := CurAliases}, {attribute, Line, ex@alias, AliasMap})
  when is_map(AliasMap)->
    AliasPList = maps:to_list(AliasMap),
    NewAliases = maps:from_list([{atom_to_ex_atom(From), atom_to_ex_atom(To)} ||
                                 {From, To} <- AliasPList]),
    % NOTE: nothing is being filtered now, we allow "bare" aliases
    NewValidAliases = maps:filter(fun (nil, _) -> false;
                                      (_, nil) -> false;
                                      (_, _) -> true
                                  end, NewAliases),
    State = case {map_size(NewAliases), map_size(NewValidAliases)} of
        {S, S} ->
            State;
        {_, _} ->
            add_warning(State, ex_invalid_aliases,
                        #{all => NewAliases,
                          valid => NewValidAliases,
                          line => Line})
    end,
    FinalAliases = maps:merge(CurAliases, NewValidAliases),
    NewState = State#{aliases := FinalAliases},
    {ignore, NewState};
walker(State, Ast={atom, AtomLine, FunName}) ->
    case atom_to_list(FunName) of
        "ex@" ++ RestName ->
            #{aliases := Aliases} = State,
            ExAtomBase = to_ex_atom(RestName),
            ExAtom = maps:get(ExAtomBase, Aliases, ExAtomBase),
            {{atom, AtomLine, ExAtom}, State};
        _ ->
            {Ast, State}
    end;
walker(State=#{aliases := Aliases},
       {call, CLine, {remote, Line, {atom, ModLine, ModAtom}, FunName}, Args}) ->
    MaybeNewModName = maps:get(ModAtom, Aliases, ModAtom),
    NewAst = {call, CLine,
              {remote, Line, {atom, ModLine, MaybeNewModName}, FunName}, Args},
    {NewAst, State};

walker(State, Other) ->
    {Other, State}.


remove_eof_and_ignore(Ast) ->
	lists:filter(fun ({eof, _}) -> false;
                     (ignore) -> false;
                     (_) -> true
                 end, Ast).

atom_to_ex_atom(Atom) ->
    case atom_to_list(Atom) of
        "ex@" ++ RestName ->
            to_ex_atom(RestName);
        _ -> Atom
    end.

to_ex_atom(Name) ->
    ElixirIdStr = "Elixir." ++ lists:flatten(string:replace(Name, "_", ".", all)),
    list_to_atom(ElixirIdStr).

add_warning(State=#{warnings := Warns}, Type, Info) ->
    State#{warnings := [{Type, Info}|Warns]}.

report_problems(#{warnings := []}) -> ok;
report_problems(#{warnings := Warns}) ->
    [io:format(Warn) || Warn <- Warns].