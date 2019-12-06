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

% similar to next until TODO of static compilation to map is solved
walker(State, Ast={call, L1, {remote, L2, {atom, L3, ex}, {atom, L4, FnName}},
               [FieldsAst={map, _L5, _Fields}]}) ->
    case atom_to_list(FnName) of
        "s@" ++ RestName ->
            {StructModAtomAst, State1} = rest_atom_to_ast(State, L3, RestName),
            % TODO: if we can call 'Elixir.Struct.Id':__struct__()
            % at compile time we can compile it diretly into a map with the
            % '__struct__' key like elixir does (faster)
            % 'Elixir.Struct.Id':__struct__(#{...})
            NewAst = {call, L1,
                      {remote, L2, StructModAtomAst, {atom, L4, '__struct__'}},
                      [FieldsAst]},
            {NewAst, State1};
        _ ->
            % TODO: warning?
            {Ast, State}
    end;

% here it's not an explicit map we call mod:__struct__/1
walker(State, Ast={call, L1, {remote, L2, {atom, L3, ex}, {atom, L4, FnName}},
               Args}) ->
    case atom_to_list(FnName) of
        "s@" ++ RestName ->
            {StructModAtomAst, State1} = rest_atom_to_ast(State, L3, RestName),
            NewAst = {call, L1,
                      {remote, L2, StructModAtomAst, {atom, L4, '__struct__'}},
                      Args},
            {NewAst, State1};
        _ ->
            % TODO: warning?
            {Ast, State}
    end;

% ex@Mod:fun(...) (or foo:bar(...) checking if foo is a bare alias)
walker(State=#{aliases := Aliases},
       {call, L1, {remote, L2, {atom, AtomLine, ModName}, FnIdAst}, Args}) ->
    case atom_to_list(ModName) of
        "ex@" ++ RestName ->
            {NewModNameAst, State1} = rest_atom_to_ast(State, AtomLine, RestName),
			NewAst = {call, L1, {remote, L2, NewModNameAst, FnIdAst}, Args},
            {NewAst, State1};
        _ ->
            MaybeNewModName = maps:get(ModName, Aliases, ModName),
            NewAst = {call, L1,
                      {remote, L2, {atom, AtomLine, MaybeNewModName}, FnIdAst},
                      Args},
            {NewAst, State}
    end;

walker(State, Ast={tuple, L1, [{atom, _L2, AtomName}, {map, _L3, MapFields}]}) ->
    case atom_to_list(AtomName) of
        "ex@" ++ RestName ->
            {StructAtomAst, State1} = rest_atom_to_ast(State, L1, RestName),
            MatchStructAttrAst = {map_field_exact, L1,
                                  {atom, L1, '__struct__'}, StructAtomAst},
			NewAst = {map, L1, [MatchStructAttrAst | MapFields]},
			{NewAst, State1};
        _ ->
            {Ast, State}
    end;

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

rest_atom_to_ast(State, Line, RestName) ->
    #{aliases := Aliases} = State,
    ExAtomBase = to_ex_atom(RestName),
    ExAtom = maps:get(ExAtomBase, Aliases, ExAtomBase),
    {{atom, Line, ExAtom}, State}.
