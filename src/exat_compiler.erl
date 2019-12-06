-module(exat_compiler).
-export([ast_to_erl/1]).

ast_to_erl(ModAst) ->
  erl_prettypr:format(erl_syntax:form_list(ModAst)).
