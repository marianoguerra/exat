-module(example1).
-export([main/0]).

-ex@alias(#{ex@Baz => ex@Foo_Bar_Baz,
            bare => ex@Foo_Long,
            ex@struct_alias => ex@Learn_User}).

main() -> 
    ex@Foo_Bar_Baz:my_fun(1),
    ex@Baz:my_fun(1),
    bare:my_fun(1),
    ex@No_Alias:my_fun(2),
    S = create_struct(#{name => "bob", age => 42}),
    get_name(S).

create_struct(MapVar) ->
    ex:s@Learn_User(MapVar),
    ex:s@Learn_User(#{name => "bob", age => 42}),
    ex:s@struct_alias(#{name => "bob", age => 42}).

get_name({ex@struct_alias, #{name := Name}}) ->
    Name;
get_name({ex@struct_alias, #{}}) ->
    {error, no_name}.
