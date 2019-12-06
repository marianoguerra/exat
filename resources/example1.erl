-module(example1).
-export([main/0]).

-ex@alias(#{ex@Baz => ex@Foo_Bar_Baz,
           bare => ex@Foo_Long}).

main() -> 
    ex@Foo_Bar_Baz:my_fun(1),
    ex@Baz:my_fun(1),
    bare:my_fun(1),
    ex@No_Alias:my_fun(2).
