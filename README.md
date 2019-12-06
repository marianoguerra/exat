# exat: Elixir Module and Struct Interoperability for Erlang.

Write erlang friendly module names and get them translated into the right Elixir
module names automatically.

The project is a parse transform but also an escript to easily debug if the
transformation is being done correctly.

See the [exat_example](https://github.com/marianoguerra-atik/exat_example) project for a simple usage example.

## Erlang Friendly Elixir Module Names

A call like `ex@A_B_C:my_fun(1)` will be translated automatically to
`'Elixir.A.B.C:my_fun(1)'` at build time using a parse transform.

The trick is that the `@` symbol is allowed in atoms if it's not the first
character (thank node names for that). We use the `ex@` prefix to identify
the modules that we must translate since no one[1] uses that prefix for modules
in erlang.

## Aliases for Long Elixir Module Names

Since Elixir module names tend to nest and be long, you can define aliases
to use in your code and save some typing, for example the following alias
declaration:

```erlang
-ex@alias(#{ex@Baz => ex@Foo_Bar_Baz,
            bare => ex@Foo_Long}).
```

Will translate `ex@Bar:foo()` to `ex@Foo_Bar_Baz:foo()` which in turn will become `'Elixir.Foo.Bar.Baz:foo()`

It will also translate the module name `bare:foo()` into `ex@Foo_Long:foo()` which in turn will become `Elixir.Foo.Long:foo()`

## Creating Structs

The code:

```erlang
ex:s@Learn_User(MapVar)
```

Becomes:

```erlang
'Elixir.Learn.User':'__struct__'(MapVar)
```

The code:

```erlang
ex:s@Learn_User(#{name => "bob", age => 42})
```

Becodes:

```erlang
'Elixir.Learn.User':'__struct__'(#{name => "bob", age => 42})
```

Which in Elixir would be:

```elixir
%Learn.User{name: 'bob', age: 42}
```

### Aliases in Structs

The following alias declaration:

```erlang
-ex@alias(#{ex@struct_alias => ex@Learn_User}).
```

Will expand this:

```erlang
    ex:s@struct_alias(#{name => "bob", age => 42})
```

Into this:

```erlang
    'Elixir.Learn.User':'__struct__'(#{name => "bob", age => 42})
```

### Note on Static Compilation of Literal Structs

On Elixir if you pass the fields to the struct it will be compiled to a map
in place since the compiler knows all the fields and their defaults at compile
time, for now `exat` uses the slower version that merges the defaults against
the provided fields using `'Elixir.Enum':reduce` in the future it will try
to get the defaults at compile time if the struct being compiled already
has a beam file (that is, it was compiled before the current file).

Use
---

Add it to your rebar.config as a dep and as a parse transform:

```erlang
{erl_opts, [..., {parse_transform, exat}, ...]}.
{deps, [exat, ...]}
```

Note: Check if can be only a build profile dependency.

Build
-----

To build the escript:

    $ rebar3 escriptize

Run
---

You can run it as an escript:

    $ _build/default/bin/exat pp [erl|ast] path/to/module.erl

For example in this repo:

    $ _build/default/bin/exat pp erl resources/example1.erl
    $ _build/default/bin/exat pp ast resources/example1.erl

License
-------

Apache License 2.0
See LICENSE file for details

[1] Famous last words
