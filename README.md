# purerl-sized-collections
Vectors and sized maps for Purescript's Erlang backend.

Partially based on [purescript-fast-vect](https://github.com/sigma-andex/purescript-fast-vect) with API mirroring `Erl.Data.List` and `Erl.Data.Map` where possible.

Currently (07/2023), it requires a patched prelude to work. Namely, Purerl's Prelude lacks the module `Data.Reflectable`. Simply copy the contents of
said module from the Javascript backend and overwrite the link to the prelude in your `packages.dhall`.

# License
MIT
