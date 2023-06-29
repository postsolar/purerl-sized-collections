-module(data_vector@foreign).
-export([ firstImpl/1
        , unconsImpl/1
        , lastImpl/1
        , unsnocImpl/1
        , replicateImpl/2
        ]).

firstImpl([X | _]) -> X.

lastImpl(Xs) -> lists:last(Xs).

unconsImpl([X | Xs]) -> #{ head => X, tail => Xs }.

unsnocImpl(Xs) ->
  #{ init => lists:reverse(tl(lists:reverse(Xs)))
  , last => lists:last(Xs)
  }.

replicateImpl(N, X) -> lists:duplicate(N, X).


