-module(data_indexMap@foreign).
-export([ indexImpl/2
        , modifyImpl/3
        ]).

indexImpl(K, M) -> maps:get(K, M).

modifyImpl(K, F, M) -> maps:update_with(K, F, M).


