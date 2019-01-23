-module(either).

-compile([export_all]).

-type either(L, R) :: {ok, L}
                    | {error, R}.

-type my_error() :: not_pos.

-spec f(integer()) -> either(integer(), my_error()).
f(I) when I > 0 -> {ok, I};
%f(I) when I > 0 -> {error, I};
f(_)            -> {error, not_pos}.

g() ->
    f(a).
