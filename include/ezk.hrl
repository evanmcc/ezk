%% we need to bench which is faster, filtering out lager messages or
%% not generating them in the first place, because this looks kind of
%% expensive.
-define(info(I), ?info(I, [])).

-define(info(I, F),
        begin
            T=seq_trace:get_token(),
            seq_trace:set_token([]),
            lager:info(I, F),
            seq_trace:set_token(T)
        end).

-define(err(I), ?err(I, [])).

-define(err(I, F),
        begin
            T=seq_trace:get_token(),
            seq_trace:set_token([]),
            lager:info(I, F),
            seq_trace:set_token(T)
        end).
