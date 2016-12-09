-module(ezk).

%% API exports
-export([
         register_app/2,
         start_span/2,
         end_span/0
        ]).

%%====================================================================
%% API functions
%%====================================================================
register_app(Application, Name) ->
    application:set_env(ezk, Application, Name).

start_span(Name, AppName) ->
    Sampled =
        case application:get_env(ezk, {sample_rate, Name}) of
            undefined ->
                case application:get_env(ezk, sample_rate) of
                    undefined ->
                        false;
                    {ok, Rate} ->
                        rand:uniform() < Rate
                end;
            {ok, Rate} ->
                rand:uniform() < Rate
        end,
    if Sampled ->
            %% not 64bit, but we'll replace this later
            Label = rand:uniform(16#ffffffffffffff),
            seq_trace:set_token(label, Label),
            seq_trace:set_token(send, true),
            seq_trace:set_token(print, true),
            %%seq_trace:set_token('receive', true),
            seq_trace:set_token(monotonic_timestamp, true),
            seq_trace:print(Label, {init, Name, AppName});
       true ->
            ok
    end.

end_span() ->
    seq_trace:print(deinit),
    seq_trace:set_token([]).

%%====================================================================
%% Internal functions
%%====================================================================

