-module(ezk_tracer).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(NAME, ezk_tracer_state).
-define(S, #?NAME).

-record(?NAME,
        {
          spans = #{} :: #{integer() => term()}
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    seq_trace:set_system_tracer(self()),
    {ok, ?S{}}.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({seq_trace, Label, Msg, TStamp}, ?S{spans = Spans} = S) ->
    Spans1 =
        case Msg of
            {print, _, Origin, [], {init, Name, AppName}} ->
                Spans#{Label =>
                           #{name => Name,
                             app_name => AppName,
                             origin => Origin,
                             start => TStamp}};
            {print, _, _Origin, [], deinit} ->
                case maps:find(Label, Spans) of
                    error ->
                        lager:error("missing span ended"),
                        Spans;
                    {ok, Span} ->
                        Span1 = Span#{final => TStamp},
                        lager:debug("final span ~p", [Span1]),
                        ezk_export:finalize(Span1),
                        maps:remove(Label, Spans)
                end;
            %% subspan accounting
            {_Dir, _Seq, Send, Recv, _Content} ->
                %% lager:info("send ~p recv ~p cont ~p", [Send, Recv, _Content]),
                case maps:find(Label, Spans) of
                    error ->
                        lager:error("missing span for subspan"),
                        Spans;
                    {ok, Span} ->
                        case maps:find({Send, Recv}, Span) of
                            {ok, {Name, Start}} ->
                                %% we're the recv side
                                Spans#{Label :=
                                           Span#{{Send, Recv} :=
                                                     {Name, Start, TStamp}}};
                            error ->
                                %% need to not do this if subspan
                                %% already named?
                                {M, _F, _A} = proc_lib:translate_initial_call(Recv),
                                %% we're the send side
                                Spans#{Label :=
                                           Span#{{Recv, Send} => {M, TStamp}}}
                        end
                end
        end,
    {noreply, S?S{spans = Spans1}};
handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
