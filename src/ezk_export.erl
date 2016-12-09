-module(ezk_export).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         finalize/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(posix_offset, 904802900000000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

finalize(Span) ->
    gen_server:cast(?SERVER, {finalize, Span}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    rand:seed(exs1024),  % to relabel with actual 64bit values
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({finalize, Span}, State) ->
    lager:info("got a thing: ~p", [Span]),
    %% post-process subspans
    Span1 = process(Span),
    %% reformat
    Span2 = jsx:encode(Span1),
    %% send
    %% maybe replace with hackney or similar?
    %% also batch these for performance
    {ok, _} = httpc:request(post, {"http://localhost:9411/api/v1/spans", [],
                                   "application/json",
                                   Span2},
                            [], []),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

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

process(#{name := Name,
          app_name := AppName,
          start := Start,
          final := End} = Span) ->
    %% full 64 bit label, can't reuse erlang ints
    ParentLabel0 = rand:uniform(16#ffffffffffffffff),
    ParentLabel = iolist_to_binary(io_lib:format("~16.16.0b",
                                                 [ParentLabel0])),
    Duration = erlang:convert_time_unit(End - Start,
                                        nano_seconds, micro_seconds),
    StartUs = ?posix_offset - Start,
    EndUs = ?posix_offset - End,

    #{name := SvcName,
      ipv4 := Addr,
      port := Port
     } = application:get_env(ezk, AppName,
                             #{name => <<"undefined application">>,
                               ipv4 => <<"127.0.0.1">>,
                               port => 0}),

    Svc = #{<<"serviceName">> => SvcName,
            <<"ipv4">> => Addr,
            <<"port">> => Port},

    [#{<<"traceId">> => ParentLabel,
       <<"name">> => Name,
       <<"id">> => ParentLabel,
       <<"timestamp">> => erlang:system_time(micro_seconds),
       <<"duration">> => Duration,
       <<"annotations">> =>
           [#{<<"endpoint">> => Svc,
              <<"timestamp">> => StartUs,
              <<"value">> => <<"sr">>},
            #{<<"endpoint">> => Svc,
              <<"timestamp">> => EndUs,
              <<"value">> => <<"ss">>}
           ],
      <<"binaryAnnoations">> => []}] ++
        subspans(Span).

subspans(_) ->
    [].
