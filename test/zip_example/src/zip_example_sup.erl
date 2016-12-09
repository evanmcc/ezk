%%%-------------------------------------------------------------------
%% @doc zip_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zip_example_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ElliOpts = [{callback, zip_example_handler}, {port, 8080}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    AChild = #{id => zauth,
               start => {zauth, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [zauth]},
    AChild2 = #{id => zdb,
                start => {zdb, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [zdb]},
    AChild3 = #{id => zdb_aux,
                start => {zdb_aux, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [zdb_aux]},
    {ok, { {one_for_all, 0, 1}, [ElliSpec, AChild, AChild2, AChild3]} }.

%%====================================================================
%% Internal functions
%%====================================================================
