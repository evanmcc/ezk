%%%-------------------------------------------------------------------
%%% @author Evan Vigil-McClanahan <mcclanhan@gmail.com>
%%% @copyright (C) 2016, Evan Vigil-McClanahan
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by Evan Vigil-McClanahan <mcclanhan@gmail.com>
%%%-------------------------------------------------------------------
-module(ezk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Tracer = #{id => ezk_tracer,
               start => {ezk_tracer, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [ezk_tracer]},

    Export = #{id => ezk_export,
               start => {ezk_export, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [ezk_export]},

    {ok, {SupFlags, [Tracer, Export]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
