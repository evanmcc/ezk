-module(zip_example_handler).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-include_lib("ezk/include/ezk.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    ezk:start_span(?MODULE, zip_example),
    %% fake auth
    zu:pretend(),
    zauth:auth(),
    %% fake service/db call several layers deep
    zu:pretend(),
    zdb:get_junk(),
    %% eventually a fake cross-node call
    zu:pretend(),
    {ok, [], <<"Hello World!\n">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found\n">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_complete, _Data, _Args) ->
    ezk:end_span(),
    ok;
handle_event(_E, _D, _A) ->
    lager:info("XXX ~p ~p ~p", [_E, _D, _A]),
    ok.
