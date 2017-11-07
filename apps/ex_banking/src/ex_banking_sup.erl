-module(ex_banking_sup).
-behaviour(supervisor).
-dialyzer({nowarn_function, init/1}).
-include_lib("fitter/include/fitter_specs_supervisor.hrl").

-export([start_link/0, init/1]).



-spec start_link() ->
    fitter:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ) | ignore.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.

