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
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 1
    },
    ChildSpecs = [#{
        id => call,
        start => {ex_banking_shard, start_link, []},
        shutdown => 5000
    }],
    {ok, {SupFlags, ChildSpecs}}.
