-module(ex_banking_app).
-behaviour(application).
-dialyzer({nowarn_function, stop/1}).
-include_lib("fitter/include/fitter_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    {ok, Sup} = ex_banking_sup:start_link(),
    {ok, ShardsCount} = application:get_env(ex_banking, shards_count),
    _ = [{ok, _Pid} = supervisor:start_child(ex_banking_sup, [ShardId]) || ShardId <- lists:seq(1, ShardsCount)],
    {ok, Sup}.



stop(_State) ->
    ok.
