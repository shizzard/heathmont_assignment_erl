-module(ex_banking_app).
-behaviour(application).
-dialyzer({nowarn_function, stop/1}).
-include_lib("fitter/include/fitter_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    ex_banking_sup:start_link().



stop(_State) ->
    ok.
