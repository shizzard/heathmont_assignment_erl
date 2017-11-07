-module(create_user_SUITE).

-compile([export_all, nowarn_export_all, return_warnings]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


all() ->
    [
        can_create_user_test,
        can_get_zero_amount_after_create_user_test,
        can_get_wrong_arguments_on_create_user_with_invalid_id_test
    ].



init_per_suite(Config) ->
    application:set_env(ex_banking, shards_count, 8, [{persistent, true}]),
    application:start(ex_banking),
    Config.



init_per_testcase(_, Config) ->
    Config.



end_per_testcase(_, Config) ->
    Config.



end_per_suite(Config) ->
    application:stop(ex_banking),
    Config.



can_create_user_test(_Config) ->
    ok = ex_banking:create_user(base64:encode(crypto:strong_rand_bytes(8))).



can_get_zero_amount_after_create_user_test(_Config) ->
    User = base64:encode(crypto:strong_rand_bytes(8)),
    ok = ex_banking:create_user(User),
    {ok, 0.0} = ex_banking:get_balance(User, <<"USD">>).



can_get_wrong_arguments_on_create_user_with_invalid_id_test(_Config) ->
    {error, wrong_arguments} = ex_banking:create_user(binary_to_list(base64:encode(crypto:strong_rand_bytes(8)))).
