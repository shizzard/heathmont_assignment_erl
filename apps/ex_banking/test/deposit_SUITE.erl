-module(deposit_SUITE).

-compile([export_all, nowarn_export_all, return_warnings]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


all() ->
    [
        can_deposit_test,
        can_get_user_does_not_exist_on_undefined_user,
        can_floor_float_value_on_deposit_test,
        can_get_wrong_arguments_on_negative_deposit_test,
        can_get_wrong_arguments_on_invalid_currency_deposit_test
    ].



init_per_suite(Config) ->
    application:set_env(ex_banking, shards_count, 8, [{persistent, true}]),
    application:start(ex_banking),
    Config.



init_per_testcase(_, Config) ->
    UserId = base64:encode(crypto:strong_rand_bytes(8)),
    ok = ex_banking:create_user(UserId),
    [{user1, UserId} | Config].



end_per_testcase(_, Config) ->
    Config.



end_per_suite(Config) ->
    application:stop(ex_banking),
    Config.



can_deposit_test(Config) ->
    User = proplists:get_value(user1, Config),
    {ok, 1.12} = ex_banking:deposit(User, 1.12, <<"USD">>),
    {ok, 1.12} = ex_banking:get_balance(User, <<"USD">>).



can_get_user_does_not_exist_on_undefined_user(_Config) ->
    {error, user_does_not_exist} = ex_banking:deposit(base64:encode(crypto:strong_rand_bytes(8)), 1.12, <<"USD">>).



can_floor_float_value_on_deposit_test(Config) ->
    User = proplists:get_value(user1, Config),
    {ok, 1.12} = ex_banking:deposit(User, 1.129, <<"USD">>),
    {ok, 1.12} = ex_banking:get_balance(User, <<"USD">>).



can_get_wrong_arguments_on_negative_deposit_test(Config) ->
    User = proplists:get_value(user1, Config),
    {error, wrong_arguments} = ex_banking:deposit(User, -1.12, <<"USD">>).



can_get_wrong_arguments_on_invalid_currency_deposit_test(Config) ->
    User = proplists:get_value(user1, Config),
    {error, wrong_arguments} = ex_banking:deposit(User, 1.12, "USD").
