-module(withdraw_SUITE).

-compile([export_all, nowarn_export_all, return_warnings]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


all() ->
    [
        can_withdraw_test,
        can_get_user_does_not_exist_on_undefined_user,
        can_floor_float_value_on_withdraw_test,
        can_get_not_enough_money_on_insufficient_funds,
        can_get_wrong_arguments_on_negative_withdraw_test,
        can_get_wrong_arguments_on_invalid_currency_withdraw_test
    ].



init_per_suite(Config) ->
    application:set_env(ex_banking, shards_count, 8, [{persistent, true}]),
    application:start(ex_banking),
    Config.



init_per_testcase(_, Config) ->
    UserId = base64:encode(crypto:strong_rand_bytes(8)),
    ok = ex_banking:create_user(UserId),
    {ok, 100.0} = ex_banking:deposit(UserId, 100.0, <<"USD">>),
    [{user1, UserId} | Config].



end_per_testcase(_, Config) ->
    Config.



end_per_suite(Config) ->
    application:stop(ex_banking),
    Config.



can_withdraw_test(Config) ->
    User = proplists:get_value(user1, Config),
    {ok, 10.5} = ex_banking:withdraw(User, 89.5, <<"USD">>),
    {ok, 10.5} = ex_banking:get_balance(User, <<"USD">>).



can_get_user_does_not_exist_on_undefined_user(_Config) ->
    {error, user_does_not_exist} = ex_banking:withdraw(base64:encode(crypto:strong_rand_bytes(8)), 1.12, <<"USD">>).



can_floor_float_value_on_withdraw_test(Config) ->
    User = proplists:get_value(user1, Config),
    {ok, 10.5} = ex_banking:withdraw(User, 89.509, <<"USD">>),
    {ok, 10.5} = ex_banking:get_balance(User, <<"USD">>).



can_get_not_enough_money_on_insufficient_funds(Config) ->
    User = proplists:get_value(user1, Config),
    {error, not_enough_money} = ex_banking:withdraw(User, 100.01, <<"USD">>).



can_get_wrong_arguments_on_negative_withdraw_test(Config) ->
    User = proplists:get_value(user1, Config),
    {error, wrong_arguments} = ex_banking:withdraw(User, -1.12, <<"USD">>).



can_get_wrong_arguments_on_invalid_currency_withdraw_test(Config) ->
    User = proplists:get_value(user1, Config),
    {error, wrong_arguments} = ex_banking:withdraw(User, 1.12, "USD").
