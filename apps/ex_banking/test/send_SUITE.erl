-module(send_SUITE).

-compile([export_all, nowarn_export_all, return_warnings]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


all() ->
    [
        can_send_test,
        can_get_user_does_not_exist_on_undefined_user,
        can_floor_float_value_on_send_test,
        can_get_not_enough_money_on_insufficient_funds,
        can_get_wrong_arguments_on_negative_send_test,
        can_get_wrong_arguments_on_invalid_currency_send_test,
        can_get_too_many_requests_to_sender,
        can_get_too_many_requests_to_receiver
    ].



init_per_suite(Config) ->
    application:set_env(ex_banking, shards_count, 8, [{persistent, true}]),
    application:start(ex_banking),
    Config.



init_per_testcase(_, Config) ->
    UserId1 = base64:encode(crypto:strong_rand_bytes(8)),
    UserId2 = base64:encode(crypto:strong_rand_bytes(8)),
    ok = ex_banking:create_user(UserId1),
    ok = ex_banking:create_user(UserId2),
    {ok, 100.0} = ex_banking:deposit(UserId1, 100.0, <<"USD">>),
    {ok, 100.0} = ex_banking:deposit(UserId2, 100.0, <<"EUR">>),
    [{user1, UserId1}, {user2, UserId2} | Config].



end_per_testcase(_, Config) ->
    Config.



end_per_suite(Config) ->
    application:stop(ex_banking),
    Config.



can_send_test(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    {ok, 10.5, 89.5} = ex_banking:send(User1, User2, 89.5, <<"USD">>),
    {ok, 10.5} = ex_banking:get_balance(User1, <<"USD">>),
    {ok, 89.5} = ex_banking:get_balance(User2, <<"USD">>),
    {ok, 50.5, 49.5} = ex_banking:send(User2, User1, 49.5, <<"EUR">>),
    {ok, 50.5} = ex_banking:get_balance(User2, <<"EUR">>),
    {ok, 49.5} = ex_banking:get_balance(User1, <<"EUR">>).



can_get_user_does_not_exist_on_undefined_user(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    {error, sender_does_not_exist} = ex_banking:send(base64:encode(crypto:strong_rand_bytes(8)), User1, 1.12, <<"USD">>),
    {error, receiver_does_not_exist} = ex_banking:send(User2, base64:encode(crypto:strong_rand_bytes(8)), 1.12, <<"EUR">>).



can_floor_float_value_on_send_test(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    {ok, 10.5, 89.5} = ex_banking:send(User1, User2, 89.509, <<"USD">>),
    {ok, 10.5} = ex_banking:get_balance(User1, <<"USD">>),
    {ok, 89.5} = ex_banking:get_balance(User2, <<"USD">>),
    {ok, 50.5, 49.5} = ex_banking:send(User2, User1, 49.509, <<"EUR">>),
    {ok, 50.5} = ex_banking:get_balance(User2, <<"EUR">>),
    {ok, 49.5} = ex_banking:get_balance(User1, <<"EUR">>).



can_get_not_enough_money_on_insufficient_funds(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    {error, not_enough_money} = ex_banking:send(User2, User1, 89.509, <<"USD">>),
    {ok, 100.0} = ex_banking:get_balance(User1, <<"USD">>),
    {ok, 0.0} = ex_banking:get_balance(User2, <<"USD">>).



can_get_wrong_arguments_on_negative_send_test(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    {error, wrong_arguments} = ex_banking:send(User1, User2, -1.5, <<"USD">>),
    {ok, 100.0} = ex_banking:get_balance(User1, <<"USD">>),
    {ok, 0.0} = ex_banking:get_balance(User2, <<"USD">>).



can_get_wrong_arguments_on_invalid_currency_send_test(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    {error, wrong_arguments} = ex_banking:send(User1, User2, 11.5, "USD").



can_get_too_many_requests_to_sender(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    User1Shard = ex_banking:get_shard_by_user_id(User1),
    _ = [
        {ok, _Op} = ex_banking_shard:plan_deposit(User1Shard, User1, 0.1, <<"USD">>)
        || _ <- lists:seq(1,10)
    ],
    {error, too_many_requests_to_sender} = ex_banking:send(User1, User2, 1.12, <<"USD">>).



can_get_too_many_requests_to_receiver(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    User2Shard = ex_banking:get_shard_by_user_id(User2),
    _ = [
        {ok, _Op} = ex_banking_shard:plan_deposit(User2Shard, User2, 0.1, <<"USD">>)
        || _ <- lists:seq(1,10)
    ],
    {error, too_many_requests_to_receiver} = ex_banking:send(User1, User2, 1.12, <<"USD">>).
