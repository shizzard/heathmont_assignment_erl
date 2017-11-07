-module(ex_banking).

-export([get_shard_by_user_id/1, create_user/1, deposit/3, withdraw/3, get_balance/2, send/4]).

-type error_wrong_arguments() :: wrong_arguments.
-type error_user_already_exists() :: user_already_exists.
-type error_user_does_not_exist() :: user_does_not_exist.
-type error_not_enough_money() :: not_enough_money.
-type error_sender_does_not_exist() :: sender_does_not_exist.
-type error_receiver_does_not_exist() :: receiver_does_not_exist.
-type error_too_many_requests_to_user() :: too_many_requests_to_user.
-type error_too_many_requests_to_sender() :: too_many_requests_to_sender.
-type error_too_many_requests_to_receiver() :: too_many_requests_to_receiver.
-type error_operation_failed() :: operation_failed.
-type error() ::
    error_wrong_arguments() |
    error_user_already_exists() |
    error_user_does_not_exist() |
    error_not_enough_money() |
    error_sender_does_not_exist() |
    error_receiver_does_not_exist() |
    error_too_many_requests_to_user() |
    error_too_many_requests_to_sender() |
    error_too_many_requests_to_receiver() |
    error_operation_failed().

-export_type([
    error_wrong_arguments/0,
    error_user_already_exists/0,
    error_user_does_not_exist/0,
    error_not_enough_money/0,
    error_sender_does_not_exist/0,
    error_receiver_does_not_exist/0,
    error_too_many_requests_to_user/0,
    error_too_many_requests_to_sender/0,
    error_too_many_requests_to_receiver/0,
    error_operation_failed/0,
    error/0
]).



%% Interface



-spec get_shard_by_user_id(
    UserId :: ex_banking_user:id() | term()
) ->
    Ret :: atom().

get_shard_by_user_id(UserId) ->
    {ok, ShardsCount} = application:get_env(ex_banking, shards_count),
    ShardId = (erlang:crc32(term_to_binary(UserId)) rem ShardsCount) + 1,
    ex_banking_shard:shard_id_to_atom(ShardId).



-spec create_user(
    UserId :: ex_banking_user:id()
) ->
    fitter:generic_return(
        ErrorRet ::
            error_wrong_arguments() |
            error_user_already_exists()
    ).

create_user(UserId) ->
    Shard = get_shard_by_user_id(UserId),
    ex_banking_shard:create_user(Shard, UserId).



-spec deposit(
    UserId :: ex_banking_user:id(),
    Amount :: ex_banking_account:amount(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        NewBalance :: non_neg_integer(),
        ErrorRet ::
            error_wrong_arguments() |
            error_user_does_not_exist()
    ).

deposit(UserId, Amount, Currency) ->
    Shard = get_shard_by_user_id(UserId),
    case ex_banking_shard:plan_deposit(Shard, UserId, Amount, Currency) of
        {ok, Operation} ->
            ok = ex_banking_shard:commit(Shard, UserId, Currency, Operation),
            {ok, Amount1} = ex_banking_shard:get_balance(Shard, UserId, Currency),
            {ok, Amount1};
        {error, Reason} ->
            {error, Reason}
    end.



-spec withdraw(
    UserId :: ex_banking_user:id(),
    Amount :: ex_banking_account:amount(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        NewBalance :: non_neg_integer(),
        ErrorRet ::
            error_wrong_arguments() |
            error_user_does_not_exist() |
            error_not_enough_money()
    ).

withdraw(UserId, Amount, Currency) ->
    Shard = get_shard_by_user_id(UserId),
    case ex_banking_shard:plan_withdraw(Shard, UserId, Amount, Currency) of
        {ok, Operation} ->
            ok = ex_banking_shard:commit(Shard, UserId, Currency, Operation),
            {ok, Amount1} = ex_banking_shard:get_balance(Shard, UserId, Currency),
            {ok, Amount1};
        {error, Reason} ->
            {error, Reason}
    end.



-spec get_balance(
    UserId :: ex_banking_user:id(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        Balance :: non_neg_integer(),
        ErrorRet ::
            error_wrong_arguments() |
            error_user_does_not_exist()
    ).

get_balance(UserId, Currency) ->
    Shard = get_shard_by_user_id(UserId),
    ex_banking_shard:get_balance(Shard, UserId, Currency).


-spec send(
    FromUser :: ex_banking_user:id(),
    ToUser :: ex_banking_user:id(),
    Amount :: ex_banking_account:amount(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        FromUserBalance :: non_neg_integer(),
        ToUserBalance :: non_neg_integer(),
        ErrorRet ::
            error_wrong_arguments() |
            error_sender_does_not_exist() |
            error_receiver_does_not_exist() |
            error_too_many_requests_to_sender() |
            error_too_many_requests_to_receiver() |
            error_not_enough_money() |
            error_operation_failed()
    ).

send(FromUserId, ToUserId, Amount, Currency) ->
    do_send_steps(FromUserId, ToUserId, Amount, Currency).



%% Internals



do_send_steps(FromUserId, ToUserId, Amount, Currency) ->
    FromShard = get_shard_by_user_id(FromUserId),
    ToShard = get_shard_by_user_id(ToUserId),
    do_send_step_1_withdraw(FromShard, FromUserId, ToShard, ToUserId, Amount, Currency).



do_send_step_1_withdraw(FromShard, FromUserId, ToShard, ToUserId, Amount, Currency) ->
    case ex_banking_shard:plan_withdraw(FromShard, FromUserId, Amount, Currency) of
        {ok, FromOperation} ->
            do_send_step_2_deposit(FromShard, FromUserId, FromOperation, ToShard, ToUserId, Amount, Currency);
        {error, user_does_not_exist} ->
            {error, sender_does_not_exist};
        {error, too_many_requests_to_user} ->
            {error, too_many_requests_to_sender};
        {error, Reason} ->
            {error, Reason}
    end.



do_send_step_2_deposit(FromShard, FromUserId, FromOperation, ToShard, ToUserId, Amount, Currency) ->
    case ex_banking_shard:plan_deposit(ToShard, ToUserId, Amount, Currency) of
        {ok, ToOperation} ->
            do_send_step_3_commit(
                FromShard, FromUserId, FromOperation, ToShard, ToUserId, ToOperation, Amount, Currency);
        {error, user_does_not_exist} ->
            do_send_step_3_rollback(
                FromShard, FromUserId, FromOperation, ToShard, ToUserId, receiver_does_not_exist, Amount, Currency);
        {error, too_many_requests_to_user} ->
            do_send_step_3_rollback(
                FromShard, FromUserId, FromOperation, ToShard, ToUserId, too_many_requests_to_receiver, Amount, Currency);
        {error, Reason} ->
            do_send_step_3_rollback(
                FromShard, FromUserId, FromOperation, ToShard, ToUserId, Reason, Amount, Currency)
    end.



do_send_step_3_commit(FromShard, FromUserId, FromOperation, ToShard, ToUserId, ToOperation, Amount, Currency) ->
    try
        ok = ex_banking_shard:commit(FromShard, FromUserId, Currency, FromOperation),
        ok = ex_banking_shard:commit(ToShard, ToUserId, Currency, ToOperation),
        do_send_step_4_amounts(FromShard, FromUserId, ToShard, ToUserId, Currency)
    catch
        Type:Reason ->
            lager:alert(
                "Failed to commit operation [~p ~p] for users ~p -> ~p, because of ~p:~p",
                [Currency, Amount, FromUserId, ToUserId, Type, Reason]
            ),
            {error, operation_failed}
    end.



do_send_step_3_rollback(FromShard, FromUserId, FromOperation, _ToShard, ToUserId, Reason, Amount, Currency) ->
    try
        ok = ex_banking_shard:rollback(FromShard, FromUserId, Currency, FromOperation),
        {error, Reason}
    catch
        Type:Reason ->
            lager:alert(
                "Failed to rollback operation [~p ~p] for user ~p (-> ~p), because of ~p:~p",
                [Currency, Amount, FromUserId, ToUserId, Type, Reason]
            ),
            {error, operation_failed}
    end.



do_send_step_4_amounts(FromShard, FromUserId, ToShard, ToUserId, Currency) ->
    {ok, FromAmount} = ex_banking_shard:get_balance(FromShard, FromUserId, Currency),
    {ok, ToAmount} = ex_banking_shard:get_balance(ToShard, ToUserId, Currency),
    {ok, FromAmount, ToAmount}.
