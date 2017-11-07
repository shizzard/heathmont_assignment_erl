-module(ex_banking).

-export([create_user/1, deposit/3, withdraw/3, get_balance/2, send/4]).

-type error_wrong_arguments() :: wrong_arguments.
-type error_user_already_exists() :: user_already_exists.
-type error_user_does_not_exist() :: user_does_not_exist.
-type error_not_enough_money() :: not_enough_money.
-type error_sender_does_not_exist() :: sender_does_not_exist.
-type error_receiver_does_not_exist() :: receiver_does_not_exist.
-type error_too_many_requests_to_user() :: too_many_requests_to_user.
-type error_too_many_requests_to_sender() :: too_many_requests_to_sender.
-type error_too_many_requests_to_receiver() :: too_many_requests_to_receiver.
-type error() ::
    error_wrong_arguments() |
    error_user_already_exists() |
    error_user_does_not_exist() |
    error_not_enough_money() |
    error_sender_does_not_exist() |
    error_receiver_does_not_exist() |
    error_too_many_requests_to_user() |
    error_too_many_requests_to_sender() |
    error_too_many_requests_to_receiver().

-type safe(T) :: T | term().

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
    error/0,
    safe/1
]).



%% Interface



-spec create_user(
    User :: safe(string())
) ->
    fitter:generic_return(
        ErrorRet :: ex_banking:error()
    ).

create_user(_User) ->
    ok.



-spec deposit(
    User :: safe(string()),
    Amount :: safe(non_neg_integer()),
    Currency :: safe(string())
) ->
    fitter:generic_return(
        NewBalance :: non_neg_integer(),
        ErrorRet :: ex_banking:error()
    ).

deposit(_user, _amount, _currency) ->
    {ok, 0}.



-spec withdraw(
    User :: safe(string()),
    Amount :: safe(non_neg_integer()),
    Currency :: safe(string())
) ->
    fitter:generic_return(
        NewBalance :: non_neg_integer(),
        ErrorRet :: ex_banking:error()
    ).

withdraw(_user, _amount, _currency) ->
    {ok, 0}.



-spec get_balance(
    User :: safe(string()),
    Currency :: safe(string())
) ->
    fitter:generic_return(
        Balance :: non_neg_integer(),
        ErrorRet :: ex_banking:error()
    ).

get_balance(_user, _currency) ->
    {ok, 0}.



-spec send(
    FromUser :: safe(string()),
    ToUser :: safe(string()),
    Amount :: safe(non_neg_integer()),
    Currency :: safe(string())
) ->
    fitter:generic_return(
        FromUserBalance :: non_neg_integer(),
        ToUserBalance :: non_neg_integer(),
        ErrorRet :: ex_banking:error()
    ).

send(_From, _To, _Amount, _Currency) ->
    {ok, 0, 0}.
