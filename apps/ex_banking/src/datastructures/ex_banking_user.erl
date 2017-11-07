-module(ex_banking_user).
-dialyzer({nowarn_function, [get_account/2, set_account/3]}).

-export([
    new/1, id/1, get_balance/2, get_operations_count/1, get_operations_count/2,
    plan_deposit/3, plan_withdraw/3, commit/3, rollback/3
]).

-record(user, {
    id :: id(),
    accounts = dict:new() :: dict:dict(
        KeyT :: ex_banking_account:currency(),
        ValueT :: ex_banking_account:account()
    )
}).

-type id() :: binary().
-type user() :: #user{}.

-export_type([id/0, user/0]).



%% Interface



-spec new(
    Id :: id()
) ->
    Ret :: user().

new(Id) when is_binary(Id) ->
    #user{id = Id}.



-spec id(
    User :: user()
) ->
    Ret :: id().

id(#user{id = Id}) ->
    Id.



-spec get_balance(
    User :: user(),
    Currency :: ex_banking_account:currency()
) ->
    Ret :: ex_banking_account:amount().

get_balance(#user{accounts = Accounts}, Currency)
when is_binary(Currency) ->
    ex_banking_account:amount(get_account(Accounts, Currency)).



-spec get_operations_count(
    User :: user()
) ->
    Ret :: non_neg_integer().

get_operations_count(#user{accounts = Accounts}) ->
    dict:fold(fun(_Currency, Account, Acc) ->
        Acc + ex_banking_account:operations_count(Account)
    end, 0, Accounts).



-spec get_operations_count(
    User :: user(),
    Currency :: ex_banking_account:currency()
) ->
    Ret :: non_neg_integer().

get_operations_count(#user{accounts = Accounts}, Currency) ->
    ex_banking_account:operations_count(get_account(Accounts, Currency)).



-spec plan_deposit(
    User :: user(),
    Currency :: ex_banking_account:currency(),
    OperationAmount :: ex_banking_account:amount()
) ->
    fitter:ok_return(
        OkRet :: {Operation :: ex_banking_operation:operation(), User :: user()}
    ).

plan_deposit(#user{accounts = Accounts} = User, Currency, OperationAmount)
when is_binary(Currency)
andalso is_integer(OperationAmount)
andalso OperationAmount >= 0 ->
    {ok, {Operation, Account}} = ex_banking_account:plan_deposit(
        get_account(Accounts, Currency),
        OperationAmount
    ),
    {ok, {Operation, User#user{
        accounts = set_account(Accounts, Currency, Account)
    }}}.



-spec plan_withdraw(
    User :: user(),
    Currency :: ex_banking_account:currency(),
    OperationAmount :: ex_banking_account:amount()
) ->
    fitter:generic_return(
        OkRet :: {Operation :: ex_banking_operation:operation(), User :: user()},
        ErrorRet :: ex_banking:error_not_enough_money()
    ).

plan_withdraw(#user{accounts = Accounts} = User, Currency, OperationAmount)
when is_binary(Currency)
andalso is_integer(OperationAmount)
andalso OperationAmount >= 0 ->
    case ex_banking_account:plan_withdraw(
        get_account(Accounts, Currency),
        OperationAmount
    ) of
        {ok, {Operation, Account}} ->
            {ok, {Operation, User#user{
                accounts = set_account(Accounts, Currency, Account)
            }}};
        {error, Reason} ->
            {error, Reason}
    end.



-spec commit(
    Account :: user(),
    Currency :: ex_banking_account:currency(),
    Operation :: ex_banking_operation:operation()
) ->
    fitter:generic_return(
        OkRet :: user(),
        ErrorRet :: ex_banking_account:error_commit_failed()
    ).

commit(#user{accounts = Accounts} = User, Currency, Operation) ->
    case ex_banking_account:commit(get_account(Accounts, Currency), Operation) of
        {ok, Account} ->
            {ok, User#user{accounts = set_account(Accounts, Currency, Account)}};
        {error, Reason} ->
            {error, Reason}
    end.



-spec rollback(
    Account :: user(),
    Currency :: ex_banking_account:currency(),
    Operation :: ex_banking_operation:operation()
) ->
    fitter:generic_return(
        OkRet :: user(),
        ErrorRet :: ex_banking_account:error_rollback_failed()
    ).

rollback(#user{accounts = Accounts} = User, Currency, Operation) ->
    case ex_banking_account:rollback(get_account(Accounts, Currency), Operation) of
        {ok, Account} ->
            {ok, User#user{accounts = set_account(Accounts, Currency, Account)}};
        {error, Reason} ->
            {error, Reason}
    end.



%% Internals



-spec get_account(
    Accounts :: dict:dict(
        KeyT :: ex_banking_account:currency(),
        ValueT :: ex_banking_account:account()
    ),
    Currency :: ex_banking_account:currency()
) ->
    Ret :: ex_banking_account:account().

get_account(Accounts, Currency) ->
    case dict:find(Currency, Accounts) of
        {ok, Account} ->
            Account;
        error ->
            ex_banking_account:new(Currency)
    end.



-spec set_account(
    Accounts :: dict:dict(
        KeyT :: ex_banking_account:currency(),
        ValueT :: ex_banking_account:account()
    ),
    Currency :: ex_banking_account:currency(),
    Account :: ex_banking_account:account()
) ->
    Ret :: dict:dict(
        KeyT :: ex_banking_account:currency(),
        ValueT :: ex_banking_account:account()
    ).

set_account(Accounts, Currency, Account) ->
    dict:store(Currency, Account, Accounts).
