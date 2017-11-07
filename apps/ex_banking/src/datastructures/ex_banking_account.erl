-module(ex_banking_account).
-dialyzer({nowarn_function, [maybe_commit/3, maybe_rollback/3]}).

-export([
    new/1, new/2, currency/1, amount/1, operations_count/1,
    plan_deposit/2, plan_withdraw/2, commit/2, rollback/2
]).

-record(account, {
    currency :: currency(),
    amount :: amount(),
    operations = [] :: [ex_banking_operation:operation()]
}).

-type error_commit_failed() :: commit_failed.
-type error_rollback_failed() :: rollback_failed.
-type error() :: error_commit_failed() | error_rollback_failed().

-export_type([error_commit_failed/0, error_rollback_failed/0, error/0]).

-type currency() :: binary().
-type amount() :: non_neg_integer().
-opaque account() :: #account{}.

-export_type([currency/0, amount/0, account/0]).



%% Interface



-spec new(
    Currency :: currency()
) ->
    Ret :: account().

new(Currency) when is_binary(Currency) ->
    new(Currency, 0).



-spec new(
    Currency :: currency(),
    Amount :: amount()
) ->
    Ret :: account().

new(Currency, Amount)
when is_binary(Currency)
andalso is_integer(Amount)
andalso Amount >= 0 ->
    #account{currency = Currency, amount = Amount}.



-spec currency(
    Account :: account()
) ->
    Ret :: currency().

currency(#account{currency = Currency}) ->
    Currency.



-spec amount(
    Account :: account()
) ->
    Ret :: amount().

amount(#account{amount = Amount}) ->
    Amount.



-spec operations_count(
    Account :: account()
) ->
    Ret :: non_neg_integer().

operations_count(#account{operations = Operations}) ->
    length(Operations).



-spec plan_deposit(
    Account :: account(),
    OperationAmount :: amount()
) ->
    fitter:ok_return(
        OkRet :: {Operation :: ex_banking_operation:operation(), Account :: account()}
    ).

plan_deposit(#account{} = Account, OperationAmount)
when is_integer(OperationAmount)
andalso OperationAmount >= 0 ->
    maybe_plan_deposit(Account, OperationAmount).



-spec plan_withdraw(
    Account :: account(),
    OperationAmount :: amount()
) ->
    fitter:generic_return(
        OkRet :: {Operation :: ex_banking_operation:operation(), Account :: account()},
        ErrorRet :: ex_banking:error_not_enough_money()
    ).

plan_withdraw(#account{} = Account, OperationAmount)
when is_integer(OperationAmount)
andalso OperationAmount >= 0 ->
    maybe_plan_withdraw(Account, OperationAmount).



-spec commit(
    Account :: account(),
    Operation :: ex_banking_operation:operation()
) ->
    fitter:generic_return(
        OkRet :: account(),
        ErrorRet :: error_commit_failed()
    ).

commit(#account{} = Account, Operation) ->
    maybe_commit(
        Account,
        ex_banking_operation:id(Operation),
        ex_banking_operation:type(Operation)
    ).



-spec rollback(
    Account :: account(),
    Operation :: ex_banking_operation:operation()
) ->
    fitter:generic_return(
        OkRet :: account(),
        ErrorRet :: error_rollback_failed()
    ).

rollback(#account{} = Account, Operation) ->
    maybe_rollback(
        Account,
        ex_banking_operation:id(Operation),
        ex_banking_operation:type(Operation)
    ).



%% Internals



-spec maybe_plan_deposit(
    Account :: account(),
    OperationAmount :: amount()
) ->
    fitter:ok_return(
        OkRet :: {Operation :: ex_banking_operation:operation(), Account :: account()}
    ).

maybe_plan_deposit(#account{operations = Operations} = Account, OperationAmount) ->
    Operation = ex_banking_operation:new(deposit, OperationAmount),
    {ok, {Operation, Account#account{
        operations = [Operation | Operations]
    }}}.



-spec maybe_plan_withdraw(
    Account :: account(),
    OperationAmount :: amount()
) ->
    fitter:generic_return(
        OkRet :: {Operation :: ex_banking_operation:operation(), Account :: account()},
        ErrorRet :: ex_banking:error_not_enough_money()
    ).

maybe_plan_withdraw(#account{amount = Amount, operations = Operations} = Account, OperationAmount)
when Amount >= OperationAmount ->
    Operation = ex_banking_operation:new(withdraw, OperationAmount),
    {ok, {Operation, Account#account{
        amount = Amount - OperationAmount,
        operations = [Operation | Operations]
    }}};

maybe_plan_withdraw(_Account, _OperationAmount) ->
    {error, not_enough_money}.



-spec maybe_commit(
    Account :: account(),
    OperationId :: ex_banking_operation:id(),
    Type :: ex_banking_operation:type()
) ->
    fitter:generic_return(
        OkRet :: account(),
        ErrorRet :: error_commit_failed()
    ).

maybe_commit(#account{} = Account, OperationId, deposit) ->
    maybe_add_operation_amount(Account, OperationId, commit_failed);

maybe_commit(#account{} = Account, OperationId, withdraw) ->
    maybe_drop_operation(Account, OperationId, commit_failed).



-spec maybe_rollback(
    Account :: account(),
    OperationId :: ex_banking_operation:id(),
    Type :: ex_banking_operation:type()
) ->
    fitter:generic_return(
        OkRet :: account(),
        ErrorRet :: error_rollback_failed()
    ).

maybe_rollback(#account{} = Account, OperationId, deposit) ->
    maybe_drop_operation(Account, OperationId, rollback_failed);

maybe_rollback(#account{} = Account, OperationId, withdraw) ->
    maybe_add_operation_amount(Account, OperationId, rollback_failed).



-spec maybe_drop_operation(
    Account :: account(),
    OperationId :: ex_banking_operation:id(),
    ErrorCode :: error()
) ->
    fitter:generic_return(
        OkRet :: account(),
        ErrorRet :: error()
    ).

maybe_drop_operation(#account{operations = Operations0} = Account, OperationId, ErrorCode) ->
    Predicate = match_operation_id_predicate(OperationId),
    case lists:partition(Predicate, Operations0) of
        {[], Operations0} ->
            {error, ErrorCode};
        {[_Operation], Operations1} ->
            {ok, Account#account{
                operations = Operations1
            }}
    end.



-spec maybe_add_operation_amount(
    Account :: account(),
    OperationId :: ex_banking_operation:id(),
    ErrorCode :: error()
) ->
    fitter:generic_return(
        OkRet :: account(),
        ErrorRet :: error()
    ).

maybe_add_operation_amount(#account{amount = Amount, operations = Operations0} = Account, OperationId, ErrorCode) ->
    Predicate = match_operation_id_predicate(OperationId),
    case lists:partition(Predicate, Operations0) of
        {[], Operations0} ->
            {error, ErrorCode};
        {[Operation], Operations1} ->
            {ok, Account#account{
                amount = Amount + ex_banking_operation:amount(Operation),
                operations = Operations1
            }}
    end.



-spec match_operation_id_predicate(
    OperationId :: ex_banking_operation:id()
) ->
    fun(
        (Operation :: ex_banking_operation:operation()) -> boolean()
    ).

match_operation_id_predicate(OperationId) ->
    fun(Operation) ->
        OperationId == ex_banking_operation:id(Operation)
    end.
