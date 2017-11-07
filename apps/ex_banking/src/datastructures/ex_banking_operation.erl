-module(ex_banking_operation).

-export([new/2, id/1, type/1, amount/1]).

-record(operation, {
    id :: id(),
    type :: type(),
    amount :: ex_banking_account:amount()
}).

-type id() :: reference().
-type type_deposit() :: deposit.
-type type_withdraw() :: withdraw.
-type type() :: type_deposit() | type_withdraw().
-opaque operation() :: #operation{}.

-export_type([id/0, operation/0]).



%% Interface



-spec new(
    Type :: type(),
    Amount :: ex_banking_account:amount()
) ->
    Ret :: operation().

new(Type, Amount)
when (Type == deposit orelse Type == withdraw)
andalso is_integer(Amount)
andalso Amount >= 0 ->
    #operation{id = erlang:make_ref(), type = Type, amount = Amount}.



-spec id(
    Operation :: operation()
) ->
    Ret :: id().

id(#operation{id = Id}) ->
    Id.



-spec type(
    Operation :: operation()
) ->
    Ret :: type().

type(#operation{type = Type}) ->
    Type.



-spec amount(
    Operation :: operation()
) ->
    Ret :: ex_banking_account:amount().

amount(#operation{amount = Amount}) ->
    Amount.
