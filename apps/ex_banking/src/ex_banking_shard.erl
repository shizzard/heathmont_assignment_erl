-module(ex_banking_shard).
-dialyzer({nowarn_function, [init/1, terminate/2, code_change/3]}).
-behaviour(gen_server).

-include_lib("fitter/include/fitter_specs_gen_server.hrl").

-export([
    shard_id_to_atom/1,
    create_user/2, get_balance/3, plan_deposit/4, plan_withdraw/4, commit/4, rollback/4
]).
-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(state, {
    ets :: ets:tid()
}).
-type state() :: #state{}.

-define(create_user(UserId), {create_user, UserId}).
-define(get_balance(UserId, Currency), {get_balance, UserId, Currency}).
-define(plan_deposit(UserId, Amount, Currency), {plan_deposit, UserId, Amount, Currency}).
-define(plan_withdraw(UserId, Amount, Currency), {plan_withdraw, UserId, Amount, Currency}).
-define(commit(UserId, Currency, Operation), {commit, UserId, Currency, Operation}).
-define(rollback(UserId, Currency, Operation), {rollback, UserId, Currency, Operation}).



%% Interface



-spec shard_id_to_atom(
    ShardId :: pos_integer()
) ->
    Ret :: atom().

shard_id_to_atom(ShardId)
when is_integer(ShardId)
andalso ShardId > 0 ->
    list_to_atom(lists:flatten(io_lib:format("ex_banking_shard-~p", [ShardId]))).



-spec create_user(
    Shard :: atom(),
    UserId :: ex_banking_user:id()
) ->
    fitter:generic_return(
        ErrorRet ::
            ex_banking:error_wrong_arguments() |
            ex_banking:error_user_already_exists()
    ).

create_user(Shard, UserId) ->
    gen_server:call(Shard, ?create_user(UserId)).



-spec get_balance(
    Shard :: atom(),
    UserId :: ex_banking_user:id(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        OkRet :: ex_banking_account:amount(),
        ErrorRet ::
            ex_banking:error_wrong_arguments() |
            ex_banking:error_user_does_not_exist()
    ).

get_balance(Shard, UserId, Currency) ->
    gen_server:call(Shard, ?get_balance(UserId, Currency)).



-spec plan_deposit(
    Shard :: atom(),
    UserId :: ex_banking_user:id(),
    Amount :: ex_banking_account:amount(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        OkRet :: ex_banking_operation:operation(),
        ErrorRet ::
            ex_banking:error_wrong_arguments() |
            ex_banking:error_user_does_not_exist() |
            ex_banking:too_many_requests_to_user()
    ).

plan_deposit(Shard, UserId, Amount, Currency) ->
    gen_server:call(Shard, ?plan_deposit(UserId, Amount, Currency)).



-spec plan_withdraw(
    Shard :: atom(),
    UserId :: ex_banking_user:id(),
    Amount :: ex_banking_account:amount(),
    Currency :: ex_banking_account:currency()
) ->
    fitter:generic_return(
        OkRet :: ex_banking_operation:operation(),
        ErrorRet ::
            ex_banking:error_wrong_arguments() |
            ex_banking:error_user_does_not_exist() |
            ex_banking:error_not_enough_money() |
            ex_banking:too_many_requests_to_user()
    ).

plan_withdraw(Shard, UserId, Amount, Currency) ->
    gen_server:call(Shard, ?plan_withdraw(UserId, Amount, Currency)).



-spec commit(
    Shard :: atom(),
    UserId :: ex_banking_user:id(),
    Currency :: ex_banking_account:currency(),
    Operation :: ex_banking_operation:operation()
) ->
    fitter:generic_return(
        ErrorRet ::
            ex_banking:error_wrong_arguments() |
            ex_banking:error_user_does_not_exist() |
            ex_banking_account:error_commit_failed()
    ).

commit(Shard, UserId, Currency, Operation) ->
    gen_server:call(Shard, ?commit(UserId, Currency, Operation)).



-spec rollback(
    Shard :: atom(),
    UserId :: ex_banking_user:id(),
    Currency :: ex_banking_account:currency(),
    Operation :: ex_banking_operation:operation()
) ->
    fitter:generic_return(
        ErrorRet ::
            ex_banking:error_wrong_arguments() |
            ex_banking:error_user_does_not_exist() |
            ex_banking_account:error_rollback_failed()
    ).

rollback(Shard, UserId, Currency, Operation) ->
    gen_server:call(Shard, ?rollback(UserId, Currency, Operation)).



-spec start_link(
    ShardId :: non_neg_integer() | term()
) ->
    fitter:generic_return(
        OkRet :: pid(),
        ErrorRet :: term()
    ) | ignore.

start_link(ShardId) ->
    gen_server:start_link({local, shard_id_to_atom(ShardId)}, ?MODULE, [], []).



init(_) ->
    Ets = ets:new(?MODULE, [protected, set, {keypos, 2}]),
    {ok, #state{
        ets = Ets
    }}.



handle_call(?create_user(UserId), _GenReplyTo, #state{ets = Ets} = S0) ->
    safe_apply(
        UserId, false,
        fun(_User) ->
            {error, user_already_exists}
        end,
        fun() ->
            true = ets:insert(Ets, ex_banking_user:new(UserId)),
            ok
        end, S0
    );

handle_call(?get_balance(UserId, Currency), _GenReplyTo, #state{ets = _Ets} = S0) ->
    safe_apply(
        UserId, false,
        fun(User) ->
            {ok, integer_to_amount(ex_banking_user:get_balance(User, Currency))}
        end,
        fun() ->
            {error, user_does_not_exist}
        end, S0
    );

handle_call(?plan_deposit(UserId, FloatingPointAmount, Currency), _GenReplyTo, #state{ets = Ets} = S0) ->
    safe_apply(
        UserId, true,
        fun(User) ->
            Amount = amount_to_integer(FloatingPointAmount),
            {ok, {Operation, User1}} = ex_banking_user:plan_deposit(User, Currency, Amount),
            true = ets:insert(Ets, User1),
            {ok, Operation}
        end,
        fun() ->
            {error, user_does_not_exist}
        end, S0
    );

handle_call(?plan_withdraw(UserId, FloatingPointAmount, Currency), _GenReplyTo, #state{ets = Ets} = S0) ->
    safe_apply(
        UserId, true,
        fun(User) ->
            Amount = amount_to_integer(FloatingPointAmount),
            case ex_banking_user:plan_withdraw(User, Currency, Amount) of
                {ok, {Operation, User1}} ->
                    ets:insert(Ets, User1),
                    {ok, Operation};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        fun() ->
            {error, user_does_not_exist}
        end, S0
    );

handle_call(?commit(UserId, Currency, Operation), _GenReplyTo, #state{ets = Ets} = S0) ->
    safe_apply(
        UserId, false,
        fun(User) ->
            case ex_banking_user:commit(User, Currency, Operation) of
                {ok, User1} ->
                    ets:insert(Ets, User1),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        fun() ->
            {error, user_does_not_exist}
        end, S0
    );

handle_call(?rollback(UserId, Currency, Operation), _GenReplyTo, #state{ets = Ets} = S0) ->
    safe_apply(
        UserId, false,
        fun(User) ->
            case ex_banking_user:rollback(User, Currency, Operation) of
                {ok, User1} ->
                    ets:insert(Ets, User1),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        fun() ->
            {error, user_does_not_exist}
        end, S0
    );

handle_call(Unexpected, _GenReplyTo, S0) ->
    lager:warning("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    lager:warning("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    lager:warning("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec safe_apply(
    UserId :: ex_banking_user:id(),
    DoCheckOperationsCount :: boolean(),
    IfExistsFun :: fun((User :: ex_banking_user:user()) -> term()),
    IfDoesNotExistFun :: fun(() -> term()),
    S0 :: state()
) ->
    fitter:gen_server_reply_simple(
        ReplyT :: fitter:generic_return(
            OkRet :: term(),
            ErrorRet :: ex_banking:error()
        ),
        State :: state()
    ).

safe_apply(UserId, DoCheckOperationsCount, IfExistsFun, IfDoesNotExistFun, #state{ets = Ets} = S0) ->
    try
        case ets:lookup(Ets, UserId) of
            [] ->
                {reply, IfDoesNotExistFun(), S0};
            [User] ->
                case {ex_banking_user:get_operations_count(User), DoCheckOperationsCount} of
                    {TooMuch, true} when TooMuch >= 10 ->
                        lager:info("Too many requests to user ~p", [User]),
                        {reply, {error, too_many_requests_to_user}, S0};
                    {_N, _} ->
                        {reply, IfExistsFun(User), S0}
                end
        end
    catch
        error:function_clause ->
            {reply, {error, wrong_arguments}, S0};
        throw:bad_amount ->
            {reply, {error, wrong_arguments}, S0};
        _:_ ->
            {reply, {error, operation_failed}, S0}
    end.



-spec amount_to_integer(
    FloatingPointAmount :: number()
) ->
    Ret :: integer().

amount_to_integer(FloatingPointAmount) ->
    erlang:floor(FloatingPointAmount * 100).



-spec integer_to_amount(
    Amount :: pos_integer()
) ->
    Ret :: float().

integer_to_amount(Amount) ->
    Amount / 100.
