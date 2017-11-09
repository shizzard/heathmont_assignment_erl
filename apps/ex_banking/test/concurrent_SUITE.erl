-module(concurrent_SUITE).

-compile([export_all, nowarn_export_all, return_warnings]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(concurrent_users, 100).
-define(concurrent_receivers, 30).
-define(concurrent_operations, 1000).
-define(initial_amount, ?concurrent_operations * 5.0).

-spec test() -> ok.


all() ->
    [
        concurrent_test
    ].



init_per_suite(Config) ->
    application:set_env(ex_banking, shards_count, 8, [{persistent, true}]),
    application:start(ex_banking),
    Config.



init_per_testcase(_, Config) ->
    %% Seeding random
    rand:seed(exs1024s, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    %% Generating users
    Users = [begin
        UserId = base64:encode(crypto:strong_rand_bytes(8)),
        ok = ex_banking:create_user(UserId),
        {ok, ?initial_amount} = ex_banking:deposit(UserId, ?initial_amount, <<"USD">>),
        UserId
    end || _ <- lists:seq(1, ?concurrent_users)],
    %% Trapping exits
    false = process_flag(trap_exit, true),
    [{users, Users} | Config].



end_per_testcase(_, Config) ->
    true = process_flag(trap_exit, false),
    Config.



end_per_suite(Config) ->
    application:stop(ex_banking),
    Config.



concurrent_test(Config) ->
    Users = proplists:get_value(users, Config),
    %% Spawning a concurrent process per user to perform random send
    lists:map(fun(UserId) ->
        spawn_link(fun() ->
            actor(
                UserId,
                lists:sublist([NeighborId || NeighborId <- Users, NeighborId =/= UserId], ?concurrent_receivers)
            )
        end)
    end, Users),
    %% Collecting exits to ensure all processes terminated
    ok = do_receive_exits(length(Users)),
    %% We want to see if we have the same amount of money in the system as before
    ExpectedAmount = ?concurrent_users * ?initial_amount,
    %% Collecting current amount
    CurrentAmount = lists:sum(lists:map(
        fun(UserId) ->
            {ok, Balance} = ex_banking:get_balance(UserId, <<"USD">>),
            Balance
        end, Users
    )),
    %% Checking with cutting out floating point numbers error (0.00000000002)
    ct:log("Expected amount: ~p, current amount: ~p", [ExpectedAmount, CurrentAmount]),
    true = (ExpectedAmount == erlang:round(CurrentAmount)).



    actor(UserId, Neighbors) ->
        actor(UserId, Neighbors, ?concurrent_operations, []).

    actor(UserId, _Neighbors, 0, ErrorsList) ->
        LegalErrorsPL = lists:map(fun(Error) ->
            {Error, length(proplists:get_all_values(Error, ErrorsList))}
        end, [
            sender_does_not_exist, receiver_does_not_exist,
            too_many_requests_to_sender, too_many_requests_to_receiver,
            not_enough_money
        ]),
        ct:log("User ~p errorlist: ~p", [UserId, LegalErrorsPL]),
        exit({ok, UserId});

    actor(UserId, Neighbors, OperationsLeft, ErrorsList) ->
        %% Selecting random neighbor
        ChosenNeighbor = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
        %% Sending random amount of money
        case ex_banking:send(UserId, ChosenNeighbor, rand:uniform() * 10, <<"USD">>) of
            %% Success
            {ok, _MyNewBalance, _NeighborNewBalance} ->
                actor(UserId, Neighbors, OperationsLeft - 1, ErrorsList);
            %% Legal errors
            {error, LegalError}
            when sender_does_not_exist == LegalError
            orelse receiver_does_not_exist == LegalError
            orelse too_many_requests_to_sender == LegalError
            orelse too_many_requests_to_receiver == LegalError
            orelse not_enough_money == LegalError ->
                actor(UserId, Neighbors, OperationsLeft - 1, [LegalError | ErrorsList]);
            %% BadErrors
            {error, IllegalError} ->
                ct:log("User ~p got illegal error ~p", [UserId, IllegalError]),
                exit({error, IllegalError})
        end.



    do_receive_exits(0) ->
        ok;

    do_receive_exits(UsersCount) ->
        receive
            {'EXIT', _From, {ok, _UserId}} ->
                do_receive_exits(UsersCount - 1);
            {'EXIT', _From, {error, Reason}} ->
                {error, Reason};
            _ ->
                do_receive_exits(UsersCount)
        after
            30000 ->
                {error, timeout}
        end.
