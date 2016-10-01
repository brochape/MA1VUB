-module(benchmark).

-export([test_fib/0, test_get_channel_history/0, test_send_message/0]).

%% Fibonacci
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

%% Benchmark helpers

% Recommendation: run each test at least 30 times to get statistically relevant
% results.
run_benchmark(Name, Fun, Times) ->
    ThisPid = self(),
    lists:foreach(fun (N) ->
        % Recommendation: to make the test fair, each new test run is to run in
        % its own, newly created Erlang process. Otherwise, if all tests run in
        % the same process, the later tests start out with larger heap sizes and
        % therefore probably do fewer garbage collections. Also consider
        % restarting the Erlang emulator between each test.
        % Source: http://erlang.org/doc/efficiency_guide/profiling.html
        spawn_link(fun () ->
            run_benchmark_once(Name, Fun, N),
            ThisPid ! done
        end),
        receive done ->
            ok
        end
    end, lists:seq(1, Times)).

run_benchmark_once(Name, Fun, N) ->
    io:format("Running benchmark ~s: ~p~n", [Name, N]),

    % Start timers
    % Tips:
    % * Wall clock time measures the actual time spent on the benchmark.
    %   I/O, swapping, and other activities in the operating system kernel are
    %   included in the measurements. This can lead to larger variations.
    %   os:timestamp() is more precise (microseconds) than
    %   statistics(wall_clock) (milliseconds)
    % * CPU time measures the actual time spent on this program, summed for all
    %   threads. Time spent in the operating system kernel (such as swapping and
    %   I/O) is not included. This leads to smaller variations but is
    %   misleading.
    statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

    % Run
    Fun(),

    % Get and print statistics
    % Recommendation [1]:
    % The granularity of both measurement types can be high. Therefore, ensure
    % that each individual measurement lasts for at least several seconds.
    % [1] http://erlang.org/doc/efficiency_guide/profiling.html
    {_, Time1} = statistics(runtime),
    Time2 = timer:now_diff(os:timestamp(), StartTime),
    io:format("CPU time = ~p ms~nWall clock time = ~p ms~n",
        [Time1, Time2 / 1000.0]),
    io:format("~s done~n", [Name]).

%% Benchmarks

test_fib() ->
    run_benchmark("Fibonacci", fun test_fib_benchmark/0, 30).

test_fib_benchmark() ->
    fib(38).

% Creates a server with 10 channels and 5000 users.
initialize_server() ->
    rand:seed_s(exsplus, {0, 0, 0}),
    NumberOfChannels = 10,
    NumberOfUsers = 5000,
    ChannelNames = lists:seq(1, NumberOfChannels),
    UserNames = lists:seq(1, NumberOfUsers),
    Channels = dict:from_list(lists:map(fun (Name) ->
        Messages = [{message, 5, Name, "Hello!", os:system_time()},
                    {message, 6, Name, "Hi!", os:system_time()},
                    {message, 5, Name, "Bye!", os:system_time()}],
        Channel = {channel, Name, Messages},
        {Name, Channel}
        end,
        ChannelNames)),
    Users = dict:from_list(lists:map(fun (Name) ->
        Subscriptions = [rand:uniform(NumberOfChannels),
                         rand:uniform(NumberOfChannels),
                         rand:uniform(NumberOfChannels)],
        User = {user, Name, sets:from_list(Subscriptions)},
        {Name, User}
        end,
        UserNames)),
    ServerPid = server_centralized:initialize_with(Users, dict:new(), Channels),
    {ServerPid, Channels, Users}.

% Creates a server with 10 channels and 5000 users, and logs in 100 users.
% `Fun(I)` is executed on the clients after log in, where I is the client's
% index, which is also its corresponding user's name.
initialize_server_and_some_clients(Fun) ->
    {ServerPid, Channels, Users} = initialize_server(),
    NumberOfActiveUsers = 100,
    BenchmarkerPid = self(),
    Clients = lists:map(fun (I) ->
        ClientPid = spawn_link(fun () ->
            server:log_in(ServerPid, I),
            BenchmarkerPid ! {logged_in, I},
            Fun(I)
        end),
        {I, ClientPid}
        end,
        lists:seq(1, NumberOfActiveUsers)),
    % Ensure that all log-ins have finished before proceeding
    lists:foreach(fun (I) ->
            receive {logged_in, I} ->
                ok
            end
        end,
        lists:seq(1, NumberOfActiveUsers)),
    {ServerPid, Channels, Users, Clients}.

% Get the history of 100000 channels (repeated 30 times).
test_get_channel_history() ->
    {ServerPid, Channels, _Users} = initialize_server(),
    NumberOfChannels = dict:size(Channels),
    run_benchmark("get_channel_history",
        fun () ->
            lists:foreach(fun (I) ->
                server:get_channel_history(ServerPid, I rem NumberOfChannels)
            end,
            lists:seq(1, 100000))
        end,
        30).

% Send a message for 1000 users, and wait for all of them to be broadcast
% (repeated 30 times).
test_send_message() ->
    run_benchmark("send_message_for_each_user",
        fun () ->
            BenchmarkerPid = self(),
            ClientFun = fun(I) ->
                receive_new_messages(BenchmarkerPid, I)
            end,
            {ServerPid, _Channels, Users, Clients} =
                initialize_server_and_some_clients(ClientFun),

            ChosenUserNames = lists:sublist(dict:fetch_keys(Users), 1000),
            send_message_for_users(ServerPid, ChosenUserNames, Users, Clients)
        end,
        30).

send_message_for_users(ServerPid, ChosenUserNames, Users, Clients) ->
    % For each of the chosen users, send a message to channel 1.
    lists:foreach(fun (UserName) ->
            server:send_message(ServerPid, UserName, 1, "Test")
        end,
        ChosenUserNames),
    % For each of the active clients, that subscribe to channel 1, wait until
    % messages arrive.
    ClientUserNames = lists:map(fun ({ClName, _ClPid}) -> ClName end, Clients),
    ClientsSubscribedTo1 = lists:filter(fun (UN) ->
        {user, UN, Subscriptions} = dict:fetch(UN, Users),
        sets:is_element(1, Subscriptions)
    end, ClientUserNames),
    lists:foreach(fun (Sender) ->
        % We expect responses from everyone except the sender.
        ExpectedResponses = lists:delete(Sender, ClientsSubscribedTo1),
        lists:foreach(fun (ClientUserName) ->
                receive {ok, ClientUserName} -> ok end
            end,
            ExpectedResponses)
        end,
        ChosenUserNames).

% Helper function: receives new messages and notifies benchmarker for each
% received message.
receive_new_messages(BenchmarkerPid, I) ->
    receive {_, new_message, _} ->
        BenchmarkerPid ! {ok, I}
    end,
    receive_new_messages(BenchmarkerPid, I).
