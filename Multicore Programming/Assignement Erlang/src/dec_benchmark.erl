-module(dec_benchmark).

-export([test_fib/0, test_get_channel_history/0, test_send_message_long/0, test_send_message_mini/0, test_send_message_short/0,
        receive_message/2, test_measure_latency/0]).

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
initialize_server(NumberOfChannels,NumberOfUsers) ->
    rand:seed_s(exsplus, {0, 0, 0}),
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
    {ServerPid, ChannelServer}  = server_not_centralized:initialize_with(Users, dict:new(), Channels),
    {ServerPid, ChannelServer, Channels, Users}.

% Creates a server with 10 channels and 5000 users, and logs in 100 users.
% `Fun(I)` is executed on the clients after log in, where I is the client's
% index, which is also its corresponding user's name.
initialize_server_and_some_clients( NumberOfChannels, NumberOfUsers, ConnectedNumber) ->
    {ServerPid, ChannelServer, Channels, Users} = initialize_server(NumberOfChannels, NumberOfUsers),
    BenchmarkerPid = self(),
    ConnectedUsernames = lists:sublist(dict:fetch_keys(Users), ConnectedNumber),
    % io:format("ConnectedUsernames : \n~p\n\n",[ConnectedUsernames]),
    Clients = lists:map(fun (I) ->
                                ClientPid = spawn_link(?MODULE,receive_message,[BenchmarkerPid,I]),
                                ServerPid ! {ClientPid, log_in, I},
                                receive
                                    {ClientUserName, logged_in, UserHandlerPid} ->
                                        {I, {ClientPid,UserHandlerPid}}

                                end
                        end,
                        ConnectedUsernames),
    

    {ServerPid, ChannelServer, Channels, Users, Clients}.


% Get the history of 100000 channels (repeated 30 times).
test_get_channel_history() ->
    {_ServerPid, ChannelServer, Channels, _Users} = initialize_server(10, 5000),
    NumberOfChannels = dict:size(Channels),
    run_benchmark("get_channel_history",
        fun () ->
            lists:foreach(fun (I) ->
                server:get_channel_history(ChannelServer, I rem NumberOfChannels)
            end,
            lists:seq(1, 100000))
        end,
        30).

% Send a message for 1000 users, and wait for all of them to be broadcast
% (repeated 30 times).
test_send_message(InitFunction, AmountTimes) ->
    run_benchmark("send_message_for_each_user",
        fun () ->
            %BenchmarkerPid = self(),
            %  = fun(I) ->
            %     receive_new_messages(BenchmarkerPid, I)
            % end,
             % io:format("Coucou\n\n\n"),
            {ServerPid, _ChannelServer, _Channels, Users, Clients} =
                InitFunction(),

            ChosenUserNames = lists:sublist(dict:fetch_keys(Users), 10),

            % io:fwrite("Avant : \n~p\n",[ChosenUserNames]),
            send_message_for_users(ServerPid, ChosenUserNames, Users, Clients)
            % io:fwrite("Apres\n")
        end,
        AmountTimes).
%Only one user sends a message.
latency(ActiveUsersFunction, BenchmarkText) ->
    run_benchmark("Measure Latency",
        fun () ->
            BenchmarkPid = self(),
            {ServerPid, ChannelServer, Channels, Users, Clients} = initialize_server_and_some_clients(12, 1000, 50),
            
            %user 1 sends a message
            % io:format("Clients : ~p \n", [Clients]),
            ClientOne = [lists:nth(1,Clients)],
            % io:format("Client One : ~p \n", [ClientOne]),
            send_message_for_users(ServerPid, [],  Users, ClientOne)
        end,
                       
        30).

test_measure_latency()->
    ActiveUsersFunction=fun(Users)->Users end,
    latency(ActiveUsersFunction, "measure latency"). 

%initialize_server_and_some_clients(Fun, NumberOfChannels, NumberOfUsers, ConnectedNumber)
test_send_message_long() ->
    test_send_message(fun () -> initialize_server_and_some_clients(  10, 5000, 100) end, 30).

test_send_message_mini() ->
    test_send_message(fun () -> initialize_server_and_some_clients( 1, 1, 1) end, 30).

test_send_message_short() ->
    test_send_message(fun () -> initialize_server_and_some_clients( 5, 50, 10) end, 30).

send_message_for_users(ServerPid, ChosenUserNames, Users, Clients) ->
    % For each of the chosen users, send a message to channel 1.
    % io:format("----------SEND MESSAGE-------------\n"),
    % io:format("----------SEND MESSAGE-------------\n"),
    % io:format("Clients : ~p\n",[Clients]),
    lists:foreach(fun ({UserName,{ClientId,UserHandler}}) ->
            % io:format("Clients : ~p - ~p\n",[UserName,UserHandler]),
            UserHandler ! {ClientId, send_message, UserName, 1, "Test", os:system_time()}
        end,
        Clients),
    %For each of the active clients, that subscribe to channel 1, wait until messages arrive.
    ClientUserNames = lists:map(fun ({ClName,{_,_}}) -> ClName end, Clients),
    ClientsSubscribedTo1 = lists:filter(fun (UN) ->
        {user, UN, Subscriptions} = dict:fetch(UN, Users),
        sets:is_element(1, Subscriptions)
    end, ClientUserNames),
    %All the client processes that aren't supposed to get a message
    Rest = lists:filter(fun ({UserName,{ClientId,UserHandler}}) ->
        {user, UserName, Subscriptions} = dict:fetch(UserName, Users),
        not sets:is_element(1, Subscriptions)
    end, Clients),
    % io:format("Rest : ~p\n",[Rest]),
    lists:foreach(fun({_,{Client,_}}) -> Client! {self(),stop} end ,Rest),
    % io:format("ClientsSubscribedTo1 : ~p\n",[ClientsSubscribedTo1]),
    lists:foreach(fun (Sender) ->
                        % We expect responses from everyone except the sender.
                        ExpectedResponses = lists:delete(Sender, ClientsSubscribedTo1),
                        lists:foreach(fun (_) ->
                            % io:fwrite("Waiting : \n"),
                            receive
                                {ok ,_} ->
                                    ok
                                end 
                            end, ExpectedResponses)
                  end,
        ClientsSubscribedTo1).
    % io:fwrite("------------END----------\n").


receive_message(BenchmarkerPid,ClientUserName) ->
    receive 
         % A ->
         %     io:format("Message : ~p\n\n",[A]),
         %     receive_message(ClientUserName);
        {UserHandlerPid,logged_in} ->
            BenchmarkerPid ! {ClientUserName, logged_in, UserHandlerPid},
            receive_message(BenchmarkerPid,ClientUserName);
        {_, channel_joined} -> 
            receive_message(BenchmarkerPid,ClientUserName);
            % receive 
            %     A ->
            %         io:format("Message : ~p\n\n",[A]);
            %     {_, new_message, _} ->
            %         ok
            % end;
        {_, new_message, _} ->
            BenchmarkerPid ! {ok, ClientUserName},
            receive_message(BenchmarkerPid,ClientUserName);
        {_,stop} ->
            % io:fwrite("STOP\n"),
            BenchmarkerPid ! {ok, ClientUserName},
            ok
    end.

% Helper function: receives new messages and notifies benchmarker for each
% received message.
% receive_new_messages(BenchmarkerPid, I) ->
%     % receive 
%     %     A ->
%     %         io:format("Message : ~p\n\n",[A]);
%     %     {_, join_channel} -> 
%     %         receive 
%     %             A ->
%     %                 io:format("Message : ~p\n\n",[A]);
%     %             {_, new_message, _} ->
%     %                 BenchmarkerPid ! {ok, I}
%     %         end;
%     %     {_, new_message, _} ->
%     %     BenchmarkerPid ! {ok, I}
%     % end,
%     receive_new_messages(BenchmarkerPid, I).
