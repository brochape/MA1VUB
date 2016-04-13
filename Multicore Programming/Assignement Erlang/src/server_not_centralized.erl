%% This is a simple implementation of the project, using one centralized server.
%%
%% It will create one "server" actor that contains all internal state (users and
%% their subscriptions, channels and their messages, and logged in users).
%%
%% This implementation is provided with unit tests. However, these tests are
%% neither complete nor implementation independent. Thus, be careful when
%% reusing them.
-module(server_not_centralized).

-include_lib("eunit/include/eunit.hrl").

-export([initialize/0, initialize_with/3, server_interface/2, user_handler/2, 
         channel_interface/1, channel_handler/2,typical_session_1/1,
         typical_session_2/1 ]).

%

%%
%% Additional API Functions
%%

% Start server.
initialize() ->
    initialize_with(dict:new(), dict:new(), dict:new()).

% Start server with an initial state.
% Useful for benchmarking.
initialize_with(Users, _, Channels) ->

    % io:fwrite("~p\n\n",[dict:to_list(Channels)]),
    ChannelHandlers = dict:map(fun (ChannelName,{_,ChannelName, Messages}) -> 
                    spawn_link(?MODULE,channel_handler,[Messages, sets:new()]) end, Channels),
    
    % io:fwrite("~p\n\n",[ChannelHandlers]),
    ChannelServer = spawn_link(?MODULE, channel_interface, [ChannelHandlers]),
    catch unregister(channelServer),
    register(channelServer,ChannelServer),
    AssociateWithPid = fun (Element) ->
                    {Element,dict:fetch(Element,ChannelHandlers)}
            end,
    % io:fwrite("~p\n\n\n",[dict:to_list(Users)]),
    UserHandlers = dict:map(fun (_,{_, Name, Subs}) ->
                                SubbedChans = dict:from_list(lists:map(AssociateWithPid,sets:to_list(Subs))),
                                 % io:fwrite("~p\n\n\n",[SubbedChans]),
                                spawn_link(?MODULE,user_handler,[Name, SubbedChans])
                            end,
                            Users),



    ServerPid = spawn_link(?MODULE, server_interface, [Users, UserHandlers]),
    catch unregister(serverInterface),
    register(serverInterface, ServerPid),
    {ServerPid, ChannelServer}.

% The server actor works like a small database and encapsulates all state of
% this simple implementation.
%
% * Users is a dictionary of user names to tuples of the form:
%     {user, Name, Subscriptions}
%   where Subscriptions is a set of channels that the user joined.
% * LoggedIn is a dictionary of the names of logged in users and their pid.
% * Channels is a dictionary of channel names to tuples:
%     {channel, Name, Messages}
%   where Messages is a list of messages, of the form:
%     {message, UserName, ChannelName, MessageText, SendTime}
server_interface(Users, UserHandlers) ->
    receive
        {Sender, register_user, UserName} ->
            NewUsers = dict:store(UserName, {user, UserName, sets:new()}, Users),
            UserHandler = spawn_link(?MODULE, user_handler, [UserName, dict:new()]),
            NewUserHandlers = dict:store(UserName, UserHandler, UserHandlers),
            Sender ! {UserHandler, user_registered},
            server_interface(NewUsers, NewUserHandlers);

        {Sender, log_in, UserName} ->
            UserHandler = dict:fetch(UserName,UserHandlers),
            UserHandler ! {Sender, log_in},
            server_interface(Users, UserHandlers)
    end.


user_handler(Username, ChannelsRegistered) ->
    % io:fwrite("~p",[ChannelsRegistered]),
    receive
        {Sender, log_in} ->
            case dict:is_empty(ChannelsRegistered) of
                false ->
                    dict:map(fun(_, Value) -> Value ! {Sender, join_channel} end , ChannelsRegistered),
                    Sender ! {self(), logged_in},
                    user_handler(Username, ChannelsRegistered);
                true ->
                    Sender ! {self(), logged_in},
                    user_handler(Username, ChannelsRegistered)
            end;

        {Sender, log_out, _} ->%Username is associated with a user handler PID (and only one)
            Sender ! {self(),logged_out},
            user_handler(Username, ChannelsRegistered);

        {Sender, join_channel, _, ChannelName} ->
            channelServer ! {Sender, join_channel, self(), ChannelName},

            user_handler(Username, ChannelsRegistered);
        {Sender, send_message, _, ChannelName, MessageText, SendTime} ->
            case dict:is_key(ChannelName, ChannelsRegistered) of
                true ->
                    Message = {message, Username, ChannelName, MessageText, SendTime},
                    dict:fetch(ChannelName,ChannelsRegistered) ! {Sender, receive_message, Message},
                    user_handler(Username, ChannelsRegistered);
                false ->
                    Sender ! {self(), message_sent},
                    user_handler(Username, ChannelsRegistered)
            end;

        {Sender, get_channel_history, ChannelName} ->
            case dict:is_key(ChannelName, ChannelsRegistered) of
                true ->
                    dict:fetch(ChannelName,ChannelsRegistered) ! {Sender, get_channel_history, ChannelName},
                    user_handler(Username, ChannelsRegistered);
                false ->
                    Sender ! {self(), channel_history, []},
                    user_handler(Username, ChannelsRegistered)
            end;


        {_, subscribe_to_channel, ChannelName, ChannelPid} ->
            NewChannelsRegistered = dict:store(ChannelName, ChannelPid, ChannelsRegistered),
            user_handler(Username,NewChannelsRegistered)
    end.


channel_interface(Channels) ->
    receive
        {Sender, join_channel, UserHandlerPid, ChannelName} ->
            case dict:is_key(ChannelName, Channels) of
                true ->
                    ChannelPid = dict:fetch(ChannelName,Channels),
                    ChannelPid ! {Sender, join_channel},
                    UserHandlerPid ! {Sender, subscribe_to_channel, ChannelName, ChannelPid},
                    channel_interface(Channels);
                false ->
                    ChannelPid = spawn_link(?MODULE, channel_handler, [[],sets:new()]),
                    NewChannels = dict:store(ChannelName,ChannelPid,Channels),
                    % Notifies to the user the Pid of the channel that he created (for his subscribed list)
                    UserHandlerPid ! {Sender, subscribe_to_channel, ChannelName, ChannelPid},
                    ChannelPid ! {Sender, join_channel},
                    channel_interface(NewChannels)
            end;
        {Sender, get_channel_history, ChannelName} ->
            case dict:is_key(ChannelName, Channels) of
                true ->
                    dict:fetch(ChannelName,Channels) ! {Sender, get_channel_history},
                    channel_interface(Channels);
                false ->
                    Sender ! {self(),channel_history,[]},
                    channel_interface(Channels)
            end
    end.



channel_handler(Messages, LoggedIn) ->
    receive
        {Sender, join_channel} -> 
            NewLoggedIn = sets:add_element(Sender, LoggedIn),
            Sender ! {self(), channel_joined},
            channel_handler(Messages,NewLoggedIn);
        {Sender, quit_channel} ->
            NewLoggedIn = sets:del_element(Sender, LoggedIn),
            channel_handler(Messages,NewLoggedIn);
        {Sender, logout} ->
            NewLoggedIn = sets:del_element(Sender,LoggedIn),
            channel_handler(Messages, NewLoggedIn);

        {Sender, receive_message, Message} ->
            NewMessages = Messages ++ [Message],
            Fun = fun (User) -> 
                case Sender == User of
                    false ->
                        % io:fwrite("SENDING NEW MESSAGE FROM : ~p TO ~p \n",[Sender, User]),
                         User ! {self(), new_message, Message};
                    true ->
                        % io:fwrite("NOT SENDING NEW MESSAGE FROM : ~p TO ~p \n", [Sender, User]),
                        ok
                end
             end,
             % io:fwrite("LoggedIn : ~p\n", [LoggedIn]),
            lists:foreach(Fun, sets:to_list(LoggedIn)),
            Sender ! {self(), message_sent},
            channel_handler(NewMessages, LoggedIn);

        {Sender, get_channel_history} ->
            Sender ! {self(), channel_history, Messages},
            channel_handler(Messages, LoggedIn)

    end.




%%
%% Tests
%%
% These tests are for this specific implementation. They are a partial
% definition of the semantics of the provided interface but also make certain
% assumptions of the implementation. You can re-use them, but you might need to
% modify them.

initialize_test() ->
    catch unregister(serverInterface),
    initialize().

register_user_test() ->
    initialize_test(),
    ?assertMatch({_, user_registered}, server:register_user(serverInterface, "A")),
    ?assertMatch({_, user_registered}, server:register_user(serverInterface, "B")),
    ?assertMatch({_, user_registered}, server:register_user(serverInterface, "C")),
    ?assertMatch({_, user_registered}, server:register_user(serverInterface, "D")),
    ["A", "B", "C", "D"].

log_in_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    ?assertMatch({_Server1, logged_in}, server:log_in(serverInterface, UserName1)),
    ?assertMatch({_Server2, logged_in}, server:log_in(serverInterface, UserName2)).
    % Note: returned pids _Server1 and _Server2 do not necessarily need to be
    % the same.

log_out_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(serverInterface, UserName1),
    {Server2, logged_in} = server:log_in(serverInterface, UserName2),
    ?assertMatch(logged_out, server:log_out(Server1, UserName1)),
    ?assertMatch(logged_out, server:log_out(Server2, UserName2)).

join_channel_test() ->
    [UserName1 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(serverInterface, UserName1),
    ?assertMatch(channel_joined,
        server:join_channel(Server1, UserName1, "Channel1")),
    ?assertMatch(channel_joined,
        server:join_channel(Server1, UserName1, "Channel2")),
    {UserName1, Server1, "Channel1", "Channel2"}.

send_message_test() ->
    {UserName1, Server1, Channel1, _Channel2} = join_channel_test(),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, Channel1, "Hello!")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, Channel1, "How are you?")).

channel_history_test() ->
    % Create users, log in, join channels.
    [UserName1, UserName2 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(serverInterface, UserName1),
    {Server2, logged_in} = server:log_in(serverInterface, UserName2),
    Channel1 = "Channel1",
    server:join_channel(Server1, UserName1, Channel1),
    server:join_channel(Server2, UserName2, Channel1),

    % Send some messages
    server:send_message(Server1, UserName1, Channel1, "Hello!"),
    server:send_message(Server2, UserName2, Channel1, "Hi!"),
    server:send_message(Server1, UserName1, Channel1, "How are you?"),
    % Check history
    [{message, UserName1, Channel1, "Hello!", Time1},
     {message, UserName2, Channel1, "Hi!", Time2},
     {message, UserName1, Channel1, "How are you?", Time3}] =
        server:get_channel_history(channelServer, Channel1),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3).

typical_session_test() ->
    initialize_test(),
    Session1 = spawn_link(?MODULE, typical_session_1, [self()]),
    Session2 = spawn_link(?MODULE, typical_session_2, [self()]),
    receive
        {Session1, ok} ->
            receive
                {Session2, ok} ->
                    done
            end
    end.

typical_session_1(TesterPid) ->
    {_, user_registered} = server:register_user(serverInterface, "Jennifer"),
    {Server, logged_in} = server:log_in(serverInterface, "Jennifer"),
    channel_joined = server:join_channel(Server, "Jennifer", "multicore"),
    message_sent = server:send_message(Server, "Jennifer", "multicore", "Hello!"),
    % Wait for reply
    Time2 = receive
        {_, new_message, Message} ->
            ?assertMatch({message, "Janwillem", "multicore", "Hi!", _}, Message),
            {message, _, _, _, Time} = Message,
            Time
    end,
    % Respond
    message_sent = server:send_message(Server, "Jennifer", "multicore", "How are you?"),

    % Check history
    [{message, "Jennifer",  "multicore", "Hello!",       Time1},
     {message, "Janwillem", "multicore", "Hi!",          Time2},
     {message, "Jennifer",  "multicore", "How are you?", Time3}] =
        server:get_channel_history(channelServer, "multicore"),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3),

    TesterPid ! {self(), ok}.

typical_session_2(TesterPid) ->
    {_, user_registered} = server:register_user(serverInterface, "Janwillem"),
    {Server, logged_in} = server:log_in(serverInterface, "Janwillem"),
    channel_joined = server:join_channel(Server, "Janwillem", "multicore"),
    % Wait for first message
    Time1 = receive
        {_, new_message, Message1} ->
            ?assertMatch({message, "Jennifer", "multicore", "Hello!", _}, Message1),
            {message, _, _, _, Time} = Message1,
            Time
    end,
    % Reply
    message_sent = server:send_message(Server, "Janwillem", "multicore", "Hi!"),
    % Wait for response
    Time3 = receive
        {_, new_message, Message3} ->
            ?assertMatch({message, "Jennifer", "multicore", "How are you?", _}, Message3),
            {message, _, _, _, Time_} = Message3,
            Time_
    end,

    % Check history
    [{message, "Jennifer",  "multicore", "Hello!",       Time1},
     {message, "Janwillem", "multicore", "Hi!",          Time2},
     {message, "Jennifer",  "multicore", "How are you?", Time3}] =
        server:get_channel_history(channelServer, "multicore"),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3),

    TesterPid ! {self(), ok}.
