%%%------------------------------------------------------------------------------
%%% @copyright (c) 2013, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%    Erlang Port Mapper Daemon implemented as an esp_tcp_service.
%%% @since v1.0.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esp_epmd).
-behaviour(esp_tcp_service).

-include("license_and_copyright.hrl").
-author('Jay Nelson <jay@duomark.com>').

%% Public API
-export([start_link/2]).

%% Callback API
-export([recv/4, recv_closed/3, recv_error/4]).


%%------------------------------------------------------------------------------
%%
%% Erlang Port Mapper Daemon
%%
%% An esp_tcp_service is used to listen for EPMD connections. Once
%% connected, an EPMD message is handled by the acceptor process
%% which recieved the connection request.
%%
%%------------------------------------------------------------------------------
-spec start_link(pos_integer(), pos_integer()) -> {ok, pid()}.

start_link(Epmd_Port, Acceptor_Count)
  when is_integer(Epmd_Port), Epmd_Port > 0,
       is_integer(Acceptor_Count), Acceptor_Count > 0 ->
    Socket_Opts = [binary, {packet, 2}, {backlog, 30}, {reuseaddr, true}],
    Tcp_Service = esp_tcp_service:new_active(Acceptor_Count, Acceptor_Count, Socket_Opts),
    Head_Kill_Switch = esp_service:link_service(Tcp_Service),
    esp_tcp_service:start(Tcp_Service, [{client_module, ?MODULE}, {port, Epmd_Port}]),
    {ok, Head_Kill_Switch}.

    
%%------------------------------------------------------------------------------
%% Active socket receive data
%%------------------------------------------------------------------------------
-spec recv(gen_tcp:socket(), string(), pos_integer(), binary()) -> ok.
-spec recv_closed(gen_tcp:socket(), string(), pos_integer()) -> ok.
-spec recv_error(gen_tcp:socket(), string(), pos_integer(), atom()) -> ok.

recv(Socket, Ip, Port, Data) ->

    %% inet:setopts fiddling works because we are currently {active, false}
    %% and no new packets will be received/parsed until this function finishes.
    %% Socket ownership is only needed by the receiver, not the sender.

    %% Replies are sent without packet sizing...
    inet:setopts(Socket, [{packet, raw}]),
    Closed_Socket = handle_request(Socket, Ip, Port, Data),

    %% But next receive should be with packet sizing.
    Closed_Socket orelse inet:setopts(Socket, [{packet, 2}]),
    ok.


%% Handle request uses a one-shot process for all requests
%% except the alive signal which remains to answer connected node queries.
-spec handle_request(gen_tcp:socket(), any(), pos_integer(), binary()) -> boolean().

handle_request(Socket, Ip, Port, << Cmd_Char, Args/binary >> = Request) ->
    case Cmd_Char of

        %% Signals are used when nodes connect...
        $x -> alive_signal(Socket, Ip, Port, Args), false;

        %% Queries return info about connected nodes...
        $d -> dump_query(Socket, Ip, Port),         true;
        $n -> name_query(Socket, Ip, Port),         true;
        $z -> port_query(Socket, Ip, Port, Args),   true;
        
        %% Commands control the epmd daemon...
        $k -> kill_cmd(Socket, Ip, Port),           true;
        $s -> stop_cmd(Socket, Ip, Port, Args),     true;

        %% Anything else is an unexpected request.
        _Unknown ->
            Err_Args = [?MODULE, Socket, {Ip, Port}, Request],
            error_logger:info_msg("esp_epmd ~p:handle_request unknown request ~p ~p~n", Err_Args),
            true
    end.

recv_closed(Socket, Ip, Port) ->
    Err_Args = [?MODULE, Socket, {Ip, Port}],
    error_logger:info_msg("esp_epmd ~p:recv_closed connection: ~p ~p.~n", Err_Args),
    ok.

recv_error(Socket, Ip, Port, Reason) ->
    Err_Args = [?MODULE, Socket, Ip, Port, Reason],
    error_logger:info_msg("esp_epmd ~p:recv_error ~p, ~p ~p: ~p~n", Err_Args),
    ok.


%%------------------------------------------------------------------------------
%% Internal functions to fulfill epmd requests
%%------------------------------------------------------------------------------
alive_signal(Socket, Ip, Port, <<PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, Rest/binary>>) ->
    <<NodeName:NLen/binary, ELen:16, Extra/binary>> = Rest,
    Creation = random:uniform(3),
    error_logger:info_msg(
      "esp_epmd: alive request from ~s:~b PortNo: ~b, NodeType: ~b, Proto: ~b, HiVer: ~b, LoVer: ~b, NodeName: '~s', ELen: ~p, Extra: ~p, Creation: ~b.~n",
      [inet_parse:ntoa(Ip), Port, PortNo, NodeType, Proto, HiVer, LoVer, NodeName, ELen, Extra, Creation]),
    %% ets:insert_new(erlpmd, {NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, Creation, Socket}}),
    gen_tcp:send(Socket, <<$y, 0:8, Creation:16>>).
% Already registered - reply with error
%	error_logger:error_msg("esp_epmd: ~s 'name' is already registered.~n", [NodeName]),
%	gen_server:cast(From, {msg, <<$y, 1:8, 99:16>>, Ip, Port})

dump_query(Socket, Ip, Port) ->
    error_logger:info_msg("esp_epmd: dump request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
    %% send request to collect all nodes
    Nodes = list_to_binary(lists:flatten(io_lib:format("active name     ~s at port ~p, fd = ~p ~n", ["foo", Port, Socket]))),
    gen_server:send(Socket, <<14369:32, Nodes/binary>>),
	  gen_tcp:close(Socket).

name_query(Socket, Ip, Port) ->
    error_logger:info_msg("esp_epmd: name(s) request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
    %% send request to collect all names
    Data = list_to_binary(lists:flatten(io_lib:format("name ~s at port ~p~n", ["foo", Port]))),
    gen_tcp:send(Socket, <<14369:32, Data/binary>>),
    gen_tcp:close(Socket).

port_query(Socket, Ip, Port, <<NodeName/binary>>) ->
    error_logger:info_msg("esp_epmd: port ~s request from ~s:~p.~n", [NodeName, inet_parse:ntoa(Ip), Port]),
    %% send request to find name
    %% gen_tcp:send(Socket, <<$w, 1:8>>)
    %%  [{NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, _, _}}] ->
    %%	  NLen = size(NodeName),
    %%	  ELen = size(Extra),
    %%	  gen_tcp:send(Socket, <<$w, 0:8, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, NodeName:NLen/binary, ELen:16, Extra:ELen/binary>>)
    %% end,
    gen_tcp:close(Socket).

kill_cmd(Socket, Ip, Port) ->
    error_logger:info_msg("esp_epmd: kill request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
    gen_tcp:send(Socket, <<"OK">>),
    gen_tcp:close(Socket).

stop_cmd(Socket, Ip, Port, <<$s, NodeName/binary>>) ->
    error_logger:info_msg("esp_epmd: '~s' stop request from ~s:~p.~n", [NodeName, inet_parse:ntoa(Ip), Port]),
    gen_tcp:send(Socket, <<"STOPPED">>),
    gen_tcp:close(Socket).
