%%%-------------------------------------------------------------------
%%% @author timothy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2018 20:29
%%%-------------------------------------------------------------------
-module(web).


-author("timothy").

%% API
-export([web_start/0]).
-import(http, [process/1]).

web_start() ->
  {ok, LSocket} = gen_tcp:listen(9999, [{active, false}, {packet, http}, {reuseaddr, true}]),
  accept(LSocket).

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  spawn(fun() -> handle_message_http(Socket) end),
  accept(LSocket).

% Echo back whatever data we receive on Socket.
%
handle_message_http(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      _Headers = receive_http_headers(Socket, []),
      Response = process(Data),
      gen_tcp:send(Socket, Response),
      gen_tcp:close(Socket);
    {error, closed} ->
      ok
  end.

receive_http_headers(Sock, []) ->
  {ok, Header} = gen_tcp:recv(Sock, 0),
  receive_http_headers(Sock, Header, []).

receive_http_headers(_, http_eoh, Headers) ->
  Headers;
receive_http_headers(Sock, Term, Headers) ->
  {ok, Header} = gen_tcp:recv(Sock, 0),
  receive_http_headers(Sock, Header, [Term | Headers]).
