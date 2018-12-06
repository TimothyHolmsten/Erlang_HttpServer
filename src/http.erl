%%%-------------------------------------------------------------------
%%% @author timothy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Dec 2018 21:00
%%%-------------------------------------------------------------------
-module(http).
-author("timothy").

%% API
-export([process/1]).
-define(HTTP_VERSION, "HTTP/1.1").

process(Data) ->
  handle_request_path(Data).

%%{http_request,'GET',{abs_path,"/"},{1,1}}
handle_request_path({_RequestType, _Method, {abs_path, "/"}, _Version}) ->
  handle_request_path({_RequestType, _Method, {abs_path, "/index"}, _Version});
handle_request_path({_RequestType, _Method, {abs_path, Path}, _Version}) ->
  F = string:split(Path, ".", trailing),
  handle_request_path(F);
handle_request_path([Path, Type]) ->
  Return = case read_lines("web" ++ Path ++ "." ++ Type) of
             {error, _} ->
               code_not_found();
             _Body ->
               Headers = build_headers([
                 "Content-Type:",
                 find_content_type(Type),
                 "Content-Length: ",
                 integer_to_list(length(_Body))
               ]),

               HTTP = build_http(?HTTP_VERSION, "200", "OK"),

               HTTP ++ Headers ++ "\n" ++ _Body
           end,
  Return;
handle_request_path([Path]) ->
  Return = case read_lines("web" ++ Path ++ ".html") of
             {error, _} ->
               code_not_found();
             _Body ->
               Headers = build_headers([
                 "Content-Type:",
                 "text/html",
                 "Content-Length:",
                 integer_to_list(length(_Body))
               ]),

               HTTP = build_http(?HTTP_VERSION, "200", "OK"),

               HTTP ++ Headers ++ "\n" ++ _Body
           end,
  Return;
handle_request_path({_Type, _Method, _Path, _Version}) ->
  code_not_found().

find_content_type("jpg") ->
  "image/jpeg";
find_content_type("jpeg") ->
  "image/jpeg";
find_content_type("gif") ->
  "image/gif";
find_content_type("png") ->
  "image/png";
find_content_type("svg") ->
  "image/svg+xml";
find_content_type("js") ->
  "text/javascript";
find_content_type("css") ->
  "text/css ";
find_content_type(_) ->
  "text/html".

build_headers([H, [T1, T2] | []]) ->
  build_headers([H, T1 ++ T2 ++ "\n"]);
build_headers([A, B]) ->
  A ++ B;
build_headers([H, T | L]) ->
  build_headers([H ++ T ++ "\n", L]).

build_http(Version, SC, RP) ->
  Version ++ " " ++ SC ++ " " ++ RP ++ "\r" ++ "\n".

code_not_found() ->
  Body = "Not found :(",

  Headers = build_headers([
    "Content-Type:",
    "text/html",
    "Content-Length:",
    integer_to_list(length(Body))
  ]),

  HTTP = build_http(?HTTP_VERSION, "404", "Not Found"),

  HTTP ++ Headers ++ "\n" ++ Body.

read_lines(FileName) ->
  case file:open(FileName, [read]) of
    {ok, Device} ->
      try get_all_lines(Device)
      after file:close(Device)
      end;
    {error, Reason} ->
      {error, Reason}
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

