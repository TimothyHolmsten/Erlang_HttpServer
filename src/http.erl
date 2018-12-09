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
-import(cgi, [process/2]).
-define(HTTP_VERSION, "HTTP/1.1").

process(Data) ->
  handle_request_path(Data).

content_type(Type) ->
  "Content-Type:" ++ Type ++ "\n".
content_length({compressed, Data}) ->
  "Content-Length:" ++ integer_to_list(byte_size(Data)) ++ "\n";
content_length({not_compressed, Data}) ->
  "Content-Length:" ++ integer_to_list(length(Data)) ++ "\n".
cache_control(Seconds) ->
  "Cache-Control:max-age=" ++ Seconds ++ "\n".
content_encoding(compressed) ->
  "Content-Encoding:gzip\n";
content_encoding(not_compressed) ->
  "Content-Encoding:none\n".

%%{http_request,'GET',{abs_path,"/"},{1,1}}
handle_request_path({_RequestType, _Method, {abs_path, "/"}, _Version}) ->
  handle_request_path({_RequestType, _Method, {abs_path, "/index.html"}, _Version});
handle_request_path({_RequestType, _Method, {abs_path, Path}, _Version}) ->
  case cgi:process(_Method, Path) of
    {{Compressed, Data}, Mime} ->
      Headers = build_headers([
        content_type(Mime),
        content_length({Compressed, Data}),
        content_encoding(Compressed),
        cache_control("31536000")
      ]),
      HTTP = build_http(?HTTP_VERSION, "200", "OK"),
      {HTTP ++ Headers ++ "\n", Data};
    {error, not_found} ->
      Headers = build_headers([
        content_type("text/html"),
        content_length({not_compressed, [0]})
      ]),
      HTTP = build_http(?HTTP_VERSION, "404", "Not Found"),
      {HTTP ++ Headers ++ "\n", [0]}
  end.
build_headers([H, T | []]) ->
  H ++ T;
build_headers([H, T | L]) ->
  build_headers(H ++ T, L).
build_headers(C, [K, V]) ->
  C ++ K ++ V;
build_headers(C, [K, V | L]) ->
  build_headers(C ++ K ++ V, L).

build_http(Version, SC, RP) ->
  Version ++ " " ++ SC ++ " " ++ RP ++ "\r" ++ "\n".
