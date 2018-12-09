%%%-------------------------------------------------------------------
%%% @author timothy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2018 20:58
%%%-------------------------------------------------------------------
-module(cgi).
-author("timothy").

%% API
-export([process/2]).
-import(mime, [find_mime/1]).

process(Method, [Path, Type]) ->
  case mime:find_mime(Type) of
    {found, Mime} ->
      process(Method, read_lines("web" ++ Path ++ "." ++ Type, Mime), Mime);
    {not_found, _} ->
      {_, {_, RetryMime}} = mime:get_font_mime(Path),
      process(Method, read_lines("web" ++ Path ++ "." ++ Type, RetryMime), RetryMime)
  end;
process(Method, Path) ->
  process(Method, string:split(Path, ".", trailing)).

process(_Method, {ok, Data}, Mime) ->
  {Data, Mime};
process(_Method, Error, _Mime) ->
  Error.

compress(Data, Mime) ->
  case mime:should_be_compressed(Mime) of
    true ->
      {compressed, zlib:gzip(Data)};
    false ->
      {not_compressed, Data}
  end.

read_lines(FileName, Mime) ->
  case file:open(FileName, [read]) of
    {ok, Device} ->
      try Lines = get_all_lines(Device),
      {ok, compress(Lines, Mime)}
      after file:close(Device)
      end;
    {error, enoent} ->
      {RetriedFileName, {_Found, RetriedMime}} = mime:get_font_mime(FileName),
      case file:open(RetriedFileName, [read]) of
        {ok, Device} ->
          try Lines = get_all_lines(Device),
          {ok, compress(Lines, RetriedMime)}
          after file:close(Device)
          end;
        {error, enoent} ->
          {error, not_found}
      end
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> Line ++ get_all_lines(Device)
  end.


