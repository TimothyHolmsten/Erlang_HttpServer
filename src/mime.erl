%%%-------------------------------------------------------------------
%%% @author timothy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2018 21:29
%%%-------------------------------------------------------------------
-module(mime).
-author("timothy").

%% API
-export([find_mime/1, get_font_mime/1, should_be_compressed/1]).

find_mime("jpg") ->
  mimed("image/jpeg");
find_mime("jpeg") ->
  mimed("image/jpeg");
find_mime("gif") ->
  mimed("image/gif");
find_mime("png") ->
  mimed("image/png");
find_mime("svg") ->
  mimed("image/svg+xml");
find_mime("js") ->
  mimed("text/javascript");
find_mime("css") ->
  mimed("text/css");
find_mime("woff2") ->
  mimed("font/woff2");
find_mime("woff") ->
  mimed("font/woff");
find_mime("ttf") ->
  mimed("x-font/ttf");
find_mime("html") ->
  mimed("text/html");
find_mime("map") ->
  mimed("application/octet-stream");
find_mime(_Mime) ->
  {not_found, _Mime}.

mimed(Mime) ->
  {found, Mime}.

get_font_mime({Name, [_Name, Mime]}) ->
  {Name, find_mime(Mime)};
get_font_mime([Name]) ->
  {Name, []};
get_font_mime([Name, _Split]) ->
  get_font_mime({Name, string:split(Name, ".", trailing)});
get_font_mime(Name) ->
  get_font_mime(string:split(Name, "?")).

should_be_compressed([MainType]) ->
  case MainType of
    "image" ->
      false;
    "font" ->
      false;
    _ ->
      true
  end;
should_be_compressed([MainType, _SpecificType]) ->
  should_be_compressed([MainType]);
should_be_compressed(Mime) ->
  should_be_compressed(string:split(Mime, "/", trailing)).
