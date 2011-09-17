%% @author Richard Jones <rj@metabrew.com>
%% Websocket Request wrapper. this is passed to the ws_loop in client code.
%% It talks to mochiweb_websocket_delegate, but hides the pid from the client
%% and has cache of useful properties.
%% Parts of API copied from mochiweb_request.erl
%%
-module(mochiweb_wsrequest, [Pid, RawPath, Headers, Peername, SocketType]).

-export([send/1, close/0, get/1, get_header_value/1, get_cookie_value/1, parse_qs/0, compact/0]).

-define(SAVE_QS, mochiweb_request_qs).
-define(SAVE_PATH, mochiweb_request_path).
-define(SAVE_COOKIE, mochiweb_request_cookie).


get(raw_path)   -> RawPath;
get(headers)    -> Headers;
get(peername)   -> Peername;
get(type)       -> SocketType;  %% plain or ssl
get(path) ->
    case erlang:get(?SAVE_PATH) of
        undefined ->
            {Path0, _, _} = mochiweb_util:urlsplit_path(RawPath),
            Path = mochiweb_util:unquote(Path0),
            put(?SAVE_PATH, Path),
            Path;
        Cached ->
            Cached
    end;
get(peer) ->
    case Peername of 
        {ok, {Addr={10, _, _, _}, _Port}} ->
            case get_header_value("x-forwarded-for") of
                undefined ->
                    inet_parse:ntoa(Addr);
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {{127, 0, 0, 1}, _Port}} ->
            case get_header_value("x-forwarded-for") of
                undefined ->
                    "127.0.0.1";
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {Addr, _Port}} ->
            inet_parse:ntoa(Addr);
        {error, enotconn} ->
            ""
    end.


send(Msg)       -> mochiweb_websocket_delegate:send(Pid, Msg).

close()         -> mochiweb_websocket_delegate:close(Pid).

%% @spec parse_qs() -> [{Key::string(), Value::string()}]
%% @doc Parse the query string of the URL.
parse_qs() ->
    case erlang:get(?SAVE_QS) of
        undefined ->
            %% TODO : ZVI
            {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
            Parsed = mochiweb_util:parse_qs(QueryString),
            put(?SAVE_QS, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

%% @spec get_header_value(K) -> undefined | Value
%% @doc Get the value of a given request header.
get_header_value(K) ->
    mochiweb_headers:get_value(K, Headers).

%% @spec get_cookie_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

%% @spec parse_cookie() -> [{Key::string(), Value::string()}]
%% @doc Parse the cookie header.
parse_cookie() ->
    case erlang:get(?SAVE_COOKIE) of
        undefined ->
            Cookies = case get_header_value("cookie") of
                          undefined ->
                              [];
                          Value ->
                              mochiweb_cookies:parse_cookie(Value)
                      end,
            put(?SAVE_COOKIE, Cookies),
            Cookies;
        Cached ->
            Cached
    end.

%% @doc comact request, i.e. delete path, query string and headers
%%      similar to mochiweb_req:cleanup
%% @end
-spec compact() -> ok.
compact() ->
    io:format("Process dict: ~w~n", [ erlang:get() ]),
    erlang:erase(?SAVE_QS), 
    erlang:erase(?SAVE_PATH), 
    erlang:erase(?SAVE_COOKIE),
    ok.

