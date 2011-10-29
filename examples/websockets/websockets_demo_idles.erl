-module(websockets_demo_idles).

-export([start/0, start/1, stop/0, loop/2, wsloop_active/1]).

%-define(DBG(Fmt,Args), io:format(Fmt,Args)).
-define(DBG(Fmt,Args), ok).

-define(SEND_IDLE, 1). 

start() -> start([{port, 8003}, {docroot, "."}]).

start(Options) ->
    PingerPid = spawn(fun pinger/0),
    register(pinger, PingerPid),

    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    %% How we validate origin for cross-domain checks:
    OriginValidator = fun(_Origin) ->
                           ?DBG("Origin '~s' -> OK~n", [_Origin]),
                           true
                      end,
    %% websocket options
    WsOpts  = [ {origin_validator, OriginValidator},
                {loop,   {?MODULE, wsloop_active}} ],
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop},
                         {websocket_opts, WsOpts} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

wsloop_active(WSReq) ->
    pinger ! {newclient, self()},
    start_timer(),
    wsloop_active0(WSReq).

wsloop_active0(WSReq) ->
    receive
        idlemsg ->
            WSReq:send(<<"{}">>),
            wsloop_active0(WSReq);
        {websockets_frame, _Frame} ->
            wsloop_active0(WSReq);
        closed ->
            ?DBG("client api got closed~n",[]),
            ok;
        {error, _Reason} ->
            ?DBG("client api got error ~p~n", [_Reason]),
            ok;
        _Other ->
            ?DBG("Uknown message: ~p~n",[_Other])            
    end.

loop(Req, _DocRoot) ->
    Req:respond({501, [], []}).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

-ifdef(SEND_IDLE).
start_timer() ->
    erlang:start_timer(25000, self(), idlemsg).
-else.
start_timer() ->
    ok.
-endif.

pinger() ->
    start_timer(),
    pinger_loop([]).

pinger_loop(L) ->
    receive
        {timeout,_,idlemsg} ->
            start_timer(),
            StartTime=now(),
            lists:foreach(fun(P) -> P ! idlemsg end, L),
            EndTime=now(),
            io:format("Broadcast ping took ~p~n", [timer:now_diff(EndTime, StartTime) / 100000]),
            pinger_loop(L);
        {newclient, P} ->
            pinger_loop([P|L])
    end.

