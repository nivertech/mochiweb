%%%-------------------------------------------------------------------
%%% File    : mochiweb_sanity_SUITE.erl
%%% Author  : Ori Bar <ori.bar@nivertech.com>
%%% Description : 
%%%
%%% Created :  2 Aug 2011 by Ori Bar <ori.bar@nivertech.com>
%%%-------------------------------------------------------------------
-module(mochiweb_sanity_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(FLASH_PORT, 2785).

%%------------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%------------------------------------------------------------------------------

%% @doc A list of key/value pairs, holding the suite info.
-type info()    ::[{atom(),term()}].

%% A list of key/value pairs, holding the test case configuration.
-type config()  ::[{atom(),term()}].

%%------------------------------------------------------------------------------
%% @doc Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%% @end
%%------------------------------------------------------------------------------
-spec suite() -> info().
suite() ->
    [{timetrap,{minutes,10}}].

%%------------------------------------------------------------------------------
%% @doc Initialization before the suite.
%%
%% Reason - The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_suite(Config0::config()) ->
              Config1::config()     
            | {skip, Reason::term()} 
            | {skip_and_save, Reason::term(), Config1::term()}.

init_per_suite(Config) ->
    Config.

%%------------------------------------------------------------------------------
%% @doc Cleanup after the suite
%% @end
%%------------------------------------------------------------------------------
-spec end_per_suite(Config0::config()) -> 
            no_return()
          | {save_config, Config1::config()}.

end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Initialization before each test case group.
%%
%% GroupName - Name of the test case group that is about to run.
%% Reason - The reason for skipping all test cases and subgroups in the group.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_group(GroupName::atom(), Config0::config()) ->
              Config1::config()       
            | {skip, Reason::term()}
            | {skip_and_save, Reason::term(), Config1::config()}.

init_per_group(_GroupName, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% @doc Cleanup after each test case 
%%
%% GroupName - Name of the test case group that is finished.
%% Config0, Config1 - a list of key/value pairs, holding configuration data for the group.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_group(GroupName::atom(), Config0::config()) ->
              no_return()
            | {save_config, Config1::config()} .

end_per_group(_GroupName, _Config) ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Initialization before each test case.
%%
%% TestCase - Name of the test case that is about to run.
%% Config0, Config1 -  A list of key/value pairs, holding the test case configuration.
%% Reason - The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_testcase(TestCase::atom(), Config0::config()) ->
              Config1::config() 
            | {skip, Reason::term()} 
            | {skip_and_save, Reason::term(), Config1::config()} .

init_per_testcase(_TestCase, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% @doc Cleanup after each test case.
%%
%% TestCase - Name of the test case that is finished.
%% Config0, Config1 - a list of key/value pairs, holding the test case configuration.
%% Reason - the reason for failing the test case.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_testcase(TestCase::atom(), Config0::config()) ->
               no_return() 
             | {save_config, Config1::config()} 
             | {fail, Reason::term()} .

end_per_testcase(_TestCase, _Config) ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Returns a list of test case group definitions.
%% @end
%%------------------------------------------------------------------------------
-type shuffle()   :: shuffle | {shuffle, Seed::seed()}.
-type seed()      :: {integer(),integer(),integer()}.
-type repeat_type()::    repeat 
                      | repeat_until_all_ok 
                      | repeat_until_all_fail 
                      | repeat_until_any_ok 
                      | repeat_until_any_fail .
-type group_properties() :: [parallel | sequence | shuffle() | {repeat_type(), integer()|forever}] .
-type groups_and_test_cases()   :: [group() | {group,atom()} | atom()] .
-type group() :: {
                    GroupName           :: atom(), 
                    Properties          :: group_properties(), 
                    GroupsAndTestcases  :: groups_and_test_cases() 
                 } .
-spec  groups() -> [group()].

groups() ->
    [].

%%------------------------------------------------------------------------------
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%% GroupName- Name of a test case group.
%% TestCase - Name of a test case.
%% Reason - The reason for skipping all groups and test cases.
%% @end
%%------------------------------------------------------------------------------
-spec all() -> 
            [{group, atom()} | atom()] 
          | {skip, Reason::term()} .

all() -> 
    [flash_test].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Test case info function - returns list of tuples to set
%%              properties for the test case.
%%
%% Info - List of key/value pairs.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%% @end
%%------------------------------------------------------------------------------
flash_test() -> 
    [].

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0, Config1 - a list of key/value pairs, holding the test case configuration.
%% Reason - the reason for skipping the test case.
%% Comment = a comment about the test case that will be printed in the html log.
%% @end
%%--------------------------------------------------------------------
-spec flash_test(Config0::config()) ->
                ok 
              | no_return()
              | {skip, Reason::term()} 
              | {comment, Comment::term()} 
              | {save_config, Config1::config()} 
              | {skip_and_save, Reason::term(), Config1::config()} .

flash_test(_Config) -> 
    Name = ?MODULE,
    Loop = fun(Req) -> 
                   Req:respond({204, [], <<>>}),
                   Req:close()
           end,
    mochiweb_http:start([{name, Name},
                         {loop, Loop},
                         {flash_policy_server, true},
                         {port, ?FLASH_PORT}]),
    {ok, Sock1} = gen_tcp:connect("localhost", ?FLASH_PORT, [binary, {packet, 0}, {active, false}]),
    gen_tcp:send(Sock1, "GET / HTTP/1.1\r\n\r\n"),
    {ok, <<"HTTP/1.1 204", _/bytes>>} = do_recv(Sock1, []),
    ok = gen_tcp:close(Sock1),
    

    {ok, Sock2} = gen_tcp:connect("localhost", ?FLASH_PORT, [binary, {packet, 0}, {active, false}]),
    gen_tcp:send(Sock2, ["<policy-file-request/>",0]),
    {ok, <<"<?xml version=\"1.0\" encoding=\"utf-8\"?>", _/bytes>>} = do_recv(Sock2, []),
    ok = gen_tcp:close(Sock2),
    mochiweb_http:stop(Name).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.
