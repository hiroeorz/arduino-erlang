%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(arduino_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, Pid} | ignore | {error, Error} when
      Pid :: pid(),
      Error :: term().
start_link() ->
    {ok, Config} = application:get_env(arduino, arduino),
    start_link(Config, [ {sample_event_handler, []} ]).

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%
%% Config example:
%%         {arduino, [{speed, 57600},
%%                    {device, "/dev/ttyACM0"},
%%                    {sampling_interval, 290},
%%                    {digital_port_reporting, [1, 0]},
%%                    {digital_port_offset, 1},
%%                    {analog_offset, 0},
%%                    {analog, [0, 1, 2, 3, 4, 5] },
%%                    {digital, [
%%                               { 0, in, [{pull, up} ]},
%%                               { 1, in, [{pull, up} ]},
%%                               { 2, in, [{pull, up} ]},
%%                               { 3, in, [{pull, up} ]},
%%                               { 4, in, [{pull, up} ]},
%%                               { 5, in, [{pull, up} ]},
%%                               { 6, in, [{pull, up} ]},
%%                               { 7, in, [{pull, up} ]},
%%                               { 8, out},
%%                               { 9, out},
%%                               {10, pwm},
%%                               {11, pwm},
%%                               {12, servo},
%%                               {13, servo}
%%                              ]
%%                    }
%%                   ]}
%%
%% EventHandlers example:
%% [ {my_handler, []}, {your_handler, [Arg1, Arg2]} ]
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Config,  EventHandlers) -> {ok, Pid}     |
					    ignore        |
					    {error, Error} when
      Config :: [tuple()],
      EventHandlers :: [atom()],
      Pid :: pid(),
      Error :: term().
start_link(IOList,  EventHandlers) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [IOList, EventHandlers]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Config, EventHandlers]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    Childs = [event_spec(EventHandlers), arduino_spec(Config)],

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

event_spec(EventHandlers) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {arduino_event, {arduino_event, start_link, [EventHandlers]},
     Restart, Shutdown, Type, [arduino_event]}.

arduino_spec(Config) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {arduino, {arduino, start_link, [Config]},
     Restart, Shutdown, Type, [arduino]}.
