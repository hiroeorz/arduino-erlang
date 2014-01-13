%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(arduino).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         cast/1,
         firmata_version_request/0,
         initialize/0,
         all_digital/0,
         all_analog/0,
         digital_write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% timer use function
-export([init_pin/1]).

-define(SERVER, ?MODULE).
-define(SYSEX_START_CODE, 16#F0).
-define(SYSEX_END_CODE,   16#F7).

-type pin_mode() :: in | out | analog | pwm | servo.

-record(state, {serial_pid                  :: pid(),
                init_flag = true            :: boolean(),
                recv_queue = <<>>           :: binary(),
                digital_conf                :: [tuple()],
                analog_conf                 :: [non_neg_integer()],
                sampling_interval = 19      :: non_neg_integer(),
                digital_port_reporting_conf :: [non_neg_integer()],
                digital_port_offset = 0     :: non_neg_integer(),
                analog_offset = 0           :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(Conf) -> {ok, pid()} | ignore | {error, term()} when
      Conf :: [tuple()].
start_link(Conf) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Conf], []).

%%--------------------------------------------------------------------
%% @doc get firmata version from arduino.
%% @end
%%--------------------------------------------------------------------
-spec firmata_version_request() -> ok.
firmata_version_request() ->
    cast(firmata:format(version_report_request)).

%%--------------------------------------------------------------------
%% @doc send request to arduino
%% @end
%%--------------------------------------------------------------------
-spec cast(binary()) -> ok.
cast(Bin) when is_binary(Bin) ->
    gen_server:cast(?SERVER, {cast, Bin}).

%%--------------------------------------------------------------------
%% @doc get digital state.
%% @end
%%--------------------------------------------------------------------
-spec all_digital() -> [0 | 1].
all_digital() ->
    case ets:info(arduino_digital) of  %% for before init call.
	undefined -> [];
	_ ->
	    case ets:first(arduino_digital) of 
		'$end_of_table' ->
		    [];
		Key ->
		    all_digital(Key, [])
	    end
    end.

all_digital('$end_of_table', Result) ->
    lists:reverse(Result);

all_digital(Key, Result) ->
    [{_PortNo, [X0, X1, X2, X3, X4, X5, X6, X7]}] = 
	ets:lookup(arduino_digital, Key),

    NextKey = ets:next(arduino_digital, Key),
    all_digital(NextKey, [X7, X6, X5, X4, X3, X2, X1, X0 | Result]).

%%--------------------------------------------------------------------
%% @doc get analog state.
%% @end
%%--------------------------------------------------------------------
-spec all_analog() -> [non_neg_integer()].
all_analog() ->
    case ets:info(arduino_digital) of %% for before init call.
	undefined -> [];
	_ ->
	    case ets:first(arduino_analog) of 
		'$end_of_table' ->
		    [];
		Key ->
		    all_analog(Key, [])
	    end
    end.

all_analog('$end_of_table', Result) ->
    lists:reverse(Result);

all_analog(Key, Result) ->
    [{_PinNo, Val}] = ets:lookup(arduino_analog, Key),
    NextKey = ets:next(arduino_analog, Key),
    all_analog(NextKey, [Val | Result]).

%%--------------------------------------------------------------------
%% @doc (re)initialize all pins.
%% @end
%%--------------------------------------------------------------------
-spec initialize() -> ok.
initialize() ->
    gen_server:cast(?SERVER, initialize).

%%--------------------------------------------------------------------
%% @doc write ON or OFF to a digital port (8 pins). 
%% @end
%%--------------------------------------------------------------------
-spec digital_write(PortNo, Vals) -> ok when
      PortNo :: non_neg_integer(),
      Vals :: [0 | 1].
digital_write(PortNo, Vals) when is_list(Vals), length(Vals) =:= 8 ->
    gen_server:cast(?SERVER, {digital_write, [PortNo, Vals]}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Conf]) ->
    Device = proplists:get_value(device, Conf, "/dev/ttyACM0"),
    Speed = proplists:get_value(speed, Conf, 57600),
    Digital = proplists:get_value(digital, Conf, 
				  [{PinNo, in} || PinNo <- lists:seq(0, 13)] ),
    Analog = proplists:get_value(analog, Conf, [0, 1, 2, 3, 4, 5]),
    SamplingInterval = proplists:get_value(sampling_interval, Conf, 1000),
    AnalogOffset = proplists:get_value(analog_offset, Conf, 0),
    DiPortReporting = proplists:get_value(digital_port_reporting, Conf, [1, 1]),
    DiPortOffset = proplists:get_value(digital_port_offset, Conf, 0),

    case file:open(Device, [read]) of
	{error, eisdir} -> %% device file exist
	    Pid = serial:start([{speed, Speed}, {open, Device}]),
	    ets:new(arduino_digital, [ordered_set, protected, named_table]),
	    ets:new(arduino_analog,  [ordered_set, protected, named_table]),

	    State = #state{serial_pid = Pid, 
			   digital_conf = Digital, 
			   analog_conf = Analog,
			   sampling_interval = SamplingInterval,
			   digital_port_reporting_conf = DiPortReporting,
			   digital_port_offset = DiPortOffset,
			   analog_offset = AnalogOffset},

	    reset_config(State),
	    version_report_request(State),
	    {ok, State};
	{error, enoent} -> %% file not exist
	    ignore
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({cast, Bin}, State) ->
    SerialPid = State#state.serial_pid,
    send(Bin, SerialPid),
    {noreply, State};

handle_cast(initialize, State) ->
    State1 = init_pin(State),
    {noreply, State1};

handle_cast({dirigal_write, PortNo, Vals}, State) ->
    SerialPid = State#state.serial_pid,
    digital_write(PortNo, Vals, SerialPid),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------------
%% sysex firmata data
%%--------------------------------------------------------------------------
handle_info({data, <<?SYSEX_START_CODE:8, _/binary>> = Queue},
	    #state{recv_queue = <<>>} = State) ->

    State1 = check_first_message(State),
    process_firmata_sysex(Queue, State1);

handle_info({data, Bin},
	    #state{recv_queue = <<?SYSEX_START_CODE:8, _/binary>>} = State) ->

    State1 = check_first_message(State),
    RecvQueue = State1#state.recv_queue,
    Queue = <<RecvQueue/binary, Bin/binary>>,
    process_firmata_sysex(Queue, State1);

%%--------------------------------------------------------------------------
%% normal firmata data
%%--------------------------------------------------------------------------
handle_info({data, <<>>}, #state{recv_queue = <<>>} = State) ->
    {noreply, State};

handle_info({data, Bin}, #state{recv_queue = RecvQueue} = State) ->
    State1 = check_first_message(State),
    Queue = <<RecvQueue/binary, Bin/binary>>,
    <<Code:8, TailOfTotal/binary>> = Queue,
    Size = firmata:size(Code),

    if Size =:= unknown ->
	    io:format("code(~p) not matched in getting size~n", [Code]),
	    io:format("data = ~p~n", [Queue]),
	    erlang:error(invalid_firmata_code);
       true -> ok
    end,

    if byte_size(TailOfTotal) >= Size ->
	    <<Code:8, Body:Size/binary, TailBin/binary>> = Queue,
	    Reply = firmata:parse(Code, Body),
	    NewState = handle_firmata(Reply, State1),
	    handle_info({data, TailBin}, NewState#state{recv_queue = <<>>});
       true ->
	    {noreply, State#state{recv_queue = Queue}}
    end;

%%--------------------------------------------------------------------------
%% unknown
%%--------------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unknown info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Firmata handler functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc handle firmata message from arduino.
%% @end
%%--------------------------------------------------------------------
handle_firmata({sysex, {name_and_version_report, {_, _, SketchName}}}, State) ->
    io:format("Arduino sketch: ~s~n", [SketchName]),
    State;

handle_firmata({version_report, {MeasureVer, MinorVer}}, State) ->
    io:format("Firmata version: ~w.~w~n", [MeasureVer, MinorVer]),
    State;

handle_firmata({digital_io_message, {ArduinoPortNo, Status}}, State) ->
    PortNo = ArduinoPortNo + State#state.digital_port_offset,
    gen_event:notify(arduino_event, {digital_port_changed, PortNo, Status}),
    true = ets:insert(arduino_digital, {PortNo, Status}),
    State;

handle_firmata({analog_io_message, {PinNo, Val}}, State) ->
    gen_event:notify(arduino_event, {analog_recv, PinNo, Val}),
    true = ets:insert(arduino_analog, {PinNo, Val}),
    State;

handle_firmata(Reply, State) ->
    io:format("haldle unknown message: ~p~n", [Reply]),
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc process firmata sysex protocol.
%% @end
%%--------------------------------------------------------------------
-spec process_firmata_sysex(binary(), #state{}) -> {noreply, #state{}}.
process_firmata_sysex(Queue, State) ->
    case has_sysex_end(Queue) of
	{true, Size} ->

	    <<?SYSEX_START_CODE:8, Body:Size/binary, ?SYSEX_END_CODE:8,
	      TailBin/binary>> = Queue,

	    Reply = firmata:parse(?SYSEX_START_CODE, Body),
	    NewState = handle_firmata(Reply, State),
	    handle_info({data, TailBin}, NewState#state{recv_queue = <<>>});
	false ->
	    {noreply, State#state{recv_queue = Queue}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc search sysex end code.
%%
%% when sysex end code is exist, return {true, Size}.
%% when not exist,               return false
%% @end
%%--------------------------------------------------------------------
-spec has_sysex_end(binary()) -> {true, non_neg_integer()} | false.
has_sysex_end(Bin) ->
    has_sysex_end(Bin, 0).

has_sysex_end(<<>>, _) ->
    false;

has_sysex_end(<<?SYSEX_END_CODE:8, _/binary>>, Size) ->
    {true, Size - 1};

has_sysex_end(<<_:8/integer, TailBin/binary>>, Size) ->
    has_sysex_end(<<TailBin/binary>>, Size + 1).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc send message to serial process.
%% @end
%%--------------------------------------------------------------------
-spec send(Bin, SerialPid) -> ok when
      Bin :: binary(),
      SerialPid :: pid().
send(Bin, SerialPid) ->
    SerialPid ! {send, Bin},
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc version report request.
%% @end
%%--------------------------------------------------------------------
-spec version_report_request(State) -> ok when
      State :: #state{}.
version_report_request(State) ->
    SerialPid = State#state.serial_pid,
    send(firmata:format(version_report_request), SerialPid).

%%--------------------------------------------------------------------
%% @private
%% @doc reset arduino config.
%% @end
%%--------------------------------------------------------------------
-spec reset_config(State) -> ok when
      State :: #state{}.
reset_config(State) ->
    SerialPid = State#state.serial_pid,
    send(firmata:format(syste_reset), SerialPid),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc init all pins if init_flag is true.
%% @end
%%--------------------------------------------------------------------
-spec check_first_message(State) -> State when
      State :: #state{}.
check_first_message(#state{init_flag = true} = State) ->
    timer:sleep(1000),
    init_pin(State),
    State#state{init_flag = false};

check_first_message(#state{init_flag = false} = State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc initialize gpio pin.
%%
%% set digital pin mode(in or out or analog or pwm or servo).
%% set digital pin reporting.
%% set analog  pin reporting. 
%% @end
%%--------------------------------------------------------------------
-spec init_pin(State) -> State when
      State :: #state{}.
init_pin(State) ->
    io:format("Arduino pin initializing "),
    ok = reset_config(State),
    timer:sleep(3), %%一旦反映させる
    DigitalList = State#state.digital_conf,
    DigitalPortReporting = State#state.digital_port_reporting_conf,
    AnalogList = State#state.analog_conf,
    SamplingInterval = State#state.sampling_interval,

    SerialPid = State#state.serial_pid,
    ok = sampling_interval(SamplingInterval, SerialPid),
    ok = init_pin_mode(DigitalList, SerialPid),
    ok = set_pullmode(DigitalList, SerialPid),
    ok = set_digital_port_reporting(0, DigitalPortReporting, SerialPid),
    ok = set_analog_port_reporting(AnalogList, SerialPid),
    io:format("done.~n"),
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc set all digital pin mode.
%% @end
%%--------------------------------------------------------------------
-spec init_pin_mode(DigitalList, SerialPid) -> ok when
      DigitalList :: [tuple()],
      SerialPid :: pid().
init_pin_mode([], _SerialPid) ->
    ok;

init_pin_mode([{PinNo, Mode} | Tail], SerialPid) ->
    init_pin_mode([{PinNo, Mode, []} | Tail], SerialPid);

init_pin_mode([{PinNo, Mode, _} | Tail], SerialPid) ->
    set_digital_pin_mode(PinNo, Mode, SerialPid),
    init_pin_mode(Tail, SerialPid).

%%--------------------------------------------------------------------
%% @private
%% @doc set digital pull mode. up -> pullup, none -> free.
%% @end
%%--------------------------------------------------------------------
-spec set_pullmode(DigitalList, SerialPid) -> ok when
      DigitalList :: [tuple()],
      SerialPid :: pid().
set_pullmode(DigitalList, SerialPid) ->
    set_pullmode(DigitalList, SerialPid, 0, []).

set_pullmode([], _SerialPid, _PortNo, _Bits) ->
    ok;

set_pullmode([{PinNo, Mode} | Tail], SerialPid, PortNo, Bits) ->
    set_pullmode([{PinNo, Mode, []} | Tail], SerialPid, PortNo, Bits); 

set_pullmode([{_PinNo, _, Opts} | Tail], SerialPid, PortNo, Bits) ->
    NewBits = case proplists:get_value(pull, Opts) of
		  undefined ->
		      [0 | Bits];
		  none ->
		      [0 | Bits];
		  up ->
		      [1 | Bits]
	      end,
    
    if length(NewBits) =:= 8 ->
	    digital_write(PortNo, NewBits, SerialPid),
	    set_pullmode(Tail, SerialPid, PortNo + 1, []);
       true ->
	    set_pullmode(Tail, SerialPid, PortNo, NewBits)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc set digital pin mode.
%% @end
%%--------------------------------------------------------------------
-spec set_digital_pin_mode(PinNo, Mode, SerialPid) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: pin_mode(),
      SerialPid :: pid().
set_digital_pin_mode(PinNo, Mode, SerialPid) ->
    ModeInt = case Mode of
		  in     -> 0;
		  out    -> 1;
		  analog -> 2;
		  pwm    -> 3;
		  servo  -> 4
	      end,

    Command = firmata:format(set_pin_mode, {PinNo, ModeInt}),
    send(Command, SerialPid).

%%--------------------------------------------------------------------
%% @private
%% @doc set digital pin to reporting.
%% @end
%%--------------------------------------------------------------------
-spec set_digital_port_reporting(PortNo, ModeList, SerialPid) -> ok when
      PortNo :: non_neg_integer(),
      ModeList :: [pin_mode()],
      SerialPid :: pid().
set_digital_port_reporting(_, [],  _SerialPid) ->
    ok;

set_digital_port_reporting(PortNo, [Mode | Tail], SerialPid) ->
    Vals = [0, 0, 0, 0, 0, 0, 0, 0],
    true = ets:insert(arduino_analog, {PortNo, Vals}),
    Command = firmata:format(set_digital_port_reporting, {PortNo, Mode}),
    send(Command, SerialPid),
    io:format("."),
    set_digital_port_reporting(PortNo + 1, Tail, SerialPid).

%%--------------------------------------------------------------------
%% @private
%% @doc set analog pin to reporting.
%% @end
%%--------------------------------------------------------------------
-spec set_analog_port_reporting(PinNoList, SerialPid) -> ok when
      PinNoList :: [non_neg_integer()],
      SerialPid :: pid().
set_analog_port_reporting([], _SerialPid) ->
    ok;

set_analog_port_reporting([PinNo | Tail], SerialPid) ->
    Command = firmata:format(set_analogin_reporting, {PinNo, 1}),
    true = ets:insert(arduino_analog, {PinNo, 0}),
    send(Command, SerialPid),
    io:format("."),
    set_analog_port_reporting(Tail, SerialPid).

%%--------------------------------------------------------------------
%% @private
%% @doc write digital value(0 or 1).
%% @end
%%--------------------------------------------------------------------
-spec digital_write(PortNo, Vals, SerialPid) -> ok when
      PortNo :: non_neg_integer(),
      Vals :: [0 | 1],
      SerialPid :: pid().
digital_write(PortNo, Vals, SerialPid) when is_list(Vals) andalso
					    length(Vals) =:= 8 ->
    Command = firmata:format(digital_io_message, {PortNo, Vals}),
    send(Command, SerialPid).

%%--------------------------------------------------------------------
%% @private
%% @doc set analog sampling interval.
%% @end
%%--------------------------------------------------------------------
-spec sampling_interval(Interval, SerialPid) -> ok when
      Interval :: non_neg_integer(),
      SerialPid :: pid().
sampling_interval(Interval, SerialPid) ->
    Command = firmata:format(sysex, sampling_interval, Interval),
    send(Command, SerialPid),
    ok.
    
