%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 29 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(firmata).

%% API
-export([size/1, parse/2, format/1, format/2, format/3]).

-define(MEASURE_VERSION, 2).
-define(MINOR_VERSION,   3).

-define(DIGITAL_IO_MESSAGE_CODE,         16#90).
-define(ANALOG_IO_MESSAGE_CODE,          16#E0).
-define(SET_PIN_MODE_CODE,               16#F4).
-define(SET_ANALOG_PIN_REPORTING_CODE,   16#C0).
-define(SET_DIGITAL_PORT_REPORTING_CODE, 16#D0).
-define(SYSEX_START_CODE,                16#F0).
-define(SYSEX_END_CODE,                  16#F7).
-define(VERSION_REPORT_CODE,             16#F9).
-define(SYSTEM_RESET_CODE,               16#FF).

-define(SYSEX_CAPABILITY_QUERY,          16#6B).
-define(SYSEX_PIN_STATE_QUERY,           16#6D).
-define(SYSEX_PIN_STATE_RESPONSE,        16#6E).
-define(SYSEX_EXTENDED_ANALOG,           16#6F).
-define(SYSEX_NAME_AND_VERSION_CODE,     16#79).
-define(SYSEX_SAMPLING_INTERVAL,         16#7A).
-define(SYSEX_SERVO_CONFIG,              16#F0).

%%%===================================================================
%%% API
%%%===================================================================
-spec size(Code) -> non_neg_integer() | in_sysex | unknown when
      Code :: non_neg_integer().

%% digital message
size(Code) when 16#90 =< Code, Code =< 16#9F ->
    2;

%% analog message
size(Code) when 16#E0 =< Code, Code =< 16#EF->
    2;

%% version report
size(?VERSION_REPORT_CODE) ->
    2;

%% set pin mode
size(?SET_PIN_MODE_CODE) ->
    3;

%% toggle analogin reporting by pin
size(Code) when 16#C0 =< Code , Code =< 16#CF ->
    1;

%% toggle digital reporting by port
size(Code) when 16#D0 =< Code , Code =< 16#DF ->
    1;

%% version report
size(?VERSION_REPORT_CODE) ->
    0;

%% sysex start
size(?SYSEX_START_CODE) ->
    in_sysex;

size(Code) ->
    io:format("requested size for unknown code:~p~n", [Code]),
    unknown.

%% protocol version report
parse(?VERSION_REPORT_CODE, <<MeasureVer:8, MinorVer:8 >>) ->
    {version_report, {MeasureVer, MinorVer}};

%% digital io message
parse(Code, Bin) when 16#90 =< Code , Code =< 16#9F ->
    PortNo = Code - ?DIGITAL_IO_MESSAGE_CODE,

    <<_:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1,
      _:7, X7:1>> = Bin,

    {digital_io_message, {PortNo, [X0, X1, X2, X3, X4, X5, X6, X7]}};

%% analog io message
parse(Code, Bin) when 16#E0 =< Code , Code =< 16#EF ->
    PinNo = Code - ?ANALOG_IO_MESSAGE_CODE,

    <<_:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1,
      _:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1, X7:1 >> = Bin,

    <<Value:16/big-unsigned-integer>> = 
	<< 0:1,  0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1,
	  X7:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1 >>,
    
    {analog_io_message, {PinNo, Value}};

%% sysex command
parse(?SYSEX_START_CODE, Bin) ->
    {sysex, parse_sysex(Bin)}.

%%--------------------------------------------------------------------
%% @doc parse sysex data
%% @end
%%--------------------------------------------------------------------
-spec parse_sysex(binary()) -> {Name, term()} when
      Name :: atom().

%--------------------------------------------------------------------
% Receive Firmware Name and Version (after query)
%--------------------------------------------------------------------
%  0  START_SYSEX (0xF0)
%  1  queryFirmware (0x79)
%  2  major version (0-127)
%  3  minor version (0-127)
%  4  first 0-6 bit of firmware name
%  5  second 7bit of firmware name @fix
%  x  ...for as many bytes as it needs)
%  6  END_SYSEX (0xF7)
%--------------------------------------------------------------------
parse_sysex(<<?SYSEX_NAME_AND_VERSION_CODE:8,
	    MeasureVer:8/integer, MinorVer:8/integer, Bin/binary>>) ->
    Name = bit7_2b_to_bit8_1b(Bin),
    {name_and_version_report, {MeasureVer, MinorVer, Name}};

%--------------------------------------------------------------------
% pin state response
%--------------------------------------------------------------------
% 0  START_SYSEX (0xF0) (MIDI System Exclusive)
% 1  pin state response (0x6E)
% 2  pin (0 to 127)
% 3  pin mode (the currently configured mode)
% 4  pin state, bits 0-6
% 5  (optional) pin state, bits 7-13
% 6  (optional) pin state, bits 14-20
% ...  additional optional bytes, as many as needed
% N  END_SYSEX (0xF7)
%--------------------------------------------------------------------
parse_sysex(<<?SYSEX_PIN_STATE_RESPONSE:8,
	    PinNo:8/integer, Mode:8/integer, StateBin/binary>>) ->
    Bin8 = bit7_to_bit8(StateBin),
    BitSize = byte_size(Bin8) * 8,
    <<Val:BitSize/little-unsigned-integer>> = Bin8,
    {pin_state_response, {PinNo, Mode, Val}}.

%%--------------------------------------------------------------------
%% @doc create binary data that formatted by firmata protocol format.
%% @end
%%--------------------------------------------------------------------
-spec format(FormatName) -> binary() when
      FormatName :: atom().

%--------------------------------------------------------------------
 % request version report
%--------------------------------------------------------------------
% 0 request version report (0xF9) (MIDI Undefined)
%--------------------------------------------------------------------
format(version_report_request) ->
    <<?VERSION_REPORT_CODE:8 >>;

%--------------------------------------------------------------------
 % request system reset
%--------------------------------------------------------------------
% 0 system reset (0xFF)
%--------------------------------------------------------------------
format(syste_reset) ->
    <<?SYSTEM_RESET_CODE:8 >>;

%--------------------------------------------------------------------
% version report format
%--------------------------------------------------------------------
% 0  version report header (0xF9) (MIDI Undefined)
% 1  major version (0-127)
% 2  minor version (0-127)
%--------------------------------------------------------------------
format(version_report) ->
    <<?VERSION_REPORT_CODE:8, ?MEASURE_VERSION:8, ?MINOR_VERSION:8 >>.

%%--------------------------------------------------------------------
%% @doc create binary data that formatted by firmata protocol format.
%% @end
%%--------------------------------------------------------------------
-spec format(FormatName, tuple()) -> binary() when
      FormatName :: atom().

%--------------------------------------------------------------------
% two byte digital data format, 
% second nibble of byte 0 gives the port number
% (e.g. 0x92 is the third port, port 2)
%--------------------------------------------------------------------
% 0  digital data, 0x90-0x9F, (MIDI NoteOn, but different data format)
% 1  digital pins 0-6 bitmask
% 2  digital pin 7 bitmask 
%--------------------------------------------------------------------
format(digital_io_message, {Port, [X0, X1, X2, X3, X4, X5, X6, X7]})
  when is_integer(Port) ->
    Code = ?DIGITAL_IO_MESSAGE_CODE + Port,
    <<Code:8,
      0:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1,
      0:7, X7:1 >>;

%--------------------------------------------------------------------
% analog 14-bit data format
%--------------------------------------------------------------------
% 0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)
% 1  analog least significant 7 bits
% 2  analog most significant 7 bits
%--------------------------------------------------------------------
format(analog_io_message, {PinNo, Value}) when is_integer(PinNo),
					       is_integer(Value) ->
    <<_:1, _:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1,
      X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1 >> 
	= <<Value:16/big-unsigned-integer>>,
    
    Code = ?ANALOG_IO_MESSAGE_CODE + PinNo,

    <<Code:8,
      0:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1,
      0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1, X7:1 >>;

%--------------------------------------------------------------------
% set pin mode
%--------------------------------------------------------------------
% 1  set digital pin mode (0xF4) (MIDI Undefined)
% 2  pin number (0-127)
% 3  state (INPUT/OUTPUT/ANALOG/PWM/SERVO, 0/1/2/3/4)
%--------------------------------------------------------------------
format(set_pin_mode, {PinNo, State}) when is_integer(PinNo),
					  (State >= 0 andalso State =< 4) ->
    <<?SET_PIN_MODE_CODE:8, PinNo:8, State:8 >>;

%--------------------------------------------------------------------
% toggle analogIn reporting by pin
%--------------------------------------------------------------------
% 0  toggle analogIn reporting (0xC0-0xCF) (MIDI Program Change)
% 1  disable(0)/enable(non-zero) 
%--------------------------------------------------------------------
format(set_analogin_reporting, {PinNo, Enable}) when is_integer(PinNo),
						     is_integer(Enable) ->
    Code = ?SET_ANALOG_PIN_REPORTING_CODE + PinNo,
    <<Code:8, Enable:8 >>;

%--------------------------------------------------------------------
% toggle digital port reporting by port (second nibble of byte 0),
%   e.g. 0xD1 is port 1 is pins 8 to 15,  
%--------------------------------------------------------------------
% 0  toggle digital port reporting (0xD0-0xDF) (MIDI Aftertouch)
% 1  disable(0)/enable(non-zero) 
%--------------------------------------------------------------------
format(set_digital_port_reporting, {PortNo, Enable}) when is_integer(PortNo),
							  is_integer(Enable) ->
    Code = ?SET_DIGITAL_PORT_REPORTING_CODE + PortNo,
    <<Code:8, Enable:8 >>.

%--------------------------------------------------------------------
% Set sampling interval
%--------------------------------------------------------------------
% 0  START_SYSEX (0xF0) (MIDI System Exclusive)
% 1  SAMPLING_INTERVAL (0x7A)
% 2  sampling interval on the millisecond time scale (LSB)
% 3  sampling interval on the millisecond time scale (MSB)
% 4  END_SYSEX (0xF7)
%--------------------------------------------------------------------
format(sysex, sampling_interval, Interval) ->
    << 0:1,  0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1,
      X7:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1>> = <<Interval:16/big-integer>>,

    IntervalBin = <<0:1,  X6:1,  X5:1,  X4:1,  X3:1, X2:1, X1:1, X0:1,
		    0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1, X7:1>>,

    <<?SYSEX_START_CODE:8, 
      ?SYSEX_SAMPLING_INTERVAL:8, IntervalBin:2/binary, 
      ?SYSEX_END_CODE:8>>;

%--------------------------------------------------------------------
% capabilities query
%--------------------------------------------------------------------
% 0  START_SYSEX (0xF0) (MIDI System Exclusive)
% 1  capabilities query (0x6B)
% 2  END_SYSEX (0xF7) (MIDI End of SysEx - EOX)
%--------------------------------------------------------------------
format(sysex, capability_query, {}) ->
    <<?SYSEX_START_CODE:8, 
      ?SYSEX_CAPABILITY_QUERY:8,
      ?SYSEX_END_CODE:8>>;

%--------------------------------------------------------------------
% extended analog
%--------------------------------------------------------------------
% 0  START_SYSEX (0xF0) (MIDI System Exclusive)
% 1  extended analog message (0x6F)
% 2  pin (0 to 127)
% 3  bits 0-6 (least significant byte)
% 4  bits 7-13
% ... additional bytes may be sent if more bits needed
% N  END_SYSEX (0xF7) (MIDI End of SysEx - EOX)
%--------------------------------------------------------------------
format(sysex, extended_analog, {PinNo, Val, ByteSize}) ->
    BitSize = ByteSize * 8,
    ValBin = <<Val:BitSize/little-unsigned-integer>>,
    ValBin1 = format_extended_analog(PinNo, ValBin, []),

    <<?SYSEX_START_CODE:8, 
      ?SYSEX_EXTENDED_ANALOG:8, PinNo:8/integer, ValBin1/binary,
      ?SYSEX_END_CODE:8>>;

%--------------------------------------------------------------------
% pin state query
%--------------------------------------------------------------------
% 0  START_SYSEX (0xF0) (MIDI System Exclusive)
% 1  pin state query (0x6D)
% 2  pin (0 to 127)
% 3  END_SYSEX (0xF7) (MIDI End of SysEx - EOX)
%--------------------------------------------------------------------
format(sysex, pin_state_query, {PinNo}) ->
    <<?SYSEX_START_CODE:8, 
      ?SYSEX_PIN_STATE_QUERY:8, PinNo:8/integer,
      ?SYSEX_END_CODE:8>>;

%--------------------------------------------------------------------
% servo config query
%--------------------------------------------------------------------
%--------------------------------------------------------------------
format(sysex, servo_config, {PinNo, MinPulse, MaxPulse}) ->
    << 0:1,  0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1,
      X7:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1>> = <<MinPulse:16/big-integer>>,

    MinPulseBin = <<0:1,  X6:1,  X5:1,  X4:1,  X3:1, X2:1, X1:1, X0:1,
		    0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1, X7:1>>,

    << 0:1,  0:1, Y13:1, Y12:1, Y11:1, Y10:1, Y9:1, Y8:1,
      Y7:1, Y6:1,  Y5:1,  Y4:1,  Y3:1,  Y2:1, Y1:1, Y0:1>> = <<MaxPulse:16/big-integer>>,

    MaxPulseBin = <<0:1,  Y6:1,  Y5:1,  Y4:1,  Y3:1, Y2:1, Y1:1, Y0:1,
		    0:1, Y13:1, Y12:1, Y11:1, Y10:1, Y9:1, Y8:1, Y7:1>>,

    <<?SYSEX_START_CODE:8, 
      ?SYSEX_SERVO_CONFIG:8, 
      PinNo:8/integer, MinPulseBin:2/binary, MaxPulseBin:2/binary, 
      ?SYSEX_END_CODE:8>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 8bit unit analog data to 7bit unit analog data.
%% @end
%%--------------------------------------------------------------------
-spec format_extended_analog(PinNo, Bin, Result) -> binary() when
      PinNo :: non_neg_integer(),
      Bin :: binary(),
      Result :: list().
format_extended_analog(_PinNo, <<>>, Result) -> 
    list_to_binary(lists:reverse(Result));

format_extended_analog(PinNo, Bin, Result) -> 
    <<X7:8, X6:8, X5:8, X4:8, X3:8, X2:8, X1:8, X0:8, TailBin/binary>> = Bin,

    UnitBin = <<0:8, X6:8, X5:8, X4:8, X3:8, X2:8, X1:8, X0:8,
		0:7, X7:8>>,

    format_extended_analog(PinNo, TailBin, [UnitBin | Result]).

%%--------------------------------------------------------------------
%% @private
%% @doc 7bit(1byte)binary to 8bit(2byte) binary.
%%
%% 07654321 00000008 -> 87654321
%% @end
%%--------------------------------------------------------------------
-spec bit7_2b_to_bit8_1b(binary()) -> binary().
bit7_2b_to_bit8_1b(Bin) ->
    bit7_2b_to_bit8_1b(Bin, []).

bit7_2b_to_bit8_1b(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));

bit7_2b_to_bit8_1b(<<P1:1/binary, P2:1/binary, Tail/binary>>, Result) ->
    <<_:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>> = P1,
    <<_:7, X7:1>> = P2,

    Byte = <<X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>>,    
    bit7_2b_to_bit8_1b(Tail, [Byte | Result]).

%%--------------------------------------------------------------------
%% @private
%% @doc each 7bit binary to each 8bit binary.
%%
%% 07654321 07654321 -> 76543217654321...
%% 07654321          -> 07654321 (no change if data size is 1byte)
%% @end
%%--------------------------------------------------------------------
-spec bit7_to_bit8(binary()) -> binary().
bit7_to_bit8(Bin) when byte_size(Bin) =:= 1 ->
    Bin;

bit7_to_bit8(Bin) ->
    bit7_to_bit8(Bin, []).

bit7_to_bit8(<<>>, Result) ->
    bit7_to_bit8(lists:reverse(Result), []);

bit7_to_bit8(<<_:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1, Tail/binary>>,
	     Result) ->
    bit7_to_bit8(Tail, [X6, X5, X4, X3, X2, X1, X0 | Result]);

bit7_to_bit8([], Result) ->
    list_to_binary(lists:reverse(Result));

bit7_to_bit8([X0, X1, X2, X3, X4, X5, X6, X7 | Tail], Result) ->
    bit7_to_bit8(Tail, 
		 [<<X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>> | Result]).

