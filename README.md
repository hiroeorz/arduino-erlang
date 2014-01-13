# arduino-erlang

## Goals

* Provide Arduino controller from Erlang/OTP Application.

## Getting Started

Fetch the latest version of arduino-erlang using git.

```
$ git clone https://github.com/hiroeorz/arduino-erlang.git
$ cd arduino-elrang
$ make
```
Or add "deps" line to your app's rebar.conf.

```erlang
{arduino, ".*", {git, "https://github.com/hiroeorz/arduino-erlang.git",
   {branch, "master"}}},

```

and get deps

```
$ ./rebar get-deps
```

## Running

You can read write Arduino I/O throuh the Firmata protocol.
You need to send sketch 'Standard Firmata' to your Arduino or implementation Firmata Protocol your own, before start up the arduino-erlang.

In sample setting, gpio 0,1,2,3,4,5,6,7 is digital input mode,
   8, 9 is digital output mode,
  10,11 is pwm,
  12,13 is servo.

### Start Erlang/OTP node.

```
$ erl  -boot start_sasl -pa ebin deps/*/ebin
```

```erl-sh
1> application:load(arduino).
2> arduino_sup:start_link().
```

When application started, receive analog values (pin 0 - 5) every 290msec, and When digital status changed, you receive digital change report like:

```erlang
digital changed(port:0): [1,1,1,0,1,1,1,1]
```

arduino_sup:start_link/0 is start with sample setting, If you want to start supervisor with your setting, you can start supervisor with your own setting like:

```erlang
Config = [{speed, 57600},
          {device, "/dev/ttyACM0"},
          {sampling_interval, 290},
          {digital_port_reporting, [1, 0]},
          {digital_port_offset, 1},
          {analog_offset, 0},
          {analog, [0, 1, 2, 3, 4, 5] },
          {digital, [
                     { 0, in, [{pull, up} ]},
                     { 1, in, [{pull, up} ]},
                     { 2, in, [{pull, up} ]},
                     { 3, in, [{pull, up} ]},
                     { 4, in, [{pull, up} ]},
                     { 5, in, [{pull, up} ]},
                     { 6, in, [{pull, up} ]},
                     { 7, in, [{pull, up} ]},
                     { 8, out},
                     { 9, out},
                     {10, pwm},
                     {11, pwm},
                     {12, servo},
                     {13, servo}
                    ]
          }
         ],

EventHandlers = [ {your_handler, []} ],

arduino_sup:start_link(Config, EventHandlers).
```
 
### Get all digital state.

```erl-sh
1> arduino:all_digital()
[1,1,0,0,0,0,0,1]
```

### Get all analog value.

```erl-sh
1> arduino:all_analog().
[343, 211, 375, 111, 0, 343]
```

### Write digital state.

PortNO is unit number of every 8 bit digital status.

```erl-sh
1> arduino:digital_write(0, [1,1,1,1,1,1,1,1]).
ok
```

### Add event handler of arduino

```erl-sh
1> arduino_event:add_event_handler(sample_event_handler, []).
ok
```

* First argument is module name.
* Second argument is arguments of sample_event_handler:init/1.

The sample_event_handler is event handler befavior of gen_event.
[sample_event_handler.erl](https://github.com/hiroeorz/arduino-erlang/blob/master/src/sample_event_handler.erl)
