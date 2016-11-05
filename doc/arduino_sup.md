

# Module arduino_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Starts the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Config, EventHandlers) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Config = [tuple()]</code></li><li><code>EventHandlers = [atom()]</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Starts the supervisor

Config example:
{arduino, [{speed, 57600},
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
]}

EventHandlers example:
[ {my_handler, []}, {your_handler, [Arg1, Arg2]} ]

