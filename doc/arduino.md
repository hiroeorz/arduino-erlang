

# Module arduino #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_analog-0">all_analog/0</a></td><td>get analog state.</td></tr><tr><td valign="top"><a href="#all_digital-0">all_digital/0</a></td><td>get digital state.</td></tr><tr><td valign="top"><a href="#analog_write-2">analog_write/2</a></td><td>write analog value.</td></tr><tr><td valign="top"><a href="#cast-1">cast/1</a></td><td>send request to arduino.</td></tr><tr><td valign="top"><a href="#digital_write-2">digital_write/2</a></td><td>write ON or OFF to a digital port (8 pins).</td></tr><tr><td valign="top"><a href="#firmata_version_request-0">firmata_version_request/0</a></td><td>get firmata version from arduino.</td></tr><tr><td valign="top"><a href="#initialize-0">initialize/0</a></td><td>(re)initialize all pins.</td></tr><tr><td valign="top"><a href="#servo_config-3">servo_config/3</a></td><td>write servo config.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_analog-0"></a>

### all_analog/0 ###

<pre><code>
all_analog() -&gt; [non_neg_integer()]
</code></pre>
<br />

get analog state.

<a name="all_digital-0"></a>

### all_digital/0 ###

<pre><code>
all_digital() -&gt; [0 | 1]
</code></pre>
<br />

get digital state.

<a name="analog_write-2"></a>

### analog_write/2 ###

<pre><code>
analog_write(PinNo, Val) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li></ul>

write analog value.

<a name="cast-1"></a>

### cast/1 ###

<pre><code>
cast(Bin::binary()) -&gt; ok
</code></pre>
<br />

send request to arduino

<a name="digital_write-2"></a>

### digital_write/2 ###

<pre><code>
digital_write(PortNo, Vals) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PortNo = non_neg_integer()</code></li><li><code>Vals = [0 | 1]</code></li></ul>

write ON or OFF to a digital port (8 pins).

<a name="firmata_version_request-0"></a>

### firmata_version_request/0 ###

<pre><code>
firmata_version_request() -&gt; ok
</code></pre>
<br />

get firmata version from arduino.

<a name="initialize-0"></a>

### initialize/0 ###

<pre><code>
initialize() -&gt; ok
</code></pre>
<br />

(re)initialize all pins.

<a name="servo_config-3"></a>

### servo_config/3 ###

<pre><code>
servo_config(PinNo, MinPulse, MaxPulse) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>MinPulse = non_neg_integer()</code></li><li><code>MaxPulse = non_neg_integer()</code></li></ul>

write servo config.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

Starts the server

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Conf) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Conf = [tuple()]</code></li></ul>

