

# Module arduino_event #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_handler-2">add_handler/2</a></td><td>Add an event handler.</td></tr><tr><td valign="top"><a href="#delete_handler-1">delete_handler/1</a></td><td>Delete an event handler.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Creates an event manager.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_handler-2"></a>

### add_handler/2 ###

<pre><code>
add_handler(Module, Args) -&gt; ok | {'EXIT', Reason} | term()
</code></pre>

<ul class="definitions"><li><code>Module = atom()</code></li><li><code>Args = [term()]</code></li><li><code>Reason = term()</code></li></ul>

Add an event handler

<a name="delete_handler-1"></a>

### delete_handler/1 ###

<pre><code>
delete_handler(Module) -&gt; ok | {'EXIT', Reason} | term()
</code></pre>

<ul class="definitions"><li><code>Module = atom()</code></li><li><code>Reason = term()</code></li></ul>

Delete an event handler

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Handlers) -&gt; {ok, Pid} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Handlers = [{atom(), list()}]</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Creates an event manager

