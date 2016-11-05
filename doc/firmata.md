

# Module firmata #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-1">format/1</a></td><td>create binary data that formatted by firmata protocol format.</td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td>create binary data that formatted by firmata protocol format.</td></tr><tr><td valign="top"><a href="#format-3">format/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(FormatName) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>FormatName = atom()</code></li></ul>

create binary data that formatted by firmata protocol format.

<a name="format-2"></a>

### format/2 ###

<pre><code>
format(FormatName, X2::tuple()) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>FormatName = atom()</code></li></ul>

create binary data that formatted by firmata protocol format.

<a name="format-3"></a>

### format/3 ###

`format(X1, X2, Interval) -> any()`

<a name="parse-2"></a>

### parse/2 ###

`parse(Code, Bin) -> any()`

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Code) -&gt; non_neg_integer() | in_sysex | unknown
</code></pre>

<ul class="definitions"><li><code>Code = non_neg_integer()</code></li></ul>

