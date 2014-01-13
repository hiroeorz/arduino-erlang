%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(arduino_event).

%% API
-export([start_link/1, add_handler/2, delete_handler/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates an event manager
%% @end
%%--------------------------------------------------------------------
-spec start_link(Handlers) -> {ok, Pid} | {error, Error} when
      Handlers :: [{atom(), list()}],
      Pid :: pid(),
      Error :: term().
start_link(Handlers) ->
    case gen_event:start_link({local, ?SERVER}) of
	{ok, Pid} ->
	    ok = lists:foreach(fun({H, Args}) -> 
				       add_handler(H, Args)
			       end, Handlers),
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Add an event handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler(Module, Args) -> ok | {'EXIT', Reason} | term() when
      Module :: atom(),
      Args :: [term()],
      Reason :: term().
add_handler(Module, Args) ->
    gen_event:add_handler(?SERVER, Module, Args).

%%--------------------------------------------------------------------
%% @doc Delete an event handler
%% @end
%%--------------------------------------------------------------------
-spec delete_handler(Module) -> ok | {'EXIT', Reason} | term() when
      Module :: atom(),
      Reason :: term().
delete_handler(Module) ->
    gen_event:delete_handler(?SERVER, Module, []).

