%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------

-module(pppd_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% debug
-export([start/0]).
-export([find_provider_pppd/1]).
-compile(export_all).

-define(SERVER, ?MODULE).

-define(PPPD, "/usr/sbin/pppd").
-define(LINK_NAME, "ppp0").

-type pppd_status() :: final1 | down | init0 | init1 | up.

-record(state,
	{
	  link,      %% default link name
	  status   :: pppd_status(),
	  netmon   :: reference(),  %% netlink subscription
	  provider :: string(),     %% current provider
	  port     :: port(),       %% port while starting pppd
	  unix_pid :: string()      %% pid of pppd
	}).

-export([on/1, off/0, attach/1]).
-export([status/0]).

%%%===================================================================
%%% API
%%%===================================================================

on(Provider) ->
    gen_server:call(?SERVER, {on,Provider}).

off() ->
    gen_server:call(?SERVER, off).

attach(Provider) ->
    gen_server:call(?SERVER, {attach,Provider}).

status() ->
    gen_server:call(?SERVER, status).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    netlink_start(),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    case os:type() of
	{unix,linux} ->
	    {ok, #state{ link = ?LINK_NAME,   %% configure me! (env?)
			 status = down
		       }};
	{unix,darwin} ->
	    {ok, #state{ link = ?LINK_NAME,   %% configure me! (env?)
			 status = down
		       }};
	_ ->
	    {stop, {error,not_supported}}
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
handle_call({on,Provider}, _From, State) ->
    if State#state.status =:= down ->
	    %% FIXME: attach existing if possible !!!
	    CfgFile = filename:join(code:priv_dir(pppd_mgr),
				    Provider++".cfg"),
	    PPPFile = filename:join(["/","tmp","ppp-"++Provider]),
	    Res = pppd_mgr_options:emit(CfgFile, PPPFile),
	    io:format("pppd_mgr_options = ~p\n", [Res]),
	    Port =
		open_port({spawn_executable, ?PPPD},
			  [{arg0,?PPPD},
			   {args,["file", PPPFile]},exit_status]),
	    {reply, ok, State#state { port=Port,
				      provider=Provider,
				      status=init0 }};
       true ->
	    io:format("pppd already runnig ~w ~s\n",
		      [State#state.status, State#state.provider]),
	    {reply, {error,not_down}, State}
    end;

handle_call({attach,Provider}, _From, State) ->
    if State#state.status =:= down ->
	    case find_provider_pppd(Provider) of
		[] ->
		    {reply, {error,enoent}, State};
		[UPid] ->
		    io:format("pppd found ~p\n", [UPid]),
		    State1 = netlink_unsubscribe(State),
		    State2 = netlink_subscribe(State1),
		    {reply, {ok,UPid},
		     State2#state { status=init1,
				    provider = Provider,
				    unix_pid = UPid,
				    port=undefined}}
	    end;
       true ->
	    {reply, {error,not_down}, State}
    end;

handle_call(off, _From, State) ->
    if State#state.status =:= up;
       State#state.status =:= init1 ->
	    os:cmd("/bin/kill -HUP " ++ State#state.unix_pid),
	    {reply, ok, State#state { status=final1,
				      provider = undefined,
				      unix_pid = undefined }};
       true ->
	    {reply, {error,not_running}, State}
    end;
handle_call(status, _From, State) ->
    {reply, State#state.status, State};
handle_call(_, _From, State) ->
    {reply, {error,bad_call}, State}.


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
handle_cast(_Msg, State) ->
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
handle_info({Port,{exit_status,Status}}, State)
  when Port =:= State#state.port ->
    if Status =:= 0 ->
	    io:format("exit_status = 0\n", []),
	    timer:sleep(1000), %% why?
	    State1 = netlink_unsubscribe(State),
	    case find_provider_pppd(State1#state.provider) of
		[] ->
		    io:format("BUG: pppd not found\n", []),
		    {noreply, State1#state { status=down,
					     port=undefined}};
		[UPid] ->
		    io:format("pppd found ~p\n", [UPid]),
		    State2 = netlink_subscribe(State1),
		    {noreply, State2#state { status=init1,
					     unix_pid = UPid,
					     port=undefined}}
	    end;
       true ->
	    io:format("exit_status = ~w\n", [Status]),
	    %% wait for ppp0 to be in state up!
	    {noreply, State#state { status=down, port=undefined}}
    end;
handle_info(_NL={netlink,_Ref,?LINK_NAME,Field,Old,New},State) ->
    io:format("Netlink state changed ~p\n", [_NL]),
    if State#state.status =:= init1, Field =:= flags ->
	    io:format("INIT1 test\n", []),
	    case (is_list(New) andalso lists:member(up, New)) of
		true ->
		    io:format("UP\n", []),
		    {noreply, State#state { status=up }};
		false ->
		    {noreply, State}
	    end;
       State#state.status =:= final1, Field =:= flags ->
	    io:format("FINAL1 test\n", []),
	    case (New =:= undefined) orelse
		(is_list(New) andalso (not lists:member(up, New))) of
		true ->
		    io:format("DOWN\n", []),
		    State1 = netlink_unsubscribe(State),
		    {noreply, State1#state { status=down }};
		false ->
		    {noreply, State}
	    end;
       Field =:= address, Old =:= undefined, is_tuple(New) ->
	    io:format("PPP got address: ~w\n", [New]),
	    {noreply, State};	    
       true ->
	    {noreply, State}
    end;
handle_info(_Info, State) ->
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
%%% Internal functions
%%%===================================================================

netlink_start() ->
    case os:type() of
	{unix,linux} ->
	    netlink:start();
	_ ->
	    %% no netlink here
	    ok
    end.

netlink_subscribe(State) ->
    case os:type() of
	{unix,linux} ->
	    case netlink:subscribe(State#state.link,[flags,address],[flush]) of
		{ok,Mon} ->
		    State#state { netmon = Mon };
		_Error ->
		    io:format("netlink_subscriber error ~p\n", [_Error]),
		    State
	    end;
	_ ->
	    State
    end.

netlink_unsubscribe(State) ->
    if is_reference(State#state.netmon) ->
	    netlink:unsubscribe(State#state.netmon),
	    State#state { netmon = undefined };
       true ->
	    State
    end.

%% Find unix process pid of pppd running Provider
find_provider_pppd(Provider) ->
    lists:filter(
      fun(P) ->
	      Cmd = command(P),
	      case string:tokens(Cmd, " \t\n") of
		  [?PPPD,"file","/tmp/ppp-"++Provider] ->
		      true;
		  _ ->
		      false
	      end
      end, pidof("pppd")).

command(Pid) ->
    case os:type() of
	{unix,Name} when Name =:= linux; Name =:= darwin ->
	    os:cmd("ps -o command= "++Pid);
	_ ->
	    ""
    end.

pidof(What) ->
    Result = 
	case os:type() of
	    {unix,Name} when Name=:=linux; Name=:=darwin ->
		os:cmd("ps axc|awk '{if ($5==\""++What++"\") print $1}'");
	    _ ->
		""
	end,
    string:tokens(Result, " \t\n").
