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

-include("pppd.hrl").

%% API
-export([start_link/0]).
-export([on/1, off/0, attach/1]).
-export([subscribe/0, unsubscribe/0]).
-export([status/0,
	 ppp_parameter/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% debug
-export([start/0]).
-export([find_provider_pppd/1]).
-compile(export_all).

-define(SERVER, ?MODULE).

-define(PPPD, "/usr/sbin/pppd").
-define(LINK_NAME, "ppp0").

-type pppd_status() :: final | down | init | up.
-type posix() :: atom().
-type ppp_parameter() :: ppp_on_time | ppp_off_time | ppp_idle_time.

-record(state,
	{
	  link,      %% default link name
	  status   :: pppd_status(),
	  tmr      :: undefined | reference(), %% timer 
	  netmon   :: reference(),  %% netlink subscription
	  provider :: string(),     %% current provider
	  port     :: port(),       %% port while starting pppd
	  unix_pid :: string(),     %% pid of pppd
	  subs = []::list()         %% subscriber list %% Monitoring needed ??
	}).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%%   Start or attach an existing ppp connection using
%%   the name Provider. The `Provider' must exist as a ppp
%%   options config in pppd_mgr/priv/<`Provider'>.cfg, this
%%   cfg file is in YANG format.
%% @end
-spec on(Provider::string()) -> ok | {error,posix()}.

on(Provider) ->
    gen_server:call(?SERVER, {on,Provider}).

%% @doc
%%   Stop an existing ppp connection.
%% @end
-spec off() -> ok | {error,posix()}.
off() ->
    gen_server:call(?SERVER, off).

%% @doc
%%   Attach an existing ppp connection using the name `Provider'.
%% @end
-spec attach(Provider::string()) -> ok | {error,posix()}.
attach(Provider) ->
    gen_server:call(?SERVER, {attach,Provider}).

%% @doc
%%   Fetch pppd_mgr current status
%% @end
-spec status() -> pppd_status().

status() ->
    gen_server:call(?SERVER, status).

%% @doc
%%   Subscribe to changes of ppp connection.
%% @end
-spec subscribe() -> ok | {error,posix()}.
subscribe() ->
    gen_server:call(?SERVER, subscribe).

%% @doc
%%   Unsubscribe to changes of ppp connection.
%% @end
-spec unsubscribe() -> ok | {error,posix()}.
unsubscribe() ->
    gen_server:call(?SERVER, unsubscribe).

%% @doc
%%   Fetch ppp parameters.
%% @end
-spec ppp_parameter(ppp_parameter()) -> term().
ppp_parameter(ppp_on_time) -> ?PPPD_ON_TIME;
ppp_parameter(ppp_off_time) ->?PPPD_OFF_TIME;
ppp_parameter(ppp_idle_time) ->?PPPD_IDLE_TIME;
ppp_parameter(_) -> undefined.
     
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
	    case find_provider_pppd(Provider) of
		[] ->
		    CfgFile = filename:join(code:priv_dir(pppd_mgr),
					    Provider++".cfg"),
		    PPPFile = filename:join(["/","tmp","ppp-"++Provider]),
		    Res = pppd_mgr_options:emit(CfgFile, PPPFile),
		    io:format("pppd_mgr_options = ~p\n", [Res]),
		    Port =
			open_port({spawn_executable, ?PPPD},
				  [{arg0,?PPPD},
				   {args,["file", PPPFile]},exit_status]),
		    Tmr = erlang:start_timer(?PPPD_ON_TIME, self(), up),
		    {reply, ok, State#state { port=Port,
					      tmr = Tmr,
					      provider=Provider,
					      status=init }};
		[UPid] ->
		    io:format("pppd found ~p\n", [UPid]),
		    State1 = netlink_unsubscribe(State),
		    State2 = netlink_subscribe(State1),
		    Tmr = erlang:start_timer(?PPPD_ATTACH_TIME, self(), attach),
		    {reply, ok,
		     State2#state { status=init,
				    tmr = Tmr,
				    provider = Provider,
				    unix_pid = UPid,
				    port=undefined}}
	    end;
       State#state.status =:= up ->
	    {reply, {error,ealready}, State};
       true ->
	    io:format("pppd not ready ~w ~s\n",
		      [State#state.status, State#state.provider]),
	    {reply, {error,ebusy}, State}
    end;

handle_call(off, _From, State) ->
    if State#state.status =:= up;
       State#state.status =:= init ->
	    io:format("off: killing ~p\n", [State#state.unix_pid]),
	    kill_pppd(State#state.unix_pid),
	    Tmr = erlang:start_timer(?PPPD_OFF_TIME, self(), down),
	    {reply, ok, State#state { status=final,
				      tmr = Tmr,
				      provider = undefined,
				      unix_pid = undefined }};
       true ->
	    {reply, {error,not_running}, State}
    end;
%% try attach to possibly existing provider 
handle_call({attach,Provider}, _From, State) ->
    if State#state.status =:= down ->
	    case find_provider_pppd(Provider) of
		[] ->
		    {reply, {error,enoent}, State};
		[UPid] ->
		    io:format("pppd found ~p\n", [UPid]),
		    State1 = netlink_unsubscribe(State),
		    State2 = netlink_subscribe(State1),
		    Tmr = erlang:start_timer(?PPPD_ATTACH_TIME, self(), attach),
		    {reply, {ok,UPid},
		     State2#state { status=init,
				    tmr = Tmr,
				    provider = Provider,
				    unix_pid = UPid,
				    port=undefined}}
	    end;
       State#state.status =:= up ->
	    {true, {error,ealready}, State};
       true ->
	    {reply, {error,not_down}, State}
    end;

handle_call(status, _From, State) ->
    {reply, State#state.status, State};

handle_call(subscribe, {Pid, _Tag}, State=#state {subs = Subs}) ->
    {reply, ok, State#state {subs = lists:usort([Pid | Subs])}};
handle_call(unsubscribe, {Pid, _Tag}, State=#state {subs = Subs}) ->
    {reply, ok, State#state {subs = Subs -- [Pid]}};

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
		    inform_subscribers(down, State),
		    {noreply, State1#state { status=down,
					     port=undefined}};
		[UPid] ->
		    io:format("pppd found ~p\n", [UPid]),
		    State2 = netlink_subscribe(State1),
		    {noreply, State2#state { status=init,
					     unix_pid = UPid,
					     port=undefined}}
	    end;
       true ->
	    io:format("exit_status = ~w\n", [Status]),
	    %% wait for ppp0 to be in state up!
	    inform_subscribers(down, State),
	    {noreply, State#state { status=down, port=undefined}}
    end;
handle_info(_NL={netlink,_Ref,?LINK_NAME,Field,Old,New},State) ->
    io:format("Netlink state changed ~p\n", [_NL]),
    if State#state.status =:= init, Field =:= flags ->
	    io:format("INIT test\n", []),
	    case (is_list(New) andalso lists:member(up, New)) of
		true ->
		    cancel_timer(State#state.tmr),
		    io:format("UP\n", []),
		    inform_subscribers(up, State),
		    {noreply, State#state { status=up, tmr=undefined }};
		false ->
		    {noreply, State}
	    end;
       State#state.status =:= final, Field =:= flags ->
	    io:format("FINAL test\n", []),
	    case (New =:= undefined) orelse
		(is_list(New) andalso (not lists:member(up, New))) of
		true ->
		    cancel_timer(State#state.tmr),
		    io:format("DOWN\n", []),
		    State1 = netlink_unsubscribe(State),
		    inform_subscribers(down, State),
		    {noreply, State1#state { status=down, tmr=undefined }};
		false ->
		    {noreply, State}
	    end;
       Field =:= local, Old =:= undefined, is_tuple(New) ->
	    io:format("PPP got address: ~w\n", [New]),
	    {noreply, State};	    
       true ->
	    {noreply, State}
    end;
handle_info({timeout,Ref,down}, State) when State#state.tmr =:= Ref ->
    if State#state.status =:= final ->
	    %% something is wrong, but we need to assume ppp is down
	    inform_subscribers(down, State),
	    {noreply, State#state { status = down, tmr = undefined }};
       true ->
	    {noreply, State}
    end;
handle_info({timeout,Ref,up}, State) when State#state.tmr =:= Ref ->
    if State#state.status =:= init ->
	    %% interface is not coming up
	    io:format("up timeout: killing ~p\n", [State#state.unix_pid]),
	    kill_pppd(State#state.unix_pid),
	    inform_subscribers(down, State),
	    {noreply, State#state { status = down, tmr = undefined }};
       true ->
	    {noreply, State}
    end;
handle_info({timeout,Ref,attach}, State) when State#state.tmr =:= Ref ->
    if State#state.status =:= init ->
	    %% interface is not coming up
	    io:format("attach timeout: killing ~p\n", [State#state.unix_pid]),
	    kill_pppd(State#state.unix_pid),
	    inform_subscribers(down, State),
	    {noreply, State#state { status = down, tmr = undefined }};
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

kill_pppd(undefined) ->
    ok;
kill_pppd(Pid) ->
    os:cmd("/bin/kill -9 " ++ Pid).

cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    erlang:cancel_timer(Ref),
    receive
	{timeout,Ref,_} ->
	    ok
    after 0 ->
	    ok
    end.

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
	    case netlink:subscribe(State#state.link,[{link,flags},local],[flush]) of
		{ok,Mon} ->
		    State#state { netmon = Mon };
		_Error ->
		    io:format("netlink_subscriber error ~p\n", [_Error]),
		    State
	    end;
	{unix,darwin} ->
	    case fake_netlink_subscribe(State#state.link,[{link,flags},local],[flush]) of
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
	    case os:type() of
		{unix,linux} ->
		    netlink:unsubscribe(State#state.netmon),
		    State#state { netmon = undefined };
		{unix,darwin} ->
		    fake_netlink_unsubscribe(State#state.netmon),
		    State#state { netmon = undefined };
		_ ->
		    State
	    end;
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

%%
%% Fake netlink by doing a bit of polling
%%
fake_netlink_subscribe(IfName,_Fs,_Flags) ->
    Caller = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
			fake_netlink(Caller,Ref,IfName,undefined,undefined,2000)
		end),
    put(fake_netlink, Pid),
    {ok,Ref}.

fake_netlink_unsubscribe(Ref) ->
    Pid = get(fake_netlink),
    if is_pid(Pid) ->
	    Pid ! {unsubscribe, Ref},
	    erase(fake_netlink_pid),
	    ok;
       true ->
	    ok
    end.

fake_netlink(Caller,Ref,IfName,Fs0,Addr0,PollTime) ->
    case inet:ifget(IfName, [flags,addr]) of
	{ok,[{flags,Fs1},{addr,Addr1}]} ->
	    if Fs0 =/= Fs1 ->
		    Caller ! {netlink,Ref,IfName,flags,Fs0,Fs1};
	       true ->
		    ok
	    end,
	    if Addr0 =/= Addr1 ->
		    Caller ! {netlink,Ref,IfName,local,Addr0,Addr1};
	       true ->
		    ok
	    end,
	    fake_netlink_wait(Caller,Ref,IfName,Fs1,Addr1,PollTime);
	{ok,[{flags,Fs1}]} ->
	    if Fs0 =/= Fs1 ->
		    Caller ! {netlink,Ref,IfName,flags,Fs0,Fs1};
	       true ->
		    ok
	    end,
	    if Addr0 =/= undefined ->
		    Caller ! {netlink,Ref,IfName,local,Addr0,undefined};
	       true ->
		    ok
	    end,
	    fake_netlink_wait(Caller,Ref,IfName,Fs1,undefined,PollTime);
	_Error ->
	    io:format("error = ~p\n", [_Error]),
	    fake_netlink_wait(Caller,Ref,IfName,Fs0,Addr0,PollTime)
    end.

fake_netlink_wait(Caller,Ref,IfName,Fs0,Addr0,PollTime) ->
    receive
	{unsubscribe,Ref} ->
	    ok
    after PollTime ->
	    fake_netlink_wait(Caller,Ref,IfName,Fs0,Addr0,PollTime)
    end.

inform_subscribers(Msg, _State=#state {subs = Subs}) ->
    lists:foreach(
      fun(Pid) when is_pid(Pid) -> Pid ! Msg;
	 (_) -> ok
      end, 
      Subs).
