%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Write PPPD options from internal format
%%% @end
%%% Created : 13 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(pppd_mgr_options).

-export([emit/2, emit/3]).

emit(InFile, OutFile) ->
    emit(InFile, OutFile, []).

emit(InFile, OutFile, ExtraOpts) ->
    case yang:parse_file(InFile) of
	{ok, Cfg} ->
	    file:write_file(OutFile, format(Cfg,ExtraOpts));
	Error ->
	    Error
    end.

format([{<<"ppp-config">>, _Line, _Arg, Stmts}], Opts) ->
    format_ppp(Stmts++Opts);
format(_,_Opts) ->
    {error, bad_config}.

format_ppp([{<<"uart-config">>, _Line, _Arg, Stmts}|Tail]) ->
    [format_uart(Stmts) | format_ppp(Tail)];
format_ppp([{Option, _Line, <<>>, []}|Tail]) ->
    [ Option, "\n" | format_ppp(Tail)];
format_ppp([{Option, _Line, Arg, []}|Tail]) ->
    [ Option," ",format_arg(Option,Arg),"\n" | format_ppp(Tail)];
format_ppp([]) ->	    
    [].

format_uart([{<<"device">>, _Line, Arg, []}|Tail]) ->
    [Arg, "\n" | format_uart(Tail)];
format_uart([{<<"baud">>, _Line, Arg, []}|Tail]) ->
    [Arg, "\n" | format_uart(Tail)];
format_uart([]) ->
    [].

format_arg(Option,Arg) ->
    case is_string_arg(Option) of
	true -> [$",Arg,$"];
	false -> Arg
    end.

is_string_arg(<<"connect">>) -> true;
is_string_arg(<<"disconnect">>) -> true;
is_string_arg(<<"init">>) -> true;
is_string_arg(<<"ipparam">>) -> true;
is_string_arg(<<"ipx-router-name">>) -> true;
is_string_arg(<<"linkname">>) -> true;
is_string_arg(<<"logfile">>) -> true;
is_string_arg(<<"name">>) -> true;
is_string_arg(<<"password">>) -> true;
is_string_arg(<<"plugin">>) -> true;
is_string_arg(<<"pty">>) -> true;
is_string_arg(<<"record">>) -> true;
is_string_arg(<<"srp-pn-secret">>) -> true;
is_string_arg(<<"user">>) -> true;
is_string_arg(<<"welcome">>) -> true;
is_string_arg(_) -> false.


    
