%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------

-module(pppd_mgr_options).

-export([emit/2, emit/3]).

emit(InFile, OutFile) ->
    emit(InFile, OutFile, []).

emit(InFile, OutFile, ExtraOpts) ->
    case yang:parse_file(InFile) of
	{ok, Cfg} ->
	    Data = format(Cfg,ExtraOpts),
	    file:write_file(OutFile, Data);
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
	true -> [$",as_string(Arg),$"];
	false -> as_string(Arg)
    end.

as_string(X) when is_atom(X) ->
    atom_to_list(X);
as_string(X) when is_integer(X) ->
    integer_to_list(X);
as_string(X) when is_list(X) ->
    X;
as_string(X) when is_binary(X) ->
    X.


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



