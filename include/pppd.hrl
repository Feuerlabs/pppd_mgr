%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Defines needed for pppd_mgr.
%%%
%%% Created : June 2013 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-ifndef(PPPD_HRL).
-define(PPPD_HRL, true).

-define(PPPD_ON_TIME,  20000).
-define(PPPD_OFF_TIME, 10000).
-define(PPPD_ATTACH_TIME, 2000).
-define(PPPD_IDLE_TIME,  60000). %% ??

%% Convenience defines 
-ifndef(ee).
-define(ee(String, List), error_logger:error_msg(String, List)).
-endif.
-ifndef(ei).
-define(ei(String, List),  error_logger:info_msg(String, List)).
-endif.

-endif.
