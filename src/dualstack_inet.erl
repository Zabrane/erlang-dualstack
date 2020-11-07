% -*- mode: erlang; indent-tabs-mode: t; tab-width: 4 -*-
% vim: set noet ts=8 sw=8 tw=8
%% Copyright (c) Jordan Bracco <href@random.sh>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(dualstack_inet).

-export([getaddrs/3]).

-ifdef(TESTING).
-export([getaddrs/5]).
-endif.

-type ip_address() :: {inet:address_family(),
					   inet:ip_address(),
					   undefined | inet:port_number(),
					   Priority :: non_neg_integer(),
					   Weight :: non_neg_integer()}.
-export_type[ip_address/0].

-spec getaddrs(inet:ip_address() | inet:hostname(), [dualstack_tcp:connect_option()], timeout())
	  -> {ok, [ip_address()]} | {error, inet:posix()}.
getaddrs({_, _, _, _} = Inet, _, _) ->
  {ok, [{inet, Inet, undefined, 0, 0}]};
getaddrs({_, _, _, _, _, _, _, _} = Inet6, _, _) ->
  {ok, [{inet6, Inet6, undefined, 0, 0}]};
getaddrs({local, _} = Local, _, _) ->
  {ok, [{local, Local, undefined, 0, 0}]};
getaddrs({srv, Name}, Opts, Timeout) ->
  expand_names(srv, Name, Opts, Timeout);
getaddrs({mx, Name}, Opts, Timeout) ->
  expand_names(mx, Name, Opts, Timeout);
getaddrs(Address, Opts, Timeout) ->
  Families = families(Opts),
  Timer = inet:start_timer(Timeout),
  try
	getaddrs(Address, Families, Timer, [], {error, nxdomain})
  after
	_ = inet:stop_timer(Timer)
  end.

getaddrs(Address, [{Family, Mod} | Families], Timer, Acc, _) ->
  case Mod:getaddrs(Address, Timer) of
	{ok, IPs0} ->
	  IPs = [{Family, IP, undefined, 0, 0} || IP <- IPs0],
	  getaddrs(Address, Families, Timer, [IPs | Acc], undefined);
	Error ->
	  getaddrs(Address, Families, Timer, Acc, Error)
  end;
getaddrs(_, [], _, [_ | _] = Acc, _) ->
  {ok, lists:flatten(lists:reverse(Acc))};
getaddrs(_, [], _, [], Error) ->
  Error.

expand_names(Class, Name, Opts, Timeout) ->
  Mod = inet_res_module(Opts),
  case Mod:lookup(Name, in, Class, [], Timeout) of
	[_ | _] = Srvs ->
	  expand_names(Class, Srvs, [], Opts, Timeout, {error, nxdomain});
	_ -> {error, nxdomain}
  end.

%exppand_names(mx, [{Priority, Name}|Names], Acc, Opts, Timeout, Error) ->
%  expand_names(srv, [{Priority, undefined, undefined, Name}|Names], Acc, Opts, Timeout, Error);
expand_names(srv, [{Priority, Weight, Port, Name}|Names], Acc, Opts, Timeout, Error) ->
  case getaddrs(Name, Opts, Timeout) of
	{ok, IPs0} ->
	  IPs = [{F, IP, Port, Priority, Weight} || {F, IP, _, _, _} <- IPs0],
	  expand_names(srv, Names, [IPs | Acc], Opts, Timeout, Error);
	Error ->
	  expand_names(srv, Names, Acc, Opts, Timeout, Error)
  end;
expand_names(_, [], [_ | _] = Acc, _, _, _) ->
  {ok, lists:flatten(lists:reverse(Acc))};
expand_names(_, [], [], _, _, Error) ->
  Error.

-ifdef(TEST).
expand_test() ->
  Jabber = [{inet6,{9733,55808,21026,21097,0,0,2,2},5269,30,30},
		   {inet,{208,68,163,218},5269,30,30}],
  {ok, Jabber} = expand_names(srv, "_xmpp-server._tcp.jabber.org", [], 500),
  {error, nxdomain} = expand_names(srv, "_tcp._xmpp-server.jabber.org", [], 500),
  %FastMail = [{'in1-smtp.messagingengine.com'}, {20, 'in2-smtp.messagingengine.com'}],
  %{ok, FastMail} = expand_names(mx, "fastmail.com", [], 500),
  ok.

getaddrs_test() ->
  Inet = {127, 0, 0, 1},
  Inet6 = {0, 0, 0, 0, 0, 0, 0, 1},
  Local = {local, '/somewhere'},
  {ok, [{inet, Inet, undefined, 0, 0}]} = getaddrs(Inet, [], 100),
  {ok, [{inet6, Inet6, undefined, 0, 0}]} = getaddrs(Inet6, [], 100),
  {ok, [{local, Local, undefined, 0, 0}]} = getaddrs(Local, [], 100),
  FInet = {inet, fake_inet},
  FInet6 = {inet6, fake_inet6},
  {ok, [{inet, Inet, _, _, _}, {inet6, Inet6, _, _, _}]} = getaddrs('local-dualstack', [FInet, FInet6], undefined, [], undefined),
  {ok, [{inet6, Inet6, _, _, _}, {inet, Inet, _, _, _}]} = getaddrs('local-dualstack', [FInet6, FInet], undefined, [], undefined),
  {ok, [{inet, Inet, _, _, _}]} = getaddrs('local-dualstack', [FInet], undefined, [], undefined),
  {ok, [{inet6, Inet6, _, _, _}]} = getaddrs('local-dualstack', [FInet6], undefined, [], undefined),
  {ok, [{inet, Inet, _, _, _}]} = getaddrs('local-inet', [FInet6, FInet], undefined, [], undefined),
  {ok, [{inet6, Inet6, _, _, _}]} = getaddrs('local-inet6', [FInet6, FInet], undefined, [], undefined),
  ok.
-endif.

families(Opts) ->
  Families = pick_families(proplists:get_value(dual_stack, Opts, true), get_opts_family(Opts)),
  Test = proplists:get_value(use_testing_inet, Opts, false),
  [{F, tcp_module(Test, F)} || F <- Families].

pick_families(true, undefined) -> [inet6, inet];
pick_families(prefer_ipv4, undefined) -> [inet, inet6];
pick_families(_, undefined) -> [inet];
pick_families(_, Family) -> [Family].

get_opts_family([inet | _]) -> inet;
get_opts_family([inet6 | _]) -> inet6;
get_opts_family([local | _]) -> local;
get_opts_family([_ | Opts]) -> get_opts_family(Opts);
get_opts_family([]) -> undefined.

-ifdef(TESTING).
tcp_module(true, inet) -> fake_inet;
tcp_module(true, inet6) -> fake_inet6;
tcp_module(true, local) -> local_tcp;
tcp_module(_, Arg) -> tcp_module_(Arg).
inet_res_module(Opts) ->
  Fake = proplists:get_value(use_testing_inet, Opts),
  if
	Fake -> fake_inet;
	true -> inet_res
  end.
-else.
tcp_module(_, Arg) -> tcp_module_(Arg).
inet_res_module(_) -> inet_res.
-endif.
tcp_module_(inet) -> inet_tcp;
tcp_module_(inet6) -> inet6_tcp;
tcp_module_(local) -> local_tcp.

-ifdef(TEST).
pick_families_test() ->
  [inet] = pick_families(true, inet),
  [inet6, inet] = pick_families(true, undefined),
  [inet, inet6] = pick_families(prefer_ipv4, undefined),
  [inet] = pick_families(false, undefined).

families_test() ->
  [{inet, inet_tcp}] = families([{dual_stack, false}]),
  [{inet6, inet6_tcp}, {inet, inet_tcp}] = families([]),
  [{inet6, inet6_tcp}] = families([inet6]).

get_opts_family_test() ->
  inet = get_opts_family([{ip, {0, 0, 0, 0}}, binary, inet, {packet, 2}]),
  inet6 = get_opts_family([binary, inet6]),
  local = get_opts_family([{packet, 2}, binary, local]),
  undefined = get_opts_family([binary]),
  undefined = get_opts_family([]).
-endif.
