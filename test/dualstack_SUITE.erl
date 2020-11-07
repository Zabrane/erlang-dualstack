-module(dualstack_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include("dualstack_inet.hrl").

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).
-import(dualstack_test, [start_listener/2, start_listener/3, control_listener/2, stop_listener/1, await_listener_accept/2]).
-import(dualstack_test, [simple_connect_test/3]).
-import(dualstack_test, [random_port/0, local4/0, local6/0]).

suite() ->
	[{timetrap, 10000}].

getaddrs_tests() ->
  [
   getaddrs_dual_stack,
   getaddrs_srv
  ].

ip_sort_tests() ->
  [
   ip_sort_dual,
   ip_sort_even,
   ip_sort_uneven_v4,
   ip_sort_uneven_v6,
   ip_sort_local
  ].

connect_tests() ->
  [
   connect_v4,
   connect_v6
  ].

dualstack_tests() ->
  [
    connect_simple_v4,
    connect_simple_v6,
    connect_dual,
    connect_dual_v6_down,
    connect_dual_prefer_v4,
    connect_dual_prefer_v4_down,
    connect_dual_timeout,
    connect_rr_v6,
    connect_rr_timeout
  ].

%% Tests that makes no sense of testing on naive
dualstack_rr_tests() ->
      [
        connect_rr_latency_v4,
        connect_rr_latency_v6
      ].

all() ->
     [{group, getaddrs},
      {group, ip_sort},
      {group, connect}, {group, connect_socket},
      {group, connect_naive_s}, {group, connect_round_robin_s}, {group, connect_happy_eyeballs_s}].

groups() ->
	[
     {getaddrs, [parallel], getaddrs_tests()},
     {ip_sort, [parallel], ip_sort_tests()},
     {connect, [parallel], connect_tests()},
     {connect_socket, [parallel], connect_tests()},
     {connect_naive_s, [parallel], dualstack_tests()},
     {connect_round_robin_s, [parallel], dualstack_tests() ++ dualstack_rr_tests()},
     {connect_happy_eyeballs_s, [parallel], dualstack_tests() ++ dualstack_rr_tests()}
    ].

otp_version() ->
  {OTP,[]} = string:to_integer(erlang:system_info(otp_release)),
  OTP.

init_per_group(getaddrs, Config) ->
  doc("dualstack_inet:getaddrs"),
  Config;
init_per_group(ip_sort, Config) ->
  doc("dualstack_ip:sort"),
  Config;
init_per_group(connect, Config) ->
  doc("dualstack_tcp:connect"),
  [{inet_backend, inet} | Config];
init_per_group(connect_socket, Config) ->
  doc("dualstack_tcp:connect with new socket backend"),
  OTP = otp_version(),
  if
    OTP >= 23 -> [{inet_backend, socket} | Config];
    true -> {skip, {unsupported_otp_version, '>=', 23}}
  end;
init_per_group(connect_naive_s, Config) ->
  doc("dualstack_tcp:connect with naive strategy"),
  [{connect_strategy, naive}, Config];
init_per_group(connect_round_robin_s, Config) ->
  doc("dualstack_tcp:connect with round_robin strategy"),
  [{connect_strategy, round_robin} | Config];
init_per_group(connect_happy_eyeballs_s, Config) ->
  doc("dualstack_tcp:connect with happy_eyeballs strategy"),
  [{connect_strategy, happy_eyeballs} | Config].

end_per_group(_, _) -> ok.

%%
%% inet
%%

getaddrs_dual_stack(_) ->
  doc("dualstack_inet:getaddrs with hostname"),
  T = use_testing_inet,
  {ok, [{inet6, ?LOCAL6, _, _, _}, {inet, ?LOCAL4, _, _, _}]} = dualstack_inet:getaddrs('local-dualstack', [T], 100),
  {ok, [{inet, ?LOCAL4, _, _, _}, {inet6, ?LOCAL6, _, _, _}]} = dualstack_inet:getaddrs('local-dualstack', [T, {dual_stack, prefer_ipv4}], 100),
  {ok, [{inet, ?LOCAL4, _, _, _}]} = dualstack_inet:getaddrs('local-inet', [T], 100),
  {ok, [{inet6, ?LOCAL6, _, _, _}]} = dualstack_inet:getaddrs('local-inet6', [T], 100),
  {ok, [{inet, ?LOCAL4, _, _, _}]} = dualstack_inet:getaddrs('local-dualstack', [T, {dual_stack, false}], 100),
  ok.

getaddrs_srv(_) ->
  doc("Expands SRV records"),
  Opts = [use_testing_inet],
  {ok, [{inet6, ?LOCAL6, _, _, _}, {inet, ?LOCAL4, _, _, _}]} = dualstack_inet:getaddrs({srv, '_srv.local-dualstack'}, Opts, 500),
  ok.

-define(INET4(X), {inet, X, undefined, 0, 0}).
-define(INET6(X), {inet6, X, undefined, 0, 0}).

%%
%% inet sort
%%

ip_sort_dual(_) ->
  doc("Sorts two addresses of different families"),
  Addr = [?INET4(?FAKE4(1)), ?INET6(?FAKE6(2))],
  [{inet6, _, _, _, _}, {inet, _, _, _, _}] = dualstack_ip:sort(Addr, []),
  [{inet, _, _, _, _}, {inet6, _, _, _, _}] = dualstack_ip:sort(Addr, [{dual_stack, prefer_ipv4}]),
  ok.

ip_sort_even(_) ->
  doc("Sort multiple addresses of different families"),
  Addr = [?INET4(?FAKE4(1)), ?INET4(?FAKE4(2)), ?INET6(?FAKE6(1)), ?INET6(?FAKE6(2))],
  Sorted = dualstack_ip:sort(Addr, []),
  [{inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}] = Sorted,
  Sorted4 = dualstack_ip:sort(Addr, [{dual_stack, prefer_ipv4}]),
  [{inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}] = Sorted4,
  ok.

ip_sort_uneven_v4(_) ->
  doc("Sort multiple addresses of different families, with a predominant ipv4"),
  Addr = [?INET4(?FAKE4(1)), ?INET4(?FAKE4(2)), ?INET4(?FAKE4(3)), ?INET6(?FAKE6(1)), ?INET6(?FAKE6(2))],
  Sorted = dualstack_ip:sort(Addr, []),
  [{inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}, {inet, _, _, _, _}] = Sorted,
  Sorted1 = dualstack_ip:sort(Addr, [{dual_stack, prefer_ipv4}]),
  [{inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}] = Sorted1,
  ok.

ip_sort_uneven_v6(_) ->
  doc("Sort multiple addresses of different families, with a predominant ipv6"),
  Addr = [?INET4(?FAKE4(1)), ?INET4(?FAKE4(2)), ?INET6(?FAKE6(1)), ?INET6(?FAKE6(2)), ?INET6(?FAKE6(3))],
  Sorted = dualstack_ip:sort(Addr, []),
  [{inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}] = Sorted,
  Sorted1 = dualstack_ip:sort(Addr, [{dual_stack, prefer_ipv4}]),
  [{inet, _, _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}, {inet6, _, _, _, _}, {inet6, _, _, _, _}] = Sorted1,
  ok.

ip_sort_local(_) ->
  doc("Sort multiple addresses of different families, UNIX sockets first"),
  Addr = [?INET4(?FAKE4(1)), ?INET6(?FAKE6(1)), {local, 'x', undefined, 0, 0}, {local, 'z', undefined, 0, 0}],
  Sorted = dualstack_ip:sort(Addr, []),
  [{local, 'x', _, _, _}, {local, 'z', _, _, _}, {inet6, _, _, _, _}, {inet, _, _, _, _}] = Sorted,
  ok.

%%
%% Connect (basics)
%%

connect_(Config, Addr) ->
  Backend = proplists:lookup(inet_backend, Config),
  {ok, Port, LSPid, _LSocket} = start_listener(#{}, [{ip,Addr}]),
  control_listener(LSPid, accept),
  {ok, S} = dualstack_tcp:connect(Addr, Port, [Backend]),
  if
    Backend =:= socket -> {'$inet', _, _} = S;
    true -> true = is_port(S)
  end,
  {ok, {Addr, _, _}} = await_listener_accept(LSPid, 500),
  ok = gen_tcp:close(S),
  stop_listener(LSPid),
  ok.

connect_v4(Config) ->
  doc("dualstack:connect(LOCAL4, Port, [])"),
  connect_(Config, ?LOCAL4).

connect_v6(Config) ->
  doc("dualstack:connect(LOCAL6, Port, [])"),
  connect_(Config, ?LOCAL6).

%%
%% Connect w/ Strategy
%%

connect_simple_v4(Config) ->
  doc("dualstack:connect(LOCAL4, Port, [])"),
  Strategy = proplists:get_value(connect_strategy, Config),
  simple_connect_test(?LOCAL4, local4(), 1000),
  {error, econnrefused} = dualstack_tcp:connect(?LOCAL4, random_port(), [{multi, Strategy}]).

connect_simple_v6(Config) ->
  doc("dualstack:connect(LOCAL6, Port, [])"),
  Strategy = proplists:get_value(connect_strategy, Config),
  simple_connect_test(?LOCAL6, local6(), 1000),
  {error, econnrefused} = dualstack_tcp:connect(?LOCAL6, random_port(), [{multi, Strategy}]).

connect_dual_timeout(Config) ->
  doc("Timeout"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = [{fake_owner, self()}, {fake_spec, #{
    default => #{latency => 1000, message => true}
  }}],
  Opts = [{multi, Strategy}, use_testing_inet,
          {tcp_module, {fake_tcp, fake_tcp, fake_tcp}}, {inet_opts, Fake}, {inet6_opts, Fake}],
  {error, timeout} = dualstack_tcp:connect('local-dualstack', 42, Opts, 500),
  Events = do_collect_fake_tcp_messages(),
  [] = [IP || {IP, _, connected} <- Events],
  if
    not (Strategy == naive) ->
      [_, _] = [IP || {IP, _, connect} <- Events];
    true -> ok
  end,
  ok.

connect_dual(Config) ->
  doc("Prefer IPv6"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 0, message => true},
    ?LOCAL6 => #{latency => 0, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}],
  {ok, {fake_socket, ?LOCAL6}} = dualstack_tcp:connect('local-dualstack', 42, Opts, 400),
  Events = do_collect_fake_tcp_messages(),
  [?LOCAL6] = [IP || {IP, _, connected} <- Events],
  [?LOCAL6] = [IP || {IP, _, connect} <- Events],
  ok.

connect_dual_v6_down(Config) ->
  doc("Prefer IPv6, connect to IPv4 if IPv6 is down"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 0, message => true},
    ?LOCAL6 => #{latency => 500, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {tcp_module, fake_inet, fake_inet6, unsupported_local}],
  Res = dualstack_tcp:connect('local-dualstack', 42, Opts, 400),
  Events = do_collect_fake_tcp_messages(),
  if
    Strategy == naive ->
      {error, timeout} = Res,
      [] = [IP || {IP, _, connected} <- Events],
      [?LOCAL6] = [IP || {IP, _, connect} <- Events];
    true ->
      {ok, {fake_socket, ?LOCAL4}} = Res,
      [?LOCAL4] = [IP || {IP, _, connected} <- Events],
      [?LOCAL6, ?LOCAL4] = [IP || {IP, _, connect} <- Events]
  end,
  ok.

connect_dual_prefer_v4(Config) ->
  doc("When preferring IPv4, connect on IPv4"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 0, message => true},
    ?LOCAL6 => #{latency => 0, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}, {dual_stack, prefer_ipv4}],
  {ok, {fake_socket, ?LOCAL4}} = dualstack_tcp:connect('local-dualstack', 42, Opts, 400),
  Events = do_collect_fake_tcp_messages(),
  [?LOCAL4] = [IP || {IP, _, connected} <- Events],
  [?LOCAL4] = [IP || {IP, _, connect} <- Events],
  ok.

connect_dual_prefer_v4_down(Config) ->
  doc("when preferring ipv4, connect to ipv6 if ipv4 is down (if timeout allows)"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 500, message => true},
    ?LOCAL6 => #{latency => 0, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}, {dual_stack, prefer_ipv4}],
  Res = dualstack_tcp:connect('local-dualstack', 42, Opts, 400),
  Events = do_collect_fake_tcp_messages(),
  if
    Strategy == naive ->
      [] = [IP || {IP, _, connected} <- Events],
      [?LOCAL4] = [IP || {IP, _, connect} <- Events],
      {error, timeout} = Res;
    true ->
      [?LOCAL6] = [IP || {IP, _, connected} <- Events],
      [?LOCAL4, ?LOCAL6] = [IP || {IP, _, connect} <- Events],
      {ok, {fake_socket, ?LOCAL6}} = Res
  end,
  ok.

connect_rr_v6(Config) ->
  doc("Huge RR"),
  Strategy = proplists:get_value(connect_strategy, Config),
  doc("dualstack:connect('rr-garbage', Port, []) connects on IPv6"),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 0, message => true},
    ?LOCAL6 => #{latency => 0, message => true},
    default => #{error => {error, eunreachable}, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}],
  {ok, {fake_socket, ?LOCAL6}} = dualstack_tcp:connect('rr-garbage', 42, Opts),
  ok.

connect_rr_latency_v6(Config) ->
  doc("Huge RR with latency except the last one"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 0, message => true},
    default => #{latency => 1000, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}],
  {ok, {fake_socket, ?LOCAL4}} = dualstack_tcp:connect('rr-garbage', 42, Opts, 500),
  Events = do_collect_fake_tcp_messages(),
  [_] = [IP || {IP, _, connected} <- Events],
  ConnTrail = [IP || {IP, _, connect} <- Events],
  io:format("ConnTrail: ~p ~n", [ConnTrail]),
  ok.

connect_rr_latency_v4(Config) ->
  doc("Huge RR with latency except the last one"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    ?LOCAL4 => #{latency => 0, message => true},
    default => #{latency => 1000, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}],
  {ok, {fake_socket, ?LOCAL4}} = dualstack_tcp:connect('rr-garbage', 42, Opts, 500),
  Events = do_collect_fake_tcp_messages(),
  [_] = [IP || {IP, _, connected} <- Events],
  ConnTrail = [IP || {IP, _, connect} <- Events],
  io:format("ConnTrail: ~p ~n", [ConnTrail]),
  [?LOCAL4, ?LOCAL6 | _] = lists:reverse(ConnTrail),
  ok.

connect_rr_timeout(Config) ->
  doc("All RR addresses timeout"),
  Strategy = proplists:get_value(connect_strategy, Config),
  Fake = {self(), #{
    default => #{latency => 1000, message => true}
  }},
  Opts = [{multi, Strategy}, use_testing_inet, {{tcp_module, fake_inet, fake_inet6, unsupported_local}, Fake}],
  {error, timeout} = dualstack_tcp:connect('rr-garbage', 42, Opts, 500),
  Events = do_collect_fake_tcp_messages(),
  [] = [IP || {IP, _, connected} <- Events],
  Connects = [IP || {IP, _, connect} <- Events],
  if
    Strategy == naive -> 1 = length(Connects);
    true -> 8 = length(Connects)
  end,
  ok.

do_collect_fake_tcp_messages() ->
  do_collect_fake_tcp_messages([]).
do_collect_fake_tcp_messages(Acc) ->
  receive
    {fake_tcp, IP, Message, Event} ->
      do_collect_fake_tcp_messages([{IP, Message, Event} | Acc])
  after
    0 -> lists:reverse(Acc)
  end.

