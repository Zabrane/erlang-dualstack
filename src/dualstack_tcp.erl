-module(dualstack_tcp).

-export([domain_lookup/2, domain_lookup/3]).
-export([connect/3, connect/4]).

-type connect_option() ::
        {ip4, inet:socket_address()} |
        {ip6, inet:socket_address()} |
        {multi, happy_eyeballs | round_robin | naive} |
        {multi_timeout, timeout()} |
        {multi_attempts, non_neg_integer()} |
        gen_tcp:connect_option().
-export_type([connect_option/0]).

% Default domain lookup timeout.
-define(DEFAULT_DOMAIN_LOOKUP_TIMEOUT, 500).
% Default round-robin strategy.
-define(DEFAULT_STRATEGY, happy_eyeballs).
% Default strategy timeout.
-define(DEFAULT_RR_TIMEOUT, infinity).
% Default timeout when a round-robin strategy is used and timeout = infinity.
-define(DEFAULT_RR_CONNECT_TIMEOUT, 2000).
%% Happy Eyeballs default delay.
-define(DEFAULT_HAPPY_EYEBALLS_DELAY, 150).
%% Happy Eyeballs default handoff timeout.
-define(DEFAULT_HAPPY_EYEBALLS_HANDOFF_TIMEOUT, 100).

%% The functions domain_lookup/4 and connect/2 are very similar
%% to connect/4 except the logic is split in order to
%% be able to trigger events between the domain lookup step
%% and the actual connect step.

%% connect/4 is similar to gen_tcp:connect/4.

-spec domain_lookup(inet:ip_address() | inet:hostname(), [connect_option()])
	-> {ok, [dualstack_inet:ip_address()]} | {error, atom()}.
domain_lookup(Address, Opts) ->
  DomainLookupTimeout = proplists:get_value(domain_lookup_timeout, Opts, ?DEFAULT_DOMAIN_LOOKUP_TIMEOUT),
  domain_lookup(Address, Opts, DomainLookupTimeout).

-spec domain_lookup(inet:ip_address() | inet:hostname(), [connect_option()], timeout())
	-> {ok, [dualstack_inet:ip_address()]} | {error, atom()}.
domain_lookup(Address, Opts, Timeout) ->
  case check_opts(Opts) of
    {ok, _} ->
        case dualstack_inet:getaddrs(Address, Opts, Timeout) of
            {ok, IPs} ->
                {ok, IPs};
            Error ->
                maybe_exit(Error)
        end;
    Error ->
      Error
  end.

-spec connect(inet:ip_address() | inet:hostname() | [dualstack_inet:ip_address()], inet:port_number(), [connect_option()])
-> {ok, inet:socket()} | {error, inet:posix()}.
connect(Address, Port, Opts) ->
  connect(Address, Port, Opts, infinity).

-spec connect(inet:ip_address() | inet:hostname() | [dualstack_inet:ip_address()], inet:port_number(), [connect_option()], timeout())
-> {ok, inet:socket()} | {error, inet:posix()}.
connect(Address, Port, Opts, Timeout) ->
  case check_opts(Opts) of
    {ok, _} ->
      case is_address_list(Address) of
        true ->
          begin_connect(Address, Port, Opts, Timeout);
        false ->
          case domain_lookup(Address, Opts) of
            {ok, IPs} -> begin_connect(IPs, Port, Opts, Timeout);
            Error -> maybe_exit(Error)
          end
      end;
    Error ->
      Error
  end.

begin_connect(IPs0, Port0, Opts, Timeout0) ->
  case inet_tcp:getserv(Port0) of
    {ok, Port} ->
      %% @todo sort IPs
      IPs = dualstack_ip:sort(IPs0, Opts),
      Strategy = if
                   length(IPs) == 1 -> naive;
                   true -> proplists:get_value(multi, Opts, ?DEFAULT_STRATEGY)
                 end,
      {StrategyTimeout, Timeout} = strategy_timeout(Strategy, Opts, Timeout0),
      Timer = inet:start_timer(StrategyTimeout),
      ConnectArgs = #{strategy => Strategy, port => Port, opts => Opts, timer => Timer, timeout => Timeout},
      try try_connect(IPs, ConnectArgs, {error, einval}) of
              {ok, S} -> {ok, S};
                Error -> maybe_exit(Error)
        after
            _ = inet:stop_timer(Timer)
      end;
    Error ->
      maybe_exit(Error)
  end.

%% Naive / Round-Robin strategy
try_connect([IP|IPs], #{strategy := Strategy} = ConnectArgs, _) when Strategy =:= naive; Strategy =:= round_robin ->
  Naive = Strategy == naive,
	case {Naive, mod_connect(IP, ConnectArgs)} of
        {_, {ok, S}} -> {ok, S};
        {true, {error, einval}} -> {error, einval};
        {true, {error, timeout}} -> {error, timeout};
        {_, Error} -> try_connect(IPs, ConnectArgs, Error)
    end;
%% Happy Eyeballs strategy
try_connect([_|_] = IPs, #{strategy := happy_eyeballs, opts := Opts} = ConnectArgs, Error) ->
  EyeballsDelay = proplists:get_value(happy_eyeballs_delay, Opts, ?DEFAULT_HAPPY_EYEBALLS_DELAY),
  HandoffTimeout = proplists:get_value(happy_eyeballs_handoff_timeout, Opts, ?DEFAULT_HAPPY_EYEBALLS_HANDOFF_TIMEOUT),
  happy_connect([], IPs, ConnectArgs#{eyeballs_delay => EyeballsDelay, eyeballs_handoff_timeout => HandoffTimeout},
                undefined, Error);
%% Error.
try_connect([], _, Error) ->
  Error.

%-spec happy_connect([] | [pid()], [] | [dualstack_inet:ip_address()], #{}, undefined | timer:tref(), any()) -> {ok, inet:socket()} | {error, inet:posix()}.
happy_connect([_ | _] = Pids0, IPs0, #{eyeballs_delay := Delay} = ConnectArgs, Timer0, LastError) ->
  receive
    {eyeball_connected, Pid} ->
      io:format("PIDs: ~p Pid: ~p~n", [Pids0, Pid]),
      case lists:member(Pid, Pids0) of
        true ->
          ack_happy_connect(Pid, Pids0, IPs0, ConnectArgs, Timer0, LastError)
      end;
    {eyeball_error, Pid, Error} ->
      io:format("Error ~p ~p~n", [Pid, Error]),
      Pids = Pids0 -- [Pid],
      happy_connect(Pids, IPs0, ConnectArgs, Timer0, Error);
    eyeball_start ->
      io:format("Timer start~n", []),
      maybe_stop_timer(Timer0),
      {ok, Timer} = timer:send_after(Delay, eyeball_start),
      {Pids, IPs} = attempt_happy_connect(Pids0, IPs0, ConnectArgs),
      happy_connect(Pids, IPs, ConnectArgs, Timer, LastError)
  after
    Delay ->
      io:format("Tick~n", []),
      %{Pids, IPs} = attempt_happy_connect(Pids0, IPs0, ConnectArgs),
      happy_connect(Pids0, IPs0, ConnectArgs, Timer0, LastError)
  end;
happy_connect([], [_ | _] = IPs0, #{eyeballs_delay := Delay} = ConnectArgs, Timer0, LastError) ->
  io:format("No PIDs~n", []),
  maybe_stop_timer(Timer0),
  {ok, Timer} = timer:send_after(Delay, eyeball_start),
  {Pids, IPs} = attempt_happy_connect([], IPs0, ConnectArgs),
  io:format("Attempt start ~p~n", [Pids]),
  happy_connect(Pids, IPs, ConnectArgs, Timer, LastError);
happy_connect(_, _, _, Timer, Error) ->
  maybe_stop_timer(Timer),
  flush_happy_connect(),
  Error.

ack_happy_connect(Pid, Pids0, IPs, #{eyeballs_handoff_timeout := Timeout} = ConnectArgs, Timer, _) ->
    Pids = Pids0 -- [Pid],
    Pid ! {eyeball_connected, ack},
    receive
      {eyeball_handoff, Pid, Socket} ->
        maybe_stop_timer(Timer),
        Clean = fun (P) -> unlink(P), exit(P, kill) end,
        [Clean(P) || P <- Pids],
        flush_happy_connect(),
        {ok, Socket}
    after
      Timeout ->
        unlink(Pid),
        exit(Pid),
        io:format("Handoff timeout"),
        happy_connect(Pids, IPs, ConnectArgs, Timer, {error, timeout})
    end.

attempt_happy_connect(Pids, [IP | IPs], ConnectArgs) ->
    Parent = self(),
	Fun = fun () ->
      case mod_connect(IP, ConnectArgs) of
        {ok, Socket} ->
          attempt_happy_connect_handoff(Parent, Socket, ConnectArgs);
        Error ->
          Parent ! {eyeball_error, self(), Error},
          shutdown
      end
    end,
    Pid = spawn_link(Fun),
  io:format("Started eyeball~p~n", [Pid]),
  {[Pid | Pids], IPs};
attempt_happy_connect(Pids, [], _) ->
  io:format("No eyeballs to start~n", []),
  {Pids, []}.

attempt_happy_connect_handoff(Parent, Socket, #{eyeballs_handoff_timeout := Timeout}) ->
  Parent ! {eyeball_connected, self()},
  receive
    {eyeball_connected, ack} ->
        ok = controlling_process(Socket, Parent),
        Parent ! {eyeball_handoff, self(), Socket},
        shutdown
  after
    Timeout ->
      Parent ! {eyeball_error, self(), {error, timeout}},
      shutdown
  end.

controlling_process({fake_socket, _}, _) -> ok;
controlling_process(S, P) -> gen_tcp:controlling_process(S, P).

flush_happy_connect() ->
  receive
    {eyeball_connected, _} -> flush_happy_connect();
    {eyeball_handoff, _, _} -> flush_happy_connect();
    {eyeball_error, _, _} -> flush_happy_connect();
    eyeball_start -> flush_happy_connect()
  after
    0 -> ok
  end.

flush_happy_start() ->
  receive
    eyeball_start -> ok
  after
    0 -> ok
  end.

maybe_stop_timer(undefined) -> ok;
maybe_stop_timer(T) ->
  _ = timer:cancel(T),
  flush_happy_start().

mod_connect({Family, IP, Port0, _, _}, #{port := Port1, opts := Opts0} = ConnectArgs) ->
  Port = pick_port(Port0, Port1),
  io:format("Mod opts: ~p~n", [Opts0]),
  {Mod, Opts} = tcp_module(Family, IP, Opts0),
  ConnectTimeout = connect_timeout(ConnectArgs),
  Mod:connect(IP, Port, Opts, ConnectTimeout).

pick_port(undefined, Port) -> Port;
pick_port(Port, _) when is_integer(Port) -> Port;
pick_port(_, Port) -> Port.

-ifdef(TESTING).
tcp_module(Family, IP, Opts) ->
  Testing = proplists:get_value(use_testing_tcp, Opts, false),
  if
    not Testing ->
      tcp_module_(Family, IP, Opts);
    true ->
      {fake_tcp, Opts}
  end.
-else.
tcp_module(Family, IP, Opts) ->
  tcp_module_(Family, IP, Opts).
-endif.
tcp_module_(Family, IP, Opts0) ->
  Opts1 = [Family | Opts0],
  CleanOpts0 = clean_opts(Opts1),
  SrcIPOpt = case {Family, proplists:get_value(ip, Opts1)} of
    {inet, {ip, Inet, _}} -> Inet;
    {inet6, {ip, _, Inet6}} -> Inet6;
    _ -> undefined
  end,
  io:format("opts ~p~n", [Opts1]),
  io:format("CleanOpts = ~p~n", [CleanOpts0]),
  io:format("Family ~p~n", [Family]),
  CleanOpts1 = case SrcIPOpt of
                 undefined -> CleanOpts0;
                 SrcIPOpt -> [{ip, SrcIPOpt} | CleanOpts0]
               end,
  io:format("ModOpt prop ~p~n", [proplists:get_value(tcp_module, Opts1)]),
  ModOpt = case {Family, proplists:get_value(tcp_module, Opts1)} of
             {inet, {InetMod, _, _}} -> InetMod;
             {inet6, {_, Inet6Mod, _}} -> Inet6Mod;
             {local, {_, _, LocalMod}} -> LocalMod;
             {_, undefined} -> undefined
           end,
  io:format("ModOpt ~p~n", [ModOpt]),
  CleanOpts2 = case ModOpt of
                  undefined -> CleanOpts1;
                  ModOpt -> [{tcp_module, ModOpt} | CleanOpts1]
               end,
  io:format("Cleanopts2 with ModOpt ~p~n", [CleanOpts2]),
  % Specific options
  SpecificOpts = case Family of
                   inet -> proplists:get_value(inet_opts, Opts1, []);
                   inet6 -> proplists:get_value(inet6_opts, Opts1, []);
                   local -> proplists:get_value(local_opts, Opts1, [])
                 end,
  CleanOpts3 = SpecificOpts ++ CleanOpts2,
  % not a dualstack option but gen_tcp requires it to be put first.
  LastOpts = case proplists:get_value(inet_backend, Opts1) of
               undefined -> CleanOpts3;
                 Backend -> [{inet_backend, Backend} | CleanOpts3]
             end,
  io:format("LastOpts ~p~n", [LastOpts]),
  inet:tcp_module(LastOpts, IP).

maybe_exit({error, einval}) -> exit(badarg);
maybe_exit(Error) -> Error.

strategy_timeout(naive, _, Timeout) -> {Timeout, Timeout};
strategy_timeout(Strategy, Opts, infinity) ->
  strategy_timeout(Strategy, Opts, ?DEFAULT_RR_CONNECT_TIMEOUT);
strategy_timeout(_, Opts, Timeout) ->
  {proplists:get_value(multi_timeout, Opts, ?DEFAULT_RR_TIMEOUT), Timeout}.

connect_timeout(#{strategy := naive, timer := Timer}) -> inet:timeout(Timer);
connect_timeout(#{timer := Timer, timeout := Timeout}) ->
  InetTimeout = inet:timeout(Timer),
  if
    InetTimeout == infinity -> Timeout;
    InetTimeout =< Timeout -> InetTimeout;
    true -> Timeout
  end.

check_opts(Opts) ->
  {ok, _Ours, _InetOpts} = check_opts(Opts, [], []),
  % @todo find a way to validate inet options without inet_tcp/inet6/.. specifics
  %{ok, _Ours, InetOpts} -> inet:connect_options(InetOpts, inet_tcp);
  {ok, Opts}.

check_opts([{dual_stack, B} = C | Opts], Acc, U) when is_boolean(B) ->
  check_opts(Opts, [C|Acc], U);
check_opts([{dual_stack, prefer_ipv4} = C | Opts], Acc, U) ->
  check_opts(Opts, [C|Acc], U);
check_opts([{dual_stack, _} | _], _, _) ->
  exit(badarg);
check_opts([{multi, M} = C | Opts], Acc, U) when M =:= naive; M =:= round_robin; M =:= happy_eyeballs ->
  check_opts(Opts, [C|Acc], U);
check_opts([{multi, _} | _], _, _) ->
  exit(badarg);
check_opts([{domain_lookup_timeout, I} = C | Opts], Acc, U) when I =:= infinity orelse I > 0 ->
  check_opts(Opts, [C|Acc], U);
check_opts([{domain_lookup_timeout, _} | _], _, _) ->
  exit(badarg);
check_opts([{tcp_module, {I, I6, L}} = C | Opts], Acc, U) when is_atom(I); is_atom(I6); is_atom(L) ->
  check_opts(Opts, [C|Acc], U);
check_opts([{tcp_module, _} | _], _, _) ->
  exit(badarg);
check_opts([{K, V} = C | Opts], Acc, U) when K =:= inet_opts orelse K =:= inet6_opts orelse K =:= local_opts; is_list(V) ->
  check_opts(Opts, [C|Acc], U);
check_opts([{K, _} | _], _, _) when K =:= inet_opts orelse K =:= inet6_opts orelse K =:= local_opts ->
  exit(badarg);
check_opts([{ip, _, _} = C | Opts], Acc, U) ->
  %% @todo Ensure {ip, undefined | {inet, ...}, undefined | {inet6, ...}}
  check_opts(Opts, [C|Acc], U);
check_opts([{ip, _} | _], _, _) ->
  exit(badarg);
check_opts([C | Opts], Acc, U) ->
  check_opts(Opts, Acc, [C | U]);
check_opts([], Acc, U) ->
  {ok, lists:reverse(Acc), lists:reverse(U)}.

%% Remove our own options before passing to inet modules.
clean_opts(Opts) ->
  lists:reverse(clean_opts(Opts, [])).
clean_opts([{multi, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([use_testing_inet | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{dual_stack, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{domain_lookup_timeout, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{tcp_module, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{inet_opts, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{inet6_opts, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{local_opts, _} | Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([{ip, _, _} | Opts], Acc) -> clean_opts(Opts, Acc);
%% not one of dualstack' options, but gen_tcp requires it to be put first.
clean_opts([{inet_backend, _}|Opts], Acc) -> clean_opts(Opts, Acc);
clean_opts([Opt | Opts], Acc) -> clean_opts(Opts, [Opt | Acc]);
clean_opts([], Acc) -> Acc.

-ifdef(TEST).
clean_opts_preserve_order_test() ->
  List = lists:seq(1, 10),
  List = clean_opts(List).

check_opts_preserve_order_test() ->
  List = [{multi, round_robin}, nodelay, active, {dual_stack, prefer_ipv4}],
  {ok, [{multi, round_robin}, {dual_stack, prefer_ipv4}], [nodelay, active]} = check_opts(List, [], []).
-endif.

is_address_list([{inet, _, _, _, _} | Rest]) ->  is_address_list(Rest);
is_address_list([{inet6, _, _, _, _} | Rest]) -> is_address_list(Rest);
is_address_list([{local, _, _, _, _} | Rest]) -> is_address_list(Rest);
is_address_list([_ | _]) -> false;
is_address_list([]) -> true;
is_address_list(_) -> false.

-ifdef(TEST).
is_address_list_test() ->
  false = is_address_list({127, 0, 0, 1}),
  false = is_address_list([{127, 0, 0, 1}]),
  false = is_address_list('google.fr'),
  false = is_address_list(['google.fr', {inet, {127, 0, 0, 1}}]),
  true = is_address_list([{inet, {0, 0, 0, 0}, undefined, 0, 0},
                          {local, {local, '/nowhere'}, undefined, 0, 0},
                          {inet6, {0, 0, 0, 0, 0, 0, 0, 0}, undefined, 0, 0}
                         ]),
  ok.
-endif.
