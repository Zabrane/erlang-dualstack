-module(fake_tcp).

-export([connect/4]).

connect(IP, _, Opts, Timeout) ->
  Parent = proplists:get_value(fake_owner, Opts),
  Specs = proplists:get_value(fake_spec, Opts),
  Spec = maps:get(IP, Specs, maps:get(default, Specs, undefined)),
  report(IP, Parent, Spec, connect),
  connect_(IP, Parent, Spec, Timeout).

connect_(IP, P, #{latency := Latency} = Spec, Timeout) ->
  io:format("Fake connecting on ~p with latency ~p (timeout ~p)~n", [IP, Latency, Timeout]),
  T = timer:send_after(Timeout, timer),
  receive
    timer ->
      io:format("Connecting on ~p timeouted~n", [IP]),
      report(IP, P, Spec, timeout),
      {error, timeout}
  after
    Latency ->
      io:format("Fake connected on ~p~n", [IP]),
      _ = timer:cancel(T),
      report(IP, P, Spec, connected),
      {ok, {fake_socket, IP}}
  end;

connect_(IP, _, #{error := Error}, _) ->
  io:format("Fake error connect on ~p: ~p~n", [IP, Error]),
  {error, Error};

connect_(IP, _, Spec, Timeout) ->
  exit({fake_tcp, unimplemented_connect, IP, Spec, Timeout}).

report(IP, P, #{message := Message}, Event) ->
  P ! {fake_tcp, IP, Message, Event};
report(_, _, _, _) ->
  ok.

