-module(dualstack_test).
-export([simple_connect_test/3,
         random_port/0, local4/0, local6/0,
         await_listener_accept/2,
         start_listener/2, start_listener/3, control_listener/2, stop_listener/1]).

-define(LOCAL4, {127, 0, 0, 1}).
%%-define(LOCAL6, {65152, 0, 0, 0, 0, 0, 0, 2}).
-define(LOCAL6, {0, 0, 0, 0, 0, 0, 0, 1}).

local4() -> ?LOCAL4.
local6() -> ?LOCAL6.

simple_connect_test(Address, BindAddress, Timeout) ->
  {ok, Port, LSPid, _LSocket} = start_listener(#{}, [{ip,BindAddress}]),
  control_listener(LSPid, accept),
  {ok, S} = dualstack_tcp:connect(Address, Port, []),
  {ok, {BindAddress, _, _}} = await_listener_accept(LSPid, Timeout),
  ok = gen_tcp:close(S),
  stop_listener(LSPid).

random_port() ->
  {ok, Port, LSPid, _} = start_listener(#{}, []),
  stop_listener(LSPid),
  Port.

await_listener_accept(ListenerPid, Timeout) ->
  receive
    {dualstack_test, ListenerPid, accept, Res} -> Res
  after
    Timeout ->
      {error, await_listener_accept_timeout}
  end.

start_listener(Args, TCPOptions) ->
  start_listener(Args, 0, TCPOptions).

start_listener(Args, Port0, TCPOptions) ->
  Self = self(),
  Pid = spawn_link(fun () -> listener_enter_loop(Self, Port0, Args, TCPOptions) end),
  receive
    {dualstack_test, up, Pid, Socket} ->
      {ok, Port} = inet:port(Socket),
      {ok, Port, Pid, Socket};
    {dualstack_test, error, Pid, Error} ->
      Error
  end.

control_listener(Pid, Command) ->
  Pid ! {dualstack_test, self(), Command}.

stop_listener(Pid) ->
  Pid ! {dualstack_test, self(), stop},
  receive
    shutdown -> ok
  after
    1500 ->
      exit(Pid, kill),
      ok
  end.

listener_enter_loop(Parent, Port, Args, TCPOptions) ->
  case gen_tcp:listen(Port, TCPOptions) of
    {ok, Socket} ->
      Parent ! {dualstack_test, up, self(), Socket},
      listener_loop(Parent, Socket, Args);
    Error ->
      Parent ! {dualstack_test, error, self(), Error}
  end.

listener_loop(Parent, Socket, Args) ->
  io:format("listen loop", []),
  receive
    {dualstack_test, Parent, accept} ->
      listener_accept(Parent, Socket, Args);
    {dualstack_test, Parent, {close, S}} ->
      listener_close_socket(Parent, Socket, Args, S);
    {dualstack_test, Parent, stop} ->
      gen_tcp:close(Socket),
      Parent ! shutdown,
      shutdown
  end.

listener_accept(Parent, Socket, Args) ->
  Res = case gen_tcp:accept(Socket, 1000) of
          {ok, S} ->
            {ok, {IP, P}} = inet:sockname(S),
            io:format("Accepted ~p:~p~n", [IP, P]),
            {ok, {IP, P, S}};
          Error ->
            Error
        end,
  Parent ! {dualstack_test, self(), accept, Res},
  listener_loop(Parent, Socket, Args).

listener_close_socket(Parent, Socket, Args, S) ->
  Res = gen_tcp:close(S),
  Parent ! {dualstack_test, close, Res},
  listener_loop(Parent, Socket, Args).

