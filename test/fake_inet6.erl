-module(fake_inet6).
-include("dualstack_inet.hrl").
-export([getaddrs/2]).

getaddrs('local-dualstack', _) ->
  {ok, [?LOCAL6]};
getaddrs('local-inet', _) ->
  {error, nxdomain};
getaddrs('local-inet6', _) ->
  {ok, [?LOCAL6]};
getaddrs('rr-garbage', _) ->
  {ok, [
        ?FAKE6(3),
        ?FAKE6(2),
        ?FAKE6(1),
        ?LOCAL6
      ]};

getaddrs('srv-local', _) ->
  {ok, [?LOCAL6]};

getaddrs('srv-1-local', _) ->
  {ok, [?FAKE6(1)]};

getaddrs('srv-2-local', _) ->
  {ok, [?FAKE6(2)]};

getaddrs('srv-3-local', _) ->
  {ok, [?FAKE6(3)]};
getaddrs(_, _) -> exit(fake_inet6_resolution).

