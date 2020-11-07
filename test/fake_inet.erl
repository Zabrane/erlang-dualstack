-module(fake_inet).
-include("dualstack_inet.hrl").
-export([getaddrs/2, lookup/5]).

getaddrs('local-dualstack', _) ->
  {ok, [?LOCAL4]};
getaddrs('local-inet', _) ->
  {ok, [?LOCAL4]};
getaddrs('local-inet6', _) ->
  {error, nxdomain};
getaddrs('rr-garbage', _) ->
  {ok, [
        ?FAKE4(3),
        ?FAKE4(2),
        ?FAKE4(1),
        ?LOCAL4
      ]};

getaddrs('srv-local', _) ->
  {ok, [?LOCAL4]};

getaddrs('srv-1-local', _) ->
  {ok, [?FAKE4(1)]};

getaddrs('srv-2-local', _) ->
  {ok, [?FAKE4(2)]};

getaddrs('srv-3-local', _) ->
  {ok, [?FAKE4(3)]};

getaddrs(_, _) -> exit(fake_inet_resolution).

lookup('_srv.local-dualstack', in, srv, _, _) ->
  [{10, 0, 100, 'srv-local'}];
lookup('_srv.rr', in, srv, _, _) ->
  [{10, 0, 100, 'srv-local'},
   {20, 0, 100, 'srv-1-local'},
   {30, 0, 100, 'srv-2-local'},
   {40, 0, 100, 'srv-3-local'}];

lookup(_, _, _, _, _) ->
  exit(fake_inet_lookup).


