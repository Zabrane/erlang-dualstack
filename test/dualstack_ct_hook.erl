-module(dualstack_ct_hook).

-export([init/2]).

init(_, _) ->
	ct_helper:start([dualstack]),
	{ok, undefined}.

