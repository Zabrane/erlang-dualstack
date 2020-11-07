-module(dualstack_ip).

-export([sort/2]).

-spec sort([dualstack_inet:ip_address()], [dualstack_tcp:connect_option()]) -> [dualstack_inet:ip_address()].
sort([_] = IP, _) -> IP;

sort(IPs0, Opts) ->
  Fold = fun
    ({inet, _, _, _, _} = A, {V4, V6, L}) -> {[A|V4], V6, L};
    ({inet6, _, _, _, _} = A, {V4, V6, L}) -> {V4, [A|V6], L};
    ({local, _, _, _, _} = A, {V4, V6, L}) -> {V4, V6, [A|L]}
  end,
  {V4, V6, Local} = lists:foldr(Fold, {[], [], []}, IPs0),
  DualStackOpt = proplists:get_value(dual_stack, Opts, true),
  {First, Last} = case DualStackOpt of
                    prefer_ipv4 -> {V4, V6};
                    _ -> {V6, V4}
                  end,
  FirstLen = length(First),
  LastLen = length(Last),
  MixedAddresses = if
    (First == []) or (Last == []) -> First ++ Last;
    FirstLen == LastLen -> [[A, B] || {A, B} <- lists:zip(First, Last)];
    FirstLen > LastLen ->
                       {FirstHead, FirstTail} = lists:split(LastLen, First),
                       [[A, B] || {A, B} <- lists:zip(FirstHead, Last)] ++ FirstTail;
    FirstLen < LastLen ->
                       {LastHead, LastTail} = lists:split(FirstLen, Last),
                       [[A, B] || {A, B} <- lists:zip(First, LastHead)] ++ LastTail
                   end,
  Addresses = lists:flatten(MixedAddresses),
  io:format("Sorted Addresses: ~p ~p~n", [Local, Addresses]),
  Local ++ Addresses.

