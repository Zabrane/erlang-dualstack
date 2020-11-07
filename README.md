# Modern tcp connect (Dual stack, Happy Eyeballs, ...) for Erlang

Modern drop-in remplacement for `gen_tcp:connect/3` featuring:

* Supports **IPv4/IPv6** DNS lookups (1995 - RFC1886),
* **SRV** Records (2000 - RFC2782+),
* **DNS Load-Balancing** (Round-Robin) (1995 - RFC1794),
* **Happy Eyeballs** (Round-Robin Fast Fallback) (2012).

## Usage and incompatibilities

Replace

    gen_tcp:connect/3

with

    dualstack_tcp:connect/3

And welcome to the modern world: dual stack with happy eyeballs is enabled by default for hostnames, and will work
sanely.

Incompatibilities with `gen_tcp:connect/3`:

* `{ip, ...}` option is now `{ip, inet_4_address | undefined, inet_6_address | undefined}`,
* `{tcp_module, ...}` option is now `{tcp_module, inet_module, inet6_module, local_module}`,
* `Timeout` is per-attempt and infinity isn't a valid option anymore:
  * To limit the entire call, set `{connect_timeout, timeout()}`.

## Configuration

* `{inet_opts, []}`, `{inet6_opts, []}`, `{local_opts, []}` -- per family specific options.

* `{connect_timeout, infinity}` -- connection overall timeout
  It is recommended to set `Timeout` and `multi_max_attempts` instead of defining this one.

* `Timeout = :timer.seconds(2)` -- connection attempt timeout, if not defined or `infinity`.
  Timeout per connection attempt.

* `{dual_stack, true}` -- resolve IPv4 and IPv6, preferring IPv6:
  Whenever to enable dual stack DNS queries.

  * `true` (default),
  * `prefer_ipv4` prioritize IPv4 instead of IPv6,
  * `false` use IPv4 unless told to use IPv6.

* `{multi, happy_eyeballs}` -- use happy eyeballs connection strategy:
  Connection strategy.

  * `happy_eyeballs` (default), uses a round-robin with happy eyeballs algorithm,
  * `round_robin` classical round-robin, tries one IP one at a time,
  * `naive` traditionnal approach: stop at the first timeout.

* `{multi_max_attempts, 8}` -- maximum number of attempts:
  How much of IPs to try.

* `{happy_eyeballs_concurrency, 4}` -- maximum number of concurrent attempts:
  How much concurrent connections attempt can run at the same time while trying to connect.

  For strictly dual-stack, set this to two.

  Four allows to profit from dual-stack _and_ hosts with redundancy, so make sense as a default.

* `{happy_eyeballs_delay, 100}` -- delay before attempting a new attempt:
  The Happy Eyeballs RFC recommends using 250ms. However, the RFC was thought for browsers and high latency
  connection/small pipes. As this library is mostly used in a datacenter setting, we picked the minimum recommended.

  Minimum 10ms. Minimum recommended 100ms. Recommended 250ms. Maximum recommended 2s.
* `{happy_eyeballs_handoff_timeout, 10}` -- internal timeout while attempting to acquire an established connection.
    After the timeout, the algorithm will proceed as if the connection didn't establish.

    There should be no pratical reason to change this setting.

---

Not only useful for IPv6/IPv4, but also for any hostname that provides multiple IPs addresses.

Intended for inclusion in Gun.

* replaces `inet:getaddr/2` by `dualstack_inet:getaddrs/3`
* replaces `gen_tcp:connect/3,4` by `dualstack_tcp:connect/3,4`
* adds `dualstack_tcp:domain_lookup/2,3` to allow splitting the logic between domain lookup and the actual connect step
* dual-stack and happy eyeballs round-robin by default (modern!!!)
* handle multiples IPs from DNS (dual stack or just round-robin) by connection strategies:
  - happy_eyeballs -- happy eyeballs (default),
  - round_robin -- classical round robin, waiting at each attempt,
  - naive -- fail on the first timeout (essentially the same as trying the first IP from all available/reachable).

If the resulting DNS results returns multiples addresses, or multiple `inet:ip_address()` are passed to connect,
the choosen `multi` algorithm will try connecting to any of thoses.

Timeouts:
 - domain_lookup_timeout
 - timeout: `dualstack_tcp:connect(_, _, _, Timeout)`
   must NOT be infinity on eyeballs or rr
 - connect_timeout
    current expectation of timeout
    total alloted time for connections
    can be infinity but won't make sense if mutiples ips

Objectives differences:
  - `ip` connection option superseded by `ip4`, `ip6`
  - The `Timeout` argument:
    - now specifies the timeout for each connection attempt
    - for hosts with more than an address:
      - defaults to two seconds instead of infinity
      - infinity isn't possible

Proposal 1:
Timeout - operation timeout
Extra setting for per connection timeout

Proposal 2:
Leave unchanged
Will need raised timeout for Classical RR to work properly
Eyeballs should be fine with traditional timeout

```
  %% Connect, automatic selection of dual-stack/families.
  dualstack_tcp:connect('google.fr', 443, []) -> {ok, Socket}.
  %% If passing an inet address directly, selection is automatic too.
  dualstack_tcp:connect({_, _, _, _}, 443, []) -> {ok, Socket}.
  dualstack_tcp:connect({_, _, _, _, _, _, _, _}, 443, []) -> {ok, Socket}.

  %% Connect with manual DNS lookup
  {ok, Addresses} = dualstack_tcp:domain_lookup('google.fr', Opts, LookupTimeout),
  {ok, Socket} = dualstack_tcp:connect(Addresses, 443, Opts, ConnectTimeout).

  %% Getaddrs
  dualstack_inet:getaddrs('google.fr', []) -> [{inet6, {_, _, _, _, _, _}, {inet, {172, _, _, _}],
  dualstack_inet:getaddrs({127, 0, 0, 1}, []) -> [{inet, {127, 0, 0, 1}}].

```

Getaddrs now share options with connect. New options for getaddrs/connect:

```
  %% source ips
  {ip4 | ip6, ip_address()} %% Similar as `ip` but for each family.

  %% Connection to multiples IPs
  {multi, happy_eyeballs} %% Enable happy eyeballs algorithm.
  {multi, round_robin} %% Enable classic round robin algorithm.
  {multi, naive} %% Fail on first timeout.
  {multi_timeout, timeout()} %% Per attempt timeout.
  {multi_attempts, non_neg_integer()} %% Maximum attempts.

  %% happy eyeballs
  {happy_eyeballs, [  %% Happy eyeballs configuration
    {delay, timeout()} %% Delay between attempts. min 10ms. default 200ms.
  ]}

```

Todo/Evolutions:
- Shuffle addresses
- SRV (different ports per address, priority (for sort))
- Historical data for sorting
-
