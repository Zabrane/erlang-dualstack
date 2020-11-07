# See LICENSE for licensing information.

PROJECT = dualstack
PROJECT_DESCRIPTION = TCP IPv4/IPv6 dual stack and happy eyeballs for Erlang/OTP.
PROJECT_VERSION = 0.0.1

CT_OPTS += -ct_hooks dualstack_ct_hook []
TEST_ERLC_OPTS += -DTESTING +debug_info +'{parse_transform, eunit_autoexport}'
TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper.git master

# Standard targets.

include erlang.mk

# Generate rebar.config on build.

app:: rebar.config

