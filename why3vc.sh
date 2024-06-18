#!/bin/sh -eux

DEBUG_FLAGS="--debug=vc_debug --debug=vc_reflow --debug=print_types --debug=print_coercions --debug=print_modules"

./bin/why3 prove --vc-only $DEBUG_FLAGS $@
