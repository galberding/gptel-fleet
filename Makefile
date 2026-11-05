EMACS ?= emacs
# Point these at your checkouts (straight: ~/.emacs.d/straight/build/...)
LOADPATH ?= -L ../gptel -L ../gptel-agent -L test/stubs
# test/stubs: minimal yaml/compat/orderless stand-ins for machines without them.
# With your real packages, override LOADPATH and leave test/stubs out.
.PHONY: compile test clean
compile:
	$(EMACS) -Q --batch $(LOADPATH) -L . --eval "(require 'gptel-agent)" -f batch-byte-compile gptel-fleet*.el
test: clean
	$(EMACS) -Q --batch $(LOADPATH) -L . -L test -l test/gptel-fleet-test.el -f ert-run-tests-batch-and-exit
clean:
	rm -f *.elc test/*.elc

# Interactive tests.  ECUKES_DEPS: dirs of ecukes, espuds, ansi, commander, dash, s, f.
ECUKES_DEPS ?= ../ext/ecukes ../ext/espuds ../ext/ansi.el ../ext/commander.el ../ext/dash.el ../ext/s.el ../ext/f.el
FLEET_LOADPATH ?= ../gptel:../gptel-agent:test/stubs
null :=
space := $(null) $(null)
.PHONY: ecukes tmux interactive
ecukes:
	EMACSLOADPATH="$(subst $(space),:,$(abspath $(ECUKES_DEPS))):$(subst $(space),:,$(abspath $(subst :, ,$(FLEET_LOADPATH)))):" \
	  FLEET_LOADPATH="" ECUKES_ARGS="$(ECUKES_ARGS)" $(EMACS) --script $(firstword $(ECUKES_DEPS))/ecukes-cli.el -Q $(ECUKES_ARGS)
tmux:
	FLEET_LOADPATH="$(FLEET_LOADPATH)" python3 test/interactive/tmux_scenarios.py
interactive: ecukes tmux
