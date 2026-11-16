# The Emacs release to run, e.g. 29.1, 30.2 or 31.1, see the Dockerfile.
EMACS ?= 31.1
# The releases CI runs, as in .github/workflows/ci.yml, for `make ci-all'.
EMACS_ALL := 29.1 30.2 31.1
IMAGE := mermaid-text-test:emacs-$(EMACS)

DOCKER := $(shell command -v docker 2>/dev/null || command -v podman 2>/dev/null)
ifeq ($(DOCKER),)
  $(error Neither docker nor podman found on PATH)
endif

# Podman's multi-stage builds ask the registry about a base image it has,
# for some 30s a stage (containers/podman#27779), so one stored is used as
# is.  BASE_IMAGE is the Dockerfile's FROM.
BASE_IMAGE := ubuntu:26.04
NO_PULL = $(if $(findstring podman,$(DOCKER)),$(shell $(DOCKER) image inspect $(BASE_IMAGE) >/dev/null 2>&1 && echo --pull=never))

# Every container runs as the host's user, with no capabilities nor
# privileges to gain, and no network but where it fetches something.  The
# tree is read-only but for .eldev, where Eldev keeps the packages and
# test results.  Rootless podman's root is the host's user already.
ROOTLESS := $(if $(findstring podman,$(DOCKER)),$(shell $(DOCKER) info --format '{{.Host.Security.Rootless}}' 2>/dev/null))
CONTAINER_USER = $(if $(filter true,$(ROOTLESS)),,--user $$(id -u):$$(id -g))
ISOLATED = --rm --cap-drop all --security-opt=no-new-privileges --pids-limit=200 \
  $(CONTAINER_USER) -e HOME=/tmp
# CI, as GitHub Actions sets it, turns on Eldev's retrying of failed
# package archive fetches.
RUN_ONLINE = $(DOCKER) run $(ISOLATED) -e CI \
  -v "$(CURDIR)":/src:ro -v "$(CURDIR)/.eldev":/src/.eldev -w /src $(IMAGE)
RUN = $(DOCKER) run $(ISOLATED) --network=none \
  -v "$(CURDIR)":/src:ro -v "$(CURDIR)/.eldev":/src/.eldev -w /src $(IMAGE)

.PHONY: ci ci-all image deps clean compile lint test

CI_CHECKS ?= compile lint test

# Build the image and install the dependencies, then run compile, lint
# and test in parallel, as they are independent.  Each can also run on
# its own once ci has run.  Each check's output goes to
# .ci/EMACS-CHECK.log; the end is a line per check, a failed one's
# findings under it.
CI_NOISE := ^ *passed |: no warnings$$|^Indenting region|^Running linter|^$$
ci: clean
	$(if $(NO_SETUP),,$(MAKE) image && $(MAKE) deps)
	@mkdir -p .ci
	@for c in $(CI_CHECKS); do \
	  ( $(MAKE) -s $$c > .ci/$(EMACS)-$$c.log 2>&1; echo $$? > .ci/$(EMACS)-$$c.status ) & \
	done; \
	wait; \
	echo "== Emacs $(EMACS)"; \
	failed=0; \
	for c in $(CI_CHECKS); do \
	  if [ "$$(cat .ci/$(EMACS)-$$c.status)" = 0 ]; then echo "$$c: ok"; \
	  else failed=1; echo "$$c: failed, see .ci/$(EMACS)-$$c.log"; \
	    grep -vE "$(CI_NOISE)" .ci/$(EMACS)-$$c.log | grep -v "^make" | sed "s/^/    /"; fi; \
	done; \
	exit $$failed

# ci on every release CI runs, at once: each release has its own image,
# and its own .eldev/VERSION.  Each run's output goes to .ci/VERSION.log.
# The images are built and the dependencies installed first, a release
# at a time: MELPA answers too many fetches at once with 429.
ci-all:
	@mkdir -p .ci
	@for v in $(EMACS_ALL); do \
	  $(MAKE) image deps EMACS=$$v > .ci/$$v-setup.log 2>&1 || \
	    { tail -20 .ci/$$v-setup.log; echo "Emacs $$v: setup failed, see .ci/$$v-setup.log"; exit 1; }; \
	done
	@for v in $(EMACS_ALL); do \
	  ( $(MAKE) ci NO_SETUP=1 EMACS=$$v > .ci/$$v.log 2>&1; \
	    echo $$? > .ci/$$v.status ) & \
	done; \
	wait; \
	failed=0; \
	for v in $(EMACS_ALL); do \
	  if [ "$$(cat .ci/$$v.status)" = 0 ]; then echo "Emacs $$v: ok"; \
	  else failed=1; echo "Emacs $$v: failed, see .ci/$$v.log"; \
	    if grep -q "^== Emacs" .ci/$$v.log; then \
	      sed -n "/^== Emacs/,\$$p" .ci/$$v.log | sed 1d | grep -v "^make" | sed "s/^/  /"; \
	    else tail -20 .ci/$$v.log; fi; fi; \
	done; \
	exit $$failed

image:
	$(DOCKER) build $(NO_PULL) --build-arg EMACS=$(EMACS) -t $(IMAGE) .

# The linters too, as lint and test run at once, against the same .eldev.
# Tried three times, as MELPA rate-limits its clients (429).
deps:
	@mkdir -p .eldev
	$(RUN_ONLINE) sh -c 'for i in 1 2 3; do \
	  eldev prepare build test lint && eldev upgrade && exit 0; sleep 30; done; exit 1'

clean:
	find . -maxdepth 2 -name '*.elc' -delete

# Compile a copy, so the .elc files do not land next to the sources while
# lint and test run alongside.  The tests are compiled too.
compile:
	$(RUN) sh -c "mkdir -p /tmp/build/.eldev \
	  && cp -a /src/Eldev /src/*.el /src/test /tmp/build \
	  && cp -a /src/.eldev/$(EMACS) /tmp/build/.eldev \
	  && cd /tmp/build && eldev compile --warnings-as-errors --set all"

# checkdoc, package-lint and elisp-lint, as the Eldev file says.
lint:
	$(RUN) eldev lint

test:
	$(RUN) eldev test
