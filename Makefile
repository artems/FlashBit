CABAL=cabal --sandbox-config-file=$(CABAL_SANDBOX_CONFIG)
CABAL_SANDBOX_CONFIG=.cabal.sandbox.config

.PHONY: all
all: build

.PHONY: sandbox
sandbox:
	@$(CABAL) sandbox init

.PHONY: deps
deps: sandbox
	@$(CABAL) install --only-dependencies

.PHONY: conf
conf: deps
	@$(CABAL) configure

.PHONY: build
build: conf
	@$(CABAL) build
.PHONY: build

build-fast:
	@$(CABAL) build

.PHONY: test
test: build
	@$(CABAL) test --test-option=--hide-successes

.PHONY: run
run: build
	@$(CABAL) run -- -d tests/_data/ubuntu-13.10-server-amd64.iso.torrent

.PHONY: clean
clean:
	@$(CABAL) clean
