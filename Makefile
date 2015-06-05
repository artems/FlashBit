CABAL=cabal --sandbox-config-file=$(CABAL_SANDBOX_CONFIG)
CABAL_SANDBOX_CONFIG=.cabal.sandbox.config

.PHONY: all
all: build

.PHONY: sandbox
sandbox:
	@$(CABAL) sandbox init

.PHONY: deps
deps: sandbox
	@$(CABAL) install --only-dependencies --enable-library-profiling

.PHONY: conf
conf: deps
	@$(CABAL) configure --enable-library-profiling --enable-executable-profiling

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
	@$(CABAL) run -- -d

.PHONY: clean
clean:
	@$(CABAL) clean
