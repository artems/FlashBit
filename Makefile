CABAL=cabal
PROJECT_CABAL=htorr.cabal
CONFIGURE_OPTIONS=

.PHONY: all
all:


.PHONY: sandbox
sandbox:
	$(CABAL) sandbox init

.PHONY: conf
conf: sandbox
	$(CABAL) install --enable-tests --only-dependencies
	$(CABAL) configure --enable-tests $(CONFIGURE_OPTIONS)

.PHONY: build
build: conf
	$(CABAL) build

.PHONY: test
test: conf
	$(CABAL) test
#	$(CABAL) test --show-details=always

.PHONY: clean
clean:
	$(RM) -rf dist
	$(RM) -rf .cabal-sandbox cabal.sandbox.config


