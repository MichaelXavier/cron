CABAL=cabal
EXPORTS=PATH=$$PATH:cabal-dev/bin
CONFIG_OPTS=

all: build

build: configure src/**/*.hs
	$(EXPORTS) $(CABAL) build

install: build
	cabal install

uninstall:
	ghc-pkg unregister cron


sdist: configure
	$(CABAL) sdist
	
configure: cron.cabal install_dependencies sandbox
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

test: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build 
	$(CABAL) test

autotest: test
	while true; do inotifywait -qr -e modify test/ src/; make test; done

configure_tests: sandbox
	$(CABAL) configure --enable-tests $(CONFIG_OPTS)

docs: sandbox
	$(CABAL) haddock

sandbox:
	$(CABAL) sandbox init

clean:
	$(CABAL) clean
	$(CABAL) sandbox delete
	rm -f **/*.{o,hi} **/**/*.{o,hi}
