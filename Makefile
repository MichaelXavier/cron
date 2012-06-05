CABAL=cabal-dev
EXPORTS=PATH=$$PATH:cabal-dev/bin
CONFIG_OPTS=

all: build

build: configure src/**/*.hs
	$(EXPORTS) $(CABAL) build
	
configure: cron.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

test: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build 
	$(CABAL) test

autotest: test
	while true; do inotifywait -qr -e modify test/ src/; make test; done

configure_tests:
	$(CABAL) configure --enable-tests

docs:
	$(CABAL) haddock

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}
