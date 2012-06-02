CABAL=cabal-dev
CONFIG_OPTS=

all: build

build: configure src/*.hs src/**/*.hs
	$(CABAL) build
	
configure: Cron.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

test: configure_tests
	$(CABAL) build
	$(CABAL) test

configure_tests:
	$(CABAL) configure --enable-tests

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}
