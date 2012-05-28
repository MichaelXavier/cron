CABAL=cabal-dev
CONFIG_OPTS=

all: build

build: configure Cron.hs
	$(CABAL) build
	
configure: Cron.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}
