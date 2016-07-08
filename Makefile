build:
	stack build --fast

build-watch:
	stack build --fast --file-watch

clean:
	stack clean

docs:
	cabal haddock --hyperlink-source

ghci:
	echo ghci

install:
	stack install --local-bin-path=./bin

profile:
	stack install --executable-profiling --library-profiling --ghc-options="-rtsopts"

weird:
	brew install icu4c
	stack install text-icu --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include
	# http://stackoverflow.com/questions/7420514/using-text-icu-library-in-haskell-on-mac-os


LIMIT=100

migrate-dev:
	./bin/ln-smf-migrate 127.0.0.1 10.0.3.14 http://dev.adarq.org migrate $(LIMIT)

unmigrate-dev:
	./bin/ln-smf-migrate 127.0.0.1 10.0.3.14 http://dev.adarq.org unmigrate

migrate:
	./bin/ln-smf-migrate 10.0.3.10 10.0.3.14 https://leuro.adarq.org migrate $(LIMIT)

unmigrate:
	./bin/ln-smf-migrate 10.0.3.10 10.0.3.14 https://leuro.adarq.org unmigrate
