build:
	stack build --file-watch

docs:
	cabal haddock --hyperlink-source

exec:
	echo exec

exec_prod:
	echo exec

exec_prod_sudo:
	echo exec

ghci:
	echo ghci

install:
	stack install --local-bin-path=./bin

weird:
	brew install icu4c
	stack install text-icu --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include
	# http://stackoverflow.com/questions/7420514/using-text-icu-library-in-haskell-on-mac-os
