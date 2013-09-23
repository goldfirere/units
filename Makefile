all: build

.PHONY: init build doc test install ghc-head


init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

test: build
	cabal test

install: init build test
	cabal install

doc:
	cabal haddock --hyperlink-source

ghc-head:
	wget --quiet -O ghc-head.tar.bz2 http://paraiso-lang.org/html/ghc-head.tar.bz2
	tar xf ghc-head.tar.bz2
	sudo apt-get install libgmp3c2 libgmp3-dev  libghc-zlib-dev -y
	cd ghc-head/; ./configure;	sudo make install
# cabal install cabal-install # sometimes we need a latest version of cabal 
# cabal update
	ghc --version
	cabal --version
