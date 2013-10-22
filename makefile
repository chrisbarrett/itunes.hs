binary = .cabal-sandbox/bin/itunes

~/bin/itunes : $(binary)
	cp -f $(binary) ~/bin/itunes

$(binary) :
	cabal install
