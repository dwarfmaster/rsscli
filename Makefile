
all:
	nix-shell --command "cabal v1-build"

doc:
	nix-shell --command "cabal v1-haddock --executables"

hoogle:
	nix-shell --command "cabal v1-haddock --executables --hoogle"

ghci:
	nix-shell --command "cd src; ghci Main.hs"

.PHONY:all doc hoogle ghci


