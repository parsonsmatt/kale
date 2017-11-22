ghcid:
	ghcid --test=":main --fail-fast --color" --command "stack ghci --test --main-is=kale:test:kale-test"

example-dump: example/src/Lib.hspp
	cd example && stack exec -- ghc src/Lib.hs -E

PHONY: ghcid
