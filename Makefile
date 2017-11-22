ghcid:
	ghcid --test=":main --fail-fast --color" --command "stack ghci --test --main-is=kale:test:kale-test"

example-dump:
	cd example && stack exec -- ghc src/Lib.hs -E

hpc:
	stack clean && stack build --coverage && stack test --coverage

test:
	stack test && cd example && stack build

lint:
	stack build hlint && stack exec -- hlint .

PHONY: ghcid example-dump test hpc
