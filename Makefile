ghcid:
	ghcid --test=":main --fail-fast --color" --command "stack ghci --test --main-is=kale:test:kale-test"

PHONY: ghcid
