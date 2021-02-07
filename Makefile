include Makefile.base

.PHONY: gen-docs
gen-docs:
	# Generate docs for hackage
	rm -rf dist-newstyle/blanks-*-docs.tar.gz
	cabal test
	cabal haddock --haddock-for-hackage --haddock-option=--hyperlinked-source

.PHONY: upload-docs
upload-docs: gen-docs
	# Upload docs to hackage
	cabal upload --publish -d dist-newstyle/blanks-*-docs.tar.gz

