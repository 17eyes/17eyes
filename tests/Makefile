TESTID=$(shell date +%Y%m%d%H%M%S)
OUTLOG=.tests.out

PARSERTESTFILES=$(shell find . -type f -name "*.php*")
PARSERTESTFILESS=$(PARSERTESTFILES:=.parser)

# HUnit-enabled tests: They can be executed separately using 'make test_hunit_only'
# and are run using ghc after preprocessing with tools/hutest2hs.
HUTESTFILES=$(shell find . -type f -name "*.hutest")
HUTESTFILESS=$(HUTESTFILES:=.hutesttest)
HUNIT_LOG=".hunit.out"

# Unparse tests
UNPARSERTESTFILES=$(shell find . -type f -name "*.php*")
UNPARSERTESTFILESS=$(UNPARSERTESTFILES:=.unparser)

.PHONY: test test_parser_only test_hunit_only test_unparser_only clean failed summary

test: clean stamp $(PARSERTESTFILESS) $(HUTESTFILESS) $(UNPARSERTESTFILESS)
	@$(MAKE) -s summary

test_hunit_only: clean stamp $(HUTESTFILESS)
	@$(MAKE) -s summary

test_parser_only: clean stamp $(PARSERTESTFILESS)
	@$(MAKE) -s summary

test_unparser_only: clean stamp $(UNPARSERTESTFILESS)
	@$(MAKE) -s summary

%.parser:
	@(cat $(@:.parser=) | phpsa --dump-ast > /dev/null 2> /dev/null && echo "$@ : OK" || echo "$@ : FAIL") >> $(OUTLOG)

%.unparser:
	@(cat $(@:.unparser=) | phpsa --unparse 2> /dev/null | diff -B -q $(@:.unparser=) > /dev/null - && echo "$@ : OK" || echo "$@ : FAIL") >> $(OUTLOG)

%.hutesttest:
	@echo "====== $@" >> $(HUNIT_LOG)
	@../tools/hutest2hs "$(@:.hutesttest=)"
	@(ghc -i../src:../tools -fobject-code -e "runTests tests" "$(@:.hutesttest=.hs)" >> $(HUNIT_LOG) 2>> $(HUNIT_LOG) && echo "$@ : OK" || echo "$@ : FAIL") >> $(OUTLOG)
	@rm "$(@:.hutesttest=.hs)"

stamp:
	@echo "TESTID=$(TESTID)" >> $(OUTLOG)

clean:
	@rm -f $(OUTLOG) $(HUNIT_LOG)

summary:
	@echo "ID: "`grep "TESTID=" $(OUTLOG)`
	@echo -n "Test cases: "`cat $(OUTLOG) | grep -c -v "TESTID=" $(OUTLOG)`
	@echo
	@echo "OK: "`grep -c ": OK" $(OUTLOG)`
	@echo "Failed: "`grep -c " : FAIL" $(OUTLOG)`

failed:
	@grep " : FAIL" $(OUTLOG) | cut -d " " -f 1 | sed -e "s/\.parser$$//g"

# show targets
help:
	@grep "^\.PHONY" Makefile | tr " " "\n" | grep -v PHONY
