TESTID=$(shell date +%Y%m%d%H%M%S)
TESTFILES=$(shell find . -type f -name "*.php*")
TESTFILESS=$(TESTFILES:=.phptest)
OUTLOG=.tests.out

# HUnit-enabled tests: They can be executed separately using 'make test_hunit_only'
# and are run using ghc after preprocessing with tools/hutest2hs.
HUTESTFILES=$(shell find . -type f -name "*.hutest")
HUTESTFILESS=$(HUTESTFILES:=.hutesttest)
HUNIT_LOG=".hunit.out"

.PHONY: test test_parser_only test_hunit_only clean failed summary

test: clean stamp $(TESTFILESS) $(HUTESTFILESS)
	@$(MAKE) -s summary

test_hunit_only: clean stamp $(HUTESTFILESS)
	@$(MAKE) -s summary

test_parser_only: clean stamp $(TESTFILESS)
	@$(MAKE) -s summary

%.phptest:
	@(cat $(@:.phptest=) | phpsa --dump-ast > /dev/null 2> /dev/null && echo "$@ : OK" || echo "$@ : FAIL") >> $(OUTLOG)

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
	@grep " : FAIL" $(OUTLOG) | cut -d " " -f 1 | sed -e "s/\.phptest$$//g"
