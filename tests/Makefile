TESTID=$(shell date +%Y%m%d%H%M%S)
TESTFILES=$(shell find . -type f -name "*.php*")
TESTFILESS=$(TESTFILES:=.phptest)
OUTLOG=.tests.out

.PHONY: test clean summary failed

test: clean stamp $(TESTFILESS) summary

%.phptest:
	@(cat $(@:.phptest=) | phpsa > /dev/null 2> /dev/null && echo "$@ : OK" || echo "$@ : FAIL") >> $(OUTLOG) 


stamp:
	@echo "TESTID=$(TESTID)" >> $(OUTLOG)

clean:
	@rm -f $(OUTLOG)

summary:
	@echo "ID: "`grep "TESTID=" $(OUTLOG)`
	@echo -n "Test cases: "`cat $(OUTLOG) | grep -c -v "TESTID=" $(OUTLOG)`
	@echo
	@echo "OK: "`grep -c ": OK" $(OUTLOG)`
	@echo "Failed: "`grep -c " : FAIL" $(OUTLOG)`

failed:
	@grep " : FAIL" $(OUTLOG) | cut -d " " -f 1 | sed -e "s/\.phptest$$//g"
