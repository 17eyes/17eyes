# test suite directories
SUBDIRS =   bare_strings \
            cmd_execution \
            code_execution \
            colons \
            control_flow \
            crap \
            csrf \
            eof_junk \
            file_disclosure \
            file_inclusion \
            header_injection \
            ldap_injection \
            lossy_coercion \
            open_redirect \
            parser \
            parser_bugs \
            runtime \
            sql_injection \
            stdlib \
            undefined \
            weak_hash \
            xss

TESTID=$(shell date +%Y%m%d%H%M%S)
TESTFILES=$(shell find . -type f -name "*.php*")
TESTFILESS=$(TESTFILES:=.phptest)
OUTLOG=.tests.out
.PHONY: test clean summary

%.phptest:
	@(cat $(@:.phptest=) | phpsa > /dev/null 2> /dev/null && echo "$@ : OK" || echo "$@ : FAIL") >> $(OUTLOG) 

test: clean stamp $(TESTFILESS) summary

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
