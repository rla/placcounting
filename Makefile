PROLOG = swipl
INSTALL=/usr/bin/install
PLBASE = $(shell eval `$(PROLOG) --dump-runtime-variables` && echo $$PLBASE)
DEST = $(PLBASE)/library/placcounting
SRC = src/database.pl src/analysis.pl src/reporting.pl src/main.pl
BIN = placcounting
	
install:
	mkdir -p $(DEST)
	$(INSTALL) -m 0644 $(SRC) $(DEST)
	$(INSTALL) -m 0755 $(BIN) /usr/local/bin

uninstall:
	rm -r $(DEST)

.PHONY: install uninstall
