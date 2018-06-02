EMACS ?= emacs
RM ?= rm
ELC = gnome-c-align.elc gnome-c-snippet.elc gnome-c-style.elc

all: $(ELC)

%.elc: %.el
	$(EMACS) -Q -batch --eval "(setq load-path (cons nil load-path))" \
		-f batch-byte-compile $<

check:
	$(EMACS) -Q -batch --eval "(setq load-path (cons nil load-path))" \
		-l ert -l gnome-c-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(RM) -rf $(ELC)
