# ‹mtg› Makefile

### Notes:

## Targets:
#
# • Standard targets — « make {build,check,dist,install} ».
# • Release targets  — « make {upload,publish}-{melpa,github} ».
#
# « $ make help »:
#
# • ‹build› — 
# • ‹check› — 
# • ‹dist› — 
# • ‹install› — 
# • ‹all› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
#
# • Standard targets — « make {build,check,dist,install} ».
# • Release targets  — « make {upload,publish}-{melpa,github} ».
#

## Files:
#
# •
#
# required and/or generated Files/Directories include:
#
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 

## Environment Variables:
#
# • Inherited environment variables — ‹$EMACS›, ‹$EMACSLOADPATH›, 
# • Custom environment variables    — ‹$EFLAGS›
#
# optional Environment Variables include:
#
# • ‹$EMACS› — 
# • ‹› — 
# • ‹› — 
# • ‹› — 

### Settings:

SHELL=bash

.EXPORT_ALL_VARIABLES:

### Constants:

PKG := mtg

SUMMARY := ""

# ^ Extract, from the very first line of the Package File,
#   text between « --- » (or bol) and « -*- » (or eol).

PKG=mtg

# ^ (Elisp) Package Name.
#
# the name of each (Elisp) Library in this Multi-Library Package
# should be prefixed. i.e. either ‹PKG-*.el› or ‹PKG.el› itself.
#

VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' $(PKG).el)"

# ^ Extract ‹ ;; Version: › Library Header.
#
# Why? Because then the package has a canonical
# version.
#

### Variables:

outdir ?=$(CURDIR)/out

# ^ Output Directory — install the targets' outputs into this directory.

tmpdir ?=$(CURDIR)/tmp

# ^ Temporary Directory — run the targets' commands within this directory.

srcdir ?=$(CURDIR)/lisp

# ^ Source Directory — find lisp source files within this directory.

EMACSLOADPATH ?= -L$(scrdir)/lisp

# ^  List of directories (colon-separated).
#
# the ‹EMACSLOADPATH› environment variable,
# as well as the ‹-L› command-line option,
# registers directories with the ‹load-path› elisp variable.
#

EFLAGS ?= --eval "(when (boundp 'load-prefer-newer) (setq load-prefer-newer t))" --eval "(setq byte-compile-error-on-warn t)"

# ^ Command-Line Options (not Arguments) for ‹emacs›.
#
# ‹EFLAGS› abbreviates “Emacs Flags”.
#

DISTFILE = $(PKG)-$(VERSION).tar.gz

# ^ Distribution Tarball — Distribute this Multi-File Package as a versioned tarball.
#
# Why? Because:
#
# • Archiving simplifies downloading.
# • Compression accelerates downloading.
#   ‹ gzip -c9 › shrinks source files by ~90%.
#

### Programs:

# EMACS := emacs

EMACS ?= $(shell which "$${EMACS}" 2> /dev/null || which "emacs" 2> /dev/null)

EmacsVersion := $(shell "$(EMACS)" -Q --batch --eval '(princ emacs-version)')

# ^ Respect ‹ $EMACS › Environment Variable, if present.
#
# Why? Because then the following invocations are equivalent:
#
# • $ export EMACS=/path/to/emacs && make
# • $ EMACS=/path/to/emacs make
# • $ make EMACS=/path/to/emacs
#
# Useful when:
#
# • ~/.bash_profile sets $EMACS
#

GZIP := gzip

GZipVersion := $(GZIP) --version

# ^ Compressor Program.

TAR := tar

TarVersion := $(TAR) --version

# ^ Archiver Program.

MKDIR ?= mkdir -p

# ^ ‹mkdir› options:
#
# • ‹ -p › — Create parent directories.
#

### Files:

EL  := $(PKG).el
EL  += $(wildcard $(PKG)-*.el)

ELC := $(EL:%.el=$(tmpdir)/%.elc)

# EL := $(filter-out $(PKG)-autoloads.el $(PKG)-pkg.el,$(wildcard *.el) $(PKG).el)
# ELC =$(EL:.el=.elc)

JSON := Vintage.json.gz

### Derived Constants:

EmacsBuild := EMACSLOADPATH=$(EMACSLOADPATH) emacs -Q --batch -f batch-byte-compile

# ^ ‹emacs› options:
#
# • ‹ -f batch-byte-compile › — calls the ‹batch-byte-compile› Elisp Function
#   on the following filepath(s?).
#
# • ‹ --batch › — non-interactive ‹emacs›.
#
# • ‹ -Q › — minimize/disable initializations. abbreviates “quiet”.
#
# • ‹ $EMACSLOADPATH › — 
#

EmacsTest := EMACSLOADPATH=$(EMACSLOADPATH) emacs 

# ^ ‹cask› options:
#
# • 
#

GZipCompress := $(GZIP) --keep -c9

# ^ ‹gzip› options:
#
# • ‹ -c9 › — Compress with maximum compression
#   (i.e. Compression Level 9).
#   Smaller output is paid for by longer runtime.
#
# • ‹ --keep › — “Don't delete input files during compression or decompression (i.e. keep them).”
# • 
#

### File Targets:

%.elc: %.el

	$(MKDIR) $(dir $@)
	$(EmacsBuild) $(EFLAGS) $<

# $(tmpdir)/%.elc: $(srcdir)/%.el

#

$(EL_GZ): $(EL)

	$(GZipCompress) $<

#

: $(EL)

	$(GZipCompress) $<

# 

define LOADDEFS_TEMPLATE
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TEMPLATE
#'

$(PKG)-autoloads.el: $(EL)

	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TEMPLATE" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

#

$(AUTOLOADS): $(ELFILES)

	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval "(setq generated-autoload-file (concat command-line-default-directory \"/\" \"$@\"))" \
		-f batch-update-autoloads "."

#

$(outdir):

	$(MKDIR)  $(outdir)

$(tmpdir):

	$(MKDIR)  $(tmpdir)

$(tmpdir)-$(EmacsVersion):

	$(MKDIR) $@

### PHONY Targets (standard)...

build: $(ELC)

.PHONY: build

#

check: build $(TEST_ELC)

	$(EmacsTest) $<

.PHONY: check

#

dist: $(TGZ)

.PHONY: dist 

#

You want to tar your files together and gzip the resulting tar file.

tar -cvzf cvd.tar.gz cvd*.txt

if you name the file with the extension .tgz, (short for tar gz), then Windows programs will recognize it as something that winzip etc can process as is.

archiver such as tar or zip. GNU tar supports the -z option to invoke gzip transparently. gzip is designed as a complement to tar, not as a replacement

#

install: $(TGZ)

	mv $^ $(outdir)

.PHONY: install

#

clean:
	rm -f -r $(tmpdir)

.PHONY: clean

#

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make clean        - remove generated files)
	@printf "\n"

.PHONY: help

### PHONY Targets (custom)...

build-loaddefs:



check-ert: $(ELCHECKS)
	$(BATCH) --eval "(when (= emacs-major-version 24)					\
                           (require 'undercover)						\
                           (undercover \"*.el\"							\
                              (:exclude \"haskell-mode-pkg.el\" \"haskell-compat.el\")))"	\
                 -L tests									\
                 $(patsubst %,-l %,$(ELCHECKS))							\
                 -f ert-run-tests-batch-and-exit
	@echo "checks passed!"

#

check-autoloads: $(AUTOLOADS)

  # Check if Autoloads load successfully:

	$(EmacsBuild) -l "$@"

#


























SRCDIR := ./lisp

SOURCES := $(sort $(wildcard ./$(SRCDIR)/*$(PKG)*.el))
OBJECTS := $(EL:.el=.elc)

# ^ e.g. 
#
# ≈ (mtg.el mtg-mode.el …)
# ≈ (mtg.elc mtg-mode.elc …)
#

TESTDIR  := ./test
TESTFILE := $(TESTDIR)/test-$(PKG).el

BENCHDIR  := ./bench
BENCHFILE := $(BENCHDIR)/bench-$(PKG).el

#

EmacsRun   := $(EMACS) -Q --batch
EmacsEval  := $(EMACS) --eval

EmacsBuild := $(EMACS) -Q --batch -L$(SRCDIR)
EmacsCheck := $(EMACS) -Q --batch -L$(SRCDIR) -L$(DIR) --load ert

EmacsBench := $(EMACS) -Q --batch -L$(SRCDIR) -L$(BENCHDIR) --load benchmark

#

# ‹*.el› → ‹*.elc›

%.el: %.elc

	$(EmacsBuild) -f batch-byte-compile $^

#

build: bytecompile

.PHONY: build

#

check: ert

.PHONY: check

#

bytecompile: $(SOURCES)

	$(EmacsBuild) -f batch-byte-compile $^

.PHONY: bytecompile

#

ert: $(TESTFILE)

	$(EmacsCheck) --load $< -f ert-run-tests-batch-and-exit 

.PHONY: ert

#

bench: $(BENCHFILE)

	$(EmacsBench) --load "bench-$(PKG).el" -f batch-byte-compile 
	$(EmacsBench) --load "bench-$(PKG).elc" -f batch-benchmark-mtg

.PHONY: bench

#

elpa: *.el
	@ver=`grep -o "Version: .*" history.el | cut -c 10-`; \
	dir=history-$$ver; \
	mkdir -p "$$dir"; \
	cp `git ls-files '*.el' | xargs` history-$$ver; \
	echo "(define-package \"history\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/history-pkg.el; \
	tar cvf history-$$ver.tar "$$dir"

.PHONY: elpa

#

release:

	git fetch && \
	git diff remotes/origin/master --exit-code && \
	git tag -a -m "Release" release-$(VERSION) && \
	# woger lua-l lua-mode lua-mode "release $(VERSION)" "Emacs major mode for editing Lua files" release-notes-$(VERSION) http://github.com/immerrr/lua-mode/ && \

	git push origin master
	@echo 'Send update to ELPA!'

.PHONY: release



















