;;; company-mtg.el --- `company-mode' backend for `mtg' -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/mtg.el
;; Keywords: local
;; Created: 09 Jun 2019
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Company Backend for `mtg'.
;; 
;; See:
;; 
;; • `company-mtg'
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'radix-tree)
  (require 'seq)
  (require 'cl-lib))

;;==============================================;;

;; project:

;; (with-demoted-errors "[MTG] %s"
;;   (require 'mtg))

;;==============================================;;

;; external:

(progn
  (require 'company))

;;----------------------------------------------;;
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst mtg-syntax-card-name-charset-regexp

  (rx "")

  "`regexpp' for Card Names.")

;;----------------------------------------------;;

(defvar company-mtg-card-names-vector mtg-card-names-vector
  "Defautls to `mtg-card-names-vector'.")

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar company-mtg-card-names-trie nil

  "Trie of MTG Card Names.

a `radix-tree-p' (nil until you call `company-mtg-card-names-trie-initialize').

See:

• `company-mtg-card-names-vector'.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun company-mtg-card-names-vector (&key normalize)

  "Accessor for `company-mtg-card-names-vector'.

Inputs:

• NORMALIZE — a `booleanp'.
  Whether to `downcase' each `stringp'.

Output:

• a `sequencep'."

  (if normalize
      (seq-map #'downcase company-mtg-card-names-vector)
    company-mtg-card-names-vector))

;;----------------------------------------------;;

(cl-defun company-mtg-card-names-trie (&key force normalize)

  "Accessor for `company-mtg-card-names-trie'.

Inputs:

• FORCE — a `booleanp'.
  See `company-mtg-card-names-trie-initialize'.
• NORMALIZE — a `booleanp'.
  See `company-mtg-card-names-vector'.

Output:

• a `radix-tree-p'."

  (progn
    (company-mtg-card-names-trie-initialize :force force :normalize normalize)
    company-mtg-card-names-trie))

;;----------------------------------------------;;

(cl-defun company-mtg-card-names-trie-initialize (&key force normalize)

  "Initialize `company-mtg-card-names-trie'.

Inputs:

• FORCE — a `booleanp'.
  Whether to re-initialize the trie.
  t when called interactively.
• NORMALIZE — a `booleanp'.
  See `company-mtg-card-names-vector'.

Effects:

• Modifies `company-mtg-card-names-trie'."

  (interactive (list :force t))

  (let* ((INITIALIZE? (or force
                          (not (bound-and-true-p company-mtg-card-names-trie))))
         )

    (when INITIALIZE?

      (let* ((TRIE (company-mtg/radix-tree/from-seq (or (bound-and-true-p mtg-card-names)
                                                        (company-mtg-card-names-vector :normalize normalize))))
             )

        (setq company-mtg-card-names-trie TRIE)))))

;;----------------------------------------------;;

(cl-defun company-mtg-complete-card-name (&key prefix)

  "Complete PREFIX as an MTG Card Name.

Inputs:

• PREFIX — a `stringp'.
  The prefix to complete.
  Match via `string-prefix-p'.

Output:

• a `listp' of `stringp's.

Examples:

• M-: (company-mtg-complete-card-name :prefix \"ancestral \")
    ⇒ '(\"Ancestral Knowledge\" \"Ancestral Mask\" \"Ancestral Memories\" \"Ancestral Recall\" \"Ancestral Statue\" \"Ancestral Tribute\" \"Ancestral Vengeance\" \"Ancestral Vision\")

See:

• thing ‘mtg-card-name’."

  (let* ((PREFIX      (or prefix ""))
         (TRIE        (company-mtg-card-names-trie))
         (IGNORE-CASE t)
         (CANDIDATES  (company-mtg/radix-tree/suffixes TRIE PREFIX :ignore-case IGNORE-CASE))
         )

    CANDIDATES))

;; ^ M-: (company-mtg-complete-card-name :prefix "ancestral ")
;;     ⇒ '("Ancestral Knowledge" "Ancestral Mask" "Ancestral Memories" "Ancestral Recall" "Ancestral Statue" "Ancestral Tribute" "Ancestral Vengeance" "Ancestral Vision")

;;----------------------------------------------;;

(defun company-mtg-grab-card-name ()

  "Grab the ‘mtg-card-name’ before `point'.

Notes:

• Idiomatic card names:

    • should (mostly) have capitalized words;
    • may include lowercase articles;
    • may have some punctuation characters (hyphen, apostrophe, colon).

If point is at the end of a word, return it.
Otherwise, if point is not inside a symbol, return an empty string."

  (pcase-let* ((`(,BEG-POINT . ,END-POINT)
                (bounds-of-thing-at-point 'mtg-card-name))

               (POINT (point))

               (TEXT (buffer-substring BEG-POINT POINT))
               )

    TEXT))

;; 

;;----------------------------------------------;;

(defun company-mtg-card-name-doc (name)

  "Return a “docstring” for (the card named) NAME.

Inputs:

• NAME — a `stringp'.

Output:

• a `stringp' (possibly `propertize'd)."

  (let* ((DOC (identity name)))
    DOC))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun company-mtg/radix-tree/insert-true (tree word)

  "Specialized `radix-tree-insert'.

Reducer for constructing a `radix-tree-p'."

  (radix-tree-insert tree word t))

;;----------------------------------------------;;

(defun company-mtg/radix-tree/from-seq (words)

  "Return a `radix-tree-p' from WORDS.

Inputs:

• WORDS — a `sequencep' of `stringp's.

Examples:

• M-: (company-mtg/radix-tree/from-seq '(\"application\" \"appetizer\" \"applicative\" \"apple\"))
    → '((\"app\" (\"l\" (\"icati\" ... ...) (\"e\" . t)) (\"etizer\" . t)))

Links:

• URL `http://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html'"

  (seq-reduce #'company-mtg/radix-tree/insert-true words radix-tree-empty))

;; ^ M-: (company-mtg/radix-tree/from-seq '("application" "appetizer" "applicative" "apple"))
;;     ⇒ '(("app" ("l" ("icati" ("on" . t) ("ve" . t)) ("e" . t)) ("etizer" . t)))
;;
;;   M-: (radix-tree-subtree (company-mtg/radix-tree/from-seq '("application" "appetizer" "applicative" "apple")) "appli")
;;     ⇒ '(("cati" ("on" . t) ("ve" . t)))
;;

;;----------------------------------------------;;

(cl-defun company-mtg/radix-tree/suffixes (trie prefix &key ignore-case)

  "Return all suffices of PREFIX (in TRIE).

Inputs:

• TRIE        — a `radix-tree-p'.
• PREFIX      — a `stringp'.
• IGNORE-CASE — a `booleanp'.

Examples:

• M-: (company-mtg/radix-tree/suffixes (company-mtg/radix-tree/from-seq (list \"application\" \"appetizer\" \"applicative\" \"apple\")) \"appli\" :ignore-case t)
    → '(\"application\" \"applicative\")"

  (let* ((SET (make-hash-table)))

    (progn

      (radix-tree-iter-mappings
       trie
       (lambda (k v)
         (when (string-prefix-p prefix k ignore-case)
           (puthash k t SET)))))

    (hash-table-keys SET)))

;; ^ M-: (company-mtg/radix-tree/suffixes (company-mtg/radix-tree/from-seq '("application" "appetizer" "applicative" "apple")) "appli" :ignore-case t)
;;     ⇒ '("application" "applicative")
;;
;;   M-: (string-prefix-p "appli" "application" t)
;;     ⇒ t
;;

;;----------------------------------------------;;
;; `company' Backend ---------------------------;;
;;----------------------------------------------;;

(defun company-mtg (command &optional argument &rest _)

  "Company Backend for MTG Cards.

Inputs:

• COMMAND — a `symbolp'.
  the Company Command to handle.
• ARGUMENT — a `stringp'. Either:

    • the Completion Prefix.
    • a Completion Candidate.

Features:

• Completion for MTG Card Names."

  (pcase command

    ('interactive
     (company-begin-backend 'company-mtg))

    ('init (company-mtg-card-names-trie-initialize))

    ('prefix
     (company-mtg-grab-card-name))

    ('candidates
     (let* ((PREFIX argument))
       (company-mtg-complete-card-name :prefix PREFIX)))

    ('annotation
     (let* ((CANDIDATE argument))
       (concat (unless company-tooltip-align-annotations " → ")
               (company-mtg-card-name-doc CANDIDATE))))

    ('ignore-case t)

    ('post-completion (progn))))

;; 

;;----------------------------------------------;;
;; Loading / Unloading -------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun company-mtg-setup ()

  "Setup `company-mtg'."

  (add-to-list (make-local-variable 'company-backends) 'company-mtg))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'company-mtg)

;;; company-mtg.el ends here