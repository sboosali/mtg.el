;;; mtg-company.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
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

;; .
;; 
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
  (require 'seq)
  (require 'cl-lib))

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

;; 

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar mtg-cards-by-name-table nil

  "MTG Cards, indexed by Card Name.")

;;

;;----------------------------------------------;;

(defvar mtg-cards-names-list nil

  "MTG Card Names.")

;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun mtg-company-grab-card-name ()

  "Grab the ‘mtg-card-name’ before `point'.

Idiomatic card names:

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

(defun mtg-company-card-name-doc (name)

  "Return a “docstring” for (the card named) NAME.

Inputs:

• NAME — a `stringp'.

Output:

• a `stringp' (possibly `propertize'd)."

  (let* ((DOC (identity name)))
    DOC))

;;----------------------------------------------;;

(defun mtg-company-complete-card-name (prefix)

  "Complete PREFIX as an MTG Card Name.

Inputs:

• PREFIX — a `stringp'.

Output:

• a `listp' of `stringp's.

See:

• thing ‘mtg-card-name’."

  (let* ((CANDIDATES (identity prefix)))
    CANDIDATES))

;;----------------------------------------------;;
;; `company' Backend ---------------------------;;
;;----------------------------------------------;;

(defun mtg-company (command &optional argument &rest _)

  "Company Backend for MTG Cards.

Inputs:

• COMMAND — a `symbolp'.
  the Company Command to handle.
• ARGUMENT — a `stringp'. Either:

    • the Completion Prefix.
    • a Completion Candidate."

  (case command

    (interactive (company-begin-backend 'company-mtg))

    (init
     (mtg-initialize-cards-by-name-table))

    (prefix
     (mtg-company-grab-card-name))

    (candidates
     (let* ((PREFIX argument))
       (mtg-company-complete-card-name PREFIX)))

    (annotation
     (let* ((CANDIDATE argument))
       (concat (unless company-tooltip-align-annotations " → ")
               (mtg-company-card-name-doc CANDIDATE))))

    (ignore-case t)

    (post-completion (progn))))

;; 

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg-company)

;;; mtg-company.el ends here