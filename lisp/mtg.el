;;; mtg.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/mtg.el
;; Keywords: local
;; Created: 06 Jun 2019
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

;; 
;; 
;; Features include:
;; 
;; • Completion for writing custom “Magic: The Gathering” cards.
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin requirements:

(progn
  (require 'cl-lib))

;;==============================================;;

;; project requirements:

(progn
  (require 'mtg-types)
  (require 'mtg-images)
  (require 'mtg-search)
  (require 'mtg-mode))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup mtg

  nil

  "Customize “Magic: The Gathering”."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/mtg.el"))

;;==============================================;;

(defcustom mtg-card-name-punctuation-characters-list

  (list ?\- ?\, ?\. ?\:
        ?\" ?\' ?\! ?\?)

  "Punctuation in Card Names

a `listp' of `characterp's.

For example, these black-bordered cards 
have such punctuation characters:

• “Borrowing 100,000 Arrows”
• “To Arms!”
• “Looter il-Kor”
• “Yawgmoth's Will”"

  :type '(repeat (character))

  :safe #'listp
  :group 'mtg)

;; ^ « " !\"',-.01:?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzàáâéíöúû" »

;;----------------------------------------------;;

(defcustom mtg-card-name-article-strings-list

  '("a" "an" "and" "as" "at" "but" "by" "en" "for" "from" "il" "in" "into" "of" "on" "or" "the" "to" "upon" "with")

  "Articles in Card Names.

a `listp' of `stringp's."

  :type '(repeat (character))

  :safe #'listp
  :group 'mtg)

;; ^ « " !\"',-.01:?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzàáâéíöúû" »

;;----------------------------------------------;;
;; Regexps -------------------------------------;;
;;----------------------------------------------;;

(defun mtg-card-name-regexp ()

  "Return a `regexpp' matching an ‘mtg-card-name’.

For example, this command matches these ‘mtg-card-name’:

• “Empty the Warrens”
• “Borrowing 100,000 Arrows”"

  (let* ((CHAR-REGEXP
          (rx-to-string `(char alpha digit ,@mtg-card-name-punctuation-characters-list) t))

         (ARTICLE-REGEXP
          (regexp-opt mtg-card-name-article-strings-list 'word))

         (WORD-REGEXP
          (rx-to-string `(or (regexp ,ARTICLE-REGEXP)
                             (and word-start
                                  (and (char upper digit)
                                       (0+ (regexp ,CHAR-REGEXP)))
                                  word-end))
                        t))

         (PHRASE-REGEXP
          (rx-to-string `(1+ (regexp ,WORD-REGEXP))))
         )

    PHRASE-REGEXP))

;; ^ M-: (mtg-card-name-regexp)
;;     ⇒ "\\(?:\\(?:\\(a\\(?:nd\\|[nst]\\)?\\|b\\(?:ut\\|y\\)\\|en\\|f\\(?:or\\|rom\\)\\|i\\(?:nto\\|[ln]\\)\\|o[fnr]\\|t\\(?:he\\|o\\)\\|upon\\|with\\)\\|\\<[[:upper:]][!\"',-.:?[:digit:][:alpha:]]*\\>\\)+\\)"

;; e.g. ‘mtg-card-name’s:
;;
;; • card Ancestral Recall card
;; • card Looter il-Kor card
;; • card Borrowing 100,000 Arrows card
;; • card To Arms! card
;; • card Empty the Warrens card
;;
;; (re-search-backward "\\(?:\\(?:\\(a\\(?:nd\\|[nst]\\)?\\|b\\(?:ut\\|y\\)\\|en\\|f\\(?:or\\|rom\\)\\|i\\(?:nto\\|[ln]\\)\\|o[fnr]\\|t\\(?:he\\|o\\)\\|upon\\|with\\)\\|\\<[[:upper:]][!\"',-.:?[:digit:][:alpha:]]*\\>\\)+\\)")
;;

;;----------------------------------------------;;
;; Motion --------------------------------------;;
;;----------------------------------------------;;

(put 'mtg-card-name 'forward-op

     (defun mtg-forward-card-name (&optional count)

       "Move across MTG Card Names.

Inputs:

 • COUNT — an optional `integerp'.
   When `called-interactively-p', the “Prefix Argument”. 
   If COUNT is:
       ° positive — Move forwards to (the end of) the next card name.
       ° negative — Move backwards to (the end of) the prior card name.

Effects:

• Moves `point'.

Metadata:

• Registered with ‘thingatpt.el’ as `mtg-card-name’ —
  Implements the property `forward-op' 
  for the “thing” symbol `mtg-card-name'.

Usage

• M-: (thing-at-point 'mtg-card-name)

Notes:

• Card Names, idiomatically:

    • must end with, and should start with, a `capitalize'd word.
      (should mostly contain capitalized words, anyways).
    • may have some punctuation characters (comma, hyphen, apostrophe, colon).
    • may include `downcase'd articles."

       (interactive "P")

       (let* ((COUNT (or count +1))
              )

         ())))

;; ^ Notes:
;;
;; • « (put '_ 'forward-op #'_) » registers a “Thing” for `thingatpt.el'.
;;

;;----------------------------------------------;;
;; Loading / Unloading -------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun mtg-setup ()

  "Setup `mtg'.

”Setup“ includes:

• Registering `mtg-mode' with `auto-mode-alist'.
• Registering `mtg-mode' with `interpreter-mode-alist'.

Related:

• Gated by `mtg-setup-p'.
• Inverted by `mtg-unload-function'."

  (progn

    (add-to-list 'auto-mode-alist (cons mtg-filepath-regexp #'mtg-mode))

    (with-eval-after-load 'company
      (with-demoted-errors "[MTG] Company] %s"
        (when (require 'mtg-company)
          (mtg-company-setup))))

    ()))

;;----------------------------------------------;;

(defun mtg-unload-function ()

  "`unload-feature' for `mtg'.

Inverts `mtg-setup' and `inferior-mtg-setup' 
(which get executed by « (load \"mtg.el\") »).

Effects:

• Unregisters `mtg-mode' from `auto-mode-alist'.
• Unregisters `mtg-mode' from `interpreter-mode-alist'."

  (progn

    (setq auto-mode-alist
          (cl-remove #'mtg-mode auto-mode-alist        :test #'equal :key #'cdr))

    (setq interpreter-mode-alist
          (cl-remove #'mtg-mode interpreter-mode-alist :test #'equal :key #'cdr))

    ()))

;;----------------------------------------------;;
;; Effects -------------------------------------;;
;;----------------------------------------------;;

(when (bound-and-true-p 'mtg-setup-p)
  (mtg-setup))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg)

;;; mtg.el ends here