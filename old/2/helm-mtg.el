;;; helm-mtg.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
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

;; Helm Integration for MTG.
;; 
;; `helm' comamnds/sources for:
;;
;; • `mtg-read-card'
;; • `mtg-query-cards'
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
  (require 'url)
  (require 'mm-url)
  (require 'seq)
  (require 'cl-lib))

;;==============================================;;

;; project:

;; (progn
;;   (require 'mtg))

;;==============================================;;

(progn
  (require 'helm))

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;

(defun helm-mtg/browse-scryfall (card-name)

  "Browse Scryfall for CARD-NAME.

Inputs:

• CARD-NAME — a `stringp'.

Examples:

• M-: (helm-mtg/browse-scryfall \"Ancestral Recall\")
   ;; (browse-url \"https://scryfall.com/search?q=ancestral+recall&order=released&dir=asc&as=grid&unique=art\")
    ↪ nil

Related:

• Calls ‘browse-url'
• Browses URL `https://scryfall.com/search'"

  (let* ((CARD-NAME (downcase card-name))
                                       
                                        ; ^ ① lowercase letters.


         (URL-QUERY (mm-url-encode-www-form-urlencoded `(("q" . ,CARD-NAME) ("order" . "released") ("dir" . "asc") ("as" . "grid") ("unique" . "art"))))

                                        ; ^ ② replace spaces with plus-signs.

         (URL (format "https://scryfall.com/search?%s" URL-QUERY))
         )

    (browse-url URL)))

;; ^ e.g. (helm-mtg/browse-scryfall "Ancestral Recall")

;; ^ notes:
;;
;; M-: (mm-url-encode-www-form-urlencoded '(("q" . "Ancestral Recall") ("order" . "released") ("dir" . "asc") ("as" . "grid") ("unique" . "art")))
;;    ↪ "q=Ancestral+Recall&order=released&dir=asc&as=grid&unique=art"
;;

;;----------------------------------------------;;
;;; Helm Actions -------------------------------;;
;;----------------------------------------------;;

(defconst helm-mtg/card-name-actions

  (helm-make-actions "Insert"   #'insert
                     "Copy"     #'helm-kill-new
                     "Scryfall" #'helm-mtg/browse-scryfall
                     )

  "Helm Actions for MTG Card Names.")

;;----------------------------------------------;;
;;; Helm Sources -------------------------------;;
;;----------------------------------------------;;

(defconst helm-mtg/card-name-source

  (helm-build-sync-source "MTG Card Names"

    :candidates (mtg-cards :type 'list :quick t :force nil)

    :action 'helm-mtg/card-name-actions

    )

  "Helm Source for MTG Card Names.")

;;----------------------------------------------;;
;;; Helm Commands ------------------------------;;
;;----------------------------------------------;;

(defun helm-mtg/card-names ()

  "Helm Command for MTG Card Names."

  (interactive)

  (helm :buffer "*Helm MTG Card Names*"

        :sources '(helm-mtg/card-name-source)

        :input (word-at-point)

        ))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; Links:
;; 
;; • URL `https://github.com/emacs-helm/helm/wiki/Developing#creating-a-source'
;; • URL `http://kitchingroup.cheme.cmu.edu/blog/2015/02/01/Handling-multiple-selections-in-helm'
;; 

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'helm-mtg)

;;; helm-mtg.el ends here