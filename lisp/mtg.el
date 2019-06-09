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