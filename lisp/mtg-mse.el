;;; mtg-mse.el --- “Magic Set Editor” integration for ‘mtg.el’ -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 13 Jul 2019
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

;; “Magic Set Editor” Integration:
;;
;; • Import/Export ‹.mse-set› files.
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

;; project

(progn
  (require 'mtg))

;;----------------------------------------------;;
;;; Constants ----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Variables ----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg-mse)

;;; mtg-mse.el ends here