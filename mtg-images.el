;;; mtg-images.el --- -*- coding: utf-8; lexical-binding: t -*-

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

;; MTG Images.
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

;;----------------------------------------------;;
;; Images: Unicode -----------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Images: SVG ---------------------------------;;
;;----------------------------------------------;;

(defimage mtg-tap-symbol-svg-image

  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M85.332 59.918h-36.004l13.185-9.383c-4.898-3.887-10.566-5.828-16.984-5.828-3.211 0-5.414.613-6.59 1.836-1.184 1.227-1.777 3.445-1.777 6.654 0 8.873 4.563 18.34 13.691 28.396l-10.391 10.521c-12.09-14.705-18.129-27.844-18.129-39.424 0-6.928 2.086-12.447 6.27-16.545 4.18-4.098 9.746-6.148 16.668-6.148 8.453 0 17.664 3.215 27.641 9.635l7.728-13.182 4.692 33.468z' fill='#0D0F0F'/></g></svg>"))

  "Image Specification for “{T}”, the “Tap Symbol”.")

;; e.g. M-: (insert-image mtg-tap-symbol-svg-image)

;;----------------------------------------------;;

(defimage mtg-blue-mana-symbol-svg-image

  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g fill='none'><circle fill='#C1D7E9' cx='50' cy='50' r='50'/><path d='M67.488 83.719c-4.787 4.871-10.684 7.307-17.688 7.307-7.861 0-14.098-2.69-18.711-8.073-4.359-5.127-6.537-11.662-6.537-19.606 0-8.543 3.717-18.286 11.15-29.224 6.064-8.969 13.199-16.83 21.402-23.58-1.197 5.469-1.793 9.355-1.793 11.662 0 5.299 1.664 10.467 4.996 15.508 4.102 5.98 7.219 10.426 9.357 13.328 3.332 5.043 4.998 9.955 4.998 14.737.002 7.093-2.391 13.074-7.174 17.941zm-.129-27.362c-1.281-2.861-2.777-4.762-4.486-5.703.256.514.385 1.24.385 2.18 0 1.795-.512 4.357-1.539 7.689l-1.664 5.127c0 2.99 1.492 4.486 4.484 4.486 3.16 0 4.742-2.095 4.742-6.281 0-2.134-.64-4.632-1.922-7.498z' fill='#0D0F0F'/></g></svg>"))

  "Image Specification for “{U}”, the Blue-Mana Symbol.")

;;----------------------------------------------;;

(defimage mtg-two-generic-mana-symbol-svg-image

  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M77.442 71.103l-5.896 19.898h-48.991v-4.254c2.38-2.652 7.595-8.001 15.646-16.053 4.849-4.852 9.649-9.972 14.407-15.371 2.378-2.651 4.21-4.942 5.487-6.862 2.836-4.114 4.255-8.32 4.255-12.624 0-4.204-1.301-7.912-3.908-11.112-2.607-3.204-5.97-4.808-10.09-4.808-8.871 0-15.823 5.998-20.854 17.98l-4.395-1.647c5.947-16.829 15.321-25.249 28.131-25.249 6.313 0 11.687 2.149 16.124 6.448 4.439 4.3 6.656 9.604 6.656 15.92 0 8.052-4.617 15.918-13.858 23.601l-9.604 7.956c-6.131 5.127-11.212 9.929-15.231 14.412-.28.273-.826.916-1.647 1.921h25.521c3.932 0 6.907-.776 8.918-2.335 1.735-1.372 3.434-3.98 5.08-7.821h4.249z' fill='#0D0F0F'/></g></svg>"))

  "Image Specification for “{2}”, the Generic Two-Mana Symbol.")

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg-images)

;;; mtg-images.el ends here