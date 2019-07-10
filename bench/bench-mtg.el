;;; bench-mtg.el --- Benchmarks of ‘mtg.el’ -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

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

;; $ emacs -Q -batch -f batch-byte-compile jit-bench.el
;; $ emacs -Q -batch -l jit-bench.elc -f benchmark-batch

;;; Code:

(require 'cl-lib)

(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (garbage-collect)
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

;; Test harness

(cl-defun make-random-entry (&key state (min 1) (max 5))
  (cl-loop repeat (+ min (cl-random (1+ (- max min)) state))
           for letter = (+ ?A (cl-random 26 state))
           collect (intern (format "%c" letter))))

(cl-defun make-random-database (&key state (count 100000))
  (cl-loop repeat count collect (make-random-entry :state state)))

(cl-defun benchmark-mtg (f &optional (n 10) (tags '(A B C D E F)))
  (let* ((state (copy-sequence [cl-random-state-tag -1 30 267466518]))
         (db (make-random-database :state state)))
    (cl-loop repeat n
             sum (measure-time
                   (funcall f db tags))
             into total
             finally return (/ total (float n)))))

;; Benchmark candidates:

(defun memq-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (memq tag entry)
                    return t)))

(defun member-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (member tag entry)
                    return t)))

(defalias 'memq-alias 'memq)

(defun memq-alias-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (memq-alias tag entry)
                    return t)))

(defun my-memq (needle haystack)
  (cl-loop for element in haystack
           when (eq needle element)
           return t))

(defun my-memq-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (my-memq tag entry)
                    return t)))

(defun jit-count (db tags)
  (let* ((lexical-binding (eval-when-compile lexical-binding))
         (memq-list (cl-loop for tag in tags collect `(memq ',tag entry)))
         (function `(lambda (db)
                      (cl-loop for entry in db
                               count (or ,@memq-list))))
         (compiled (byte-compile function)))
    (funcall compiled db)))

(defun benchmark-mtg-batch ()
  (let ((funcs (list 'memq-count
                     'member-count
                     'memq-alias-count
                     'my-memq-count
                     'jit-count)))
    (princ (format "%s\nlexical-binding = %s\n"
                   (version) (eval-when-compile lexical-binding)))
    (dolist (func funcs)
      (princ (format "%-16s %0.3fs\n" func (benchmark func 100))))))

;;; bench-mtg.el ends here