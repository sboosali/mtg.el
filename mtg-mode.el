;;; mtg-mode.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/mtg.el#readme
;; Keywords: local
;; Created: 25 May 2019
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
  (require 'font-lock)
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;

(declare-function eww-open-file "eww")

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst mtg-file-regexp

  (rx ".mtg" eos)

  "Match a filename whose contents should be MTG cards.

a `regexpp's.

(Conforms to `auto-mode-alist'.)")

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup mtg nil

  "Edit “Magic The Gathering” cards.

Customize the behavior & appearence of `mtg-mode'."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/mtg.el#readme")

  :group 'language)

;;----------------------------------------------;;

(defcustom mtg-keywords-list

  '(
    )

  "MTG Keywords (within Rules Text).

a `listp' of `stringp's.

Links:

• URL `'."

  :type '(repeated (string :tag "Keyword"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-ability-words-list

  '(
    )

  "MTG Ability Words (within Rules Text).

a `listp' of `stringp's.

Links:

• URL `'."

  :type '(repeated (string :tag "Ability Word"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;
;; Accessors: Lists ----------------------------;;
;;----------------------------------------------;;

(defun mtg-keywords ()

  "Accessor for `mtg-keyword-*-list'.

Merges:

• keyword abilities
• keyword actions
• ability words

Links:

• URL `https://en.wikipedia.org/wiki/List_of_Magic:_The_Gathering_keywords'"

  (let* ()

    (concat mtg-keywords-list)))

;;----------------------------------------------;;
;; Hooks ---------------------------------------;;
;;----------------------------------------------;;

(defcustom mtg-mode-hook

  '()

  "Commands to run after `mtg-mode' is enabled.

a `listp' of `functionp's.

Use to enable minor modes coming with `mtg-mode' or run an
arbitrary function.

Note that  `mtg-indentation-mode' and `mtg-indent-mode' should not be
run at the same time."

  :options '(superword-mode subword-mode flyspell-prog-mode)

  :type 'hook
  :group 'mtg)

;;----------------------------------------------;;
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

(defgroup mtg-faces nil

  "Fonts and colors for Mtg Mode.

Customize the appearence of `mtg-mode'."

  :prefix 'mtg
  :group  'mtg)

;;==============================================;;

(defface mtg-default-face

  '((t :inherit default)
    )

  "Default Mtg face."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-keyword-face

  '((t :inherit font-lock-keyword-face)
    )

  "Face for Mtg keywords."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-builtin-face

  '((t :inherit font-lock-builtin-face)
    )

  "Face for Mtg builtins."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-type-face

  '((t :inherit font-lock-type-face)
    )

  "Face for Mtg types."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-constructor-face

  '((t :inherit font-lock-type-face)
    )

  "Face for Mtg constructors."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-builtin-type-face

  '((t :inherit mtg-type-face
       :slant   italic
       )
    )

  "Face for Mtg builtin types."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-variable-face

  '((t :inherit font-lock-variable-name-face)
    )

  "Face for Mtg variables."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-function-face

  '((t :inherit font-lock-function-name-face)
    )

  "Face for Mtg functions."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-operator-face

  '((t :inherit font-lock-variable-name-face)
    )

  "Face for Mtg operators."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-constant-face

  '((t :inherit font-lock-constant-face)
    )

  "Face for Mtg constants."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-string-face

  '((t :inherit font-lock-string-face)
    )

  "Face for Mtg strings."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-comment-face

  '((t :inherit font-lock-comment-face)
    )

  "Face for Mtg comments."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-comment-face

  '((t :inherit font-lock-comment-delimiter-face)
    )

  "Face for Mtg comment delimieters (i.e. “--”)."

  :group 'mtg-faces)

;; mtg-definition-face       → font-lock-function-name-face
;; mtg-operator-face         → font-lock-variable-name-face

;;----------------------------------------------;;
;; Accessors: Regexps --------------------------;;
;;----------------------------------------------;;

(defun mtg-keyword-regexp ()

  "Return a `regexp' matching any Mtg keyword.

Customize:

• Variable `mtg-keywords'"

  (mtg/regexp-opt mtg-keywords))

;;----------------------------------------------;;

(defun mtg-builtin-regexp ()

  "Return a `regexp' matching any Mtg builtin.

Customize:

• Variable `mtg-builtins'"

  (mtg/regexp-opt mtg-builtins))

;;----------------------------------------------;;

(defun mtg-type-regexp ()

  "Return a `regexp' matching any Mtg type.

Customize:

• Variable `mtg-types'"

  (mtg/regexp-opt mtg-types))

;;----------------------------------------------;;
;; Images --------------------------------------;;
;;----------------------------------------------;;

(defimage mtg-tap-symbol-image

  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M85.332 59.918h-36.004l13.185-9.383c-4.898-3.887-10.566-5.828-16.984-5.828-3.211 0-5.414.613-6.59 1.836-1.184 1.227-1.777 3.445-1.777 6.654 0 8.873 4.563 18.34 13.691 28.396l-10.391 10.521c-12.09-14.705-18.129-27.844-18.129-39.424 0-6.928 2.086-12.447 6.27-16.545 4.18-4.098 9.746-6.148 16.668-6.148 8.453 0 17.664 3.215 27.641 9.635l7.728-13.182 4.692 33.468z' fill='#0D0F0F'/></g></svg>"))

  "Image Specification for “{T}”, the “Tap Symbol”.")

;; e.g. M-: (insert-image mtg-tap-symbol-image)

;;----------------------------------------------;;

(defimage mtg-blue-mana-symbol-image

  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g fill='none'><circle fill='#C1D7E9' cx='50' cy='50' r='50'/><path d='M67.488 83.719c-4.787 4.871-10.684 7.307-17.688 7.307-7.861 0-14.098-2.69-18.711-8.073-4.359-5.127-6.537-11.662-6.537-19.606 0-8.543 3.717-18.286 11.15-29.224 6.064-8.969 13.199-16.83 21.402-23.58-1.197 5.469-1.793 9.355-1.793 11.662 0 5.299 1.664 10.467 4.996 15.508 4.102 5.98 7.219 10.426 9.357 13.328 3.332 5.043 4.998 9.955 4.998 14.737.002 7.093-2.391 13.074-7.174 17.941zm-.129-27.362c-1.281-2.861-2.777-4.762-4.486-5.703.256.514.385 1.24.385 2.18 0 1.795-.512 4.357-1.539 7.689l-1.664 5.127c0 2.99 1.492 4.486 4.484 4.486 3.16 0 4.742-2.095 4.742-6.281 0-2.134-.64-4.632-1.922-7.498z' fill='#0D0F0F'/></g></svg>"))

  "Image Specification for “{U}”, the Blue-Mana Symbol.")

;;----------------------------------------------;;

(defimage mtg-two-generic-mana-symbol-image

  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M77.442 71.103l-5.896 19.898h-48.991v-4.254c2.38-2.652 7.595-8.001 15.646-16.053 4.849-4.852 9.649-9.972 14.407-15.371 2.378-2.651 4.21-4.942 5.487-6.862 2.836-4.114 4.255-8.32 4.255-12.624 0-4.204-1.301-7.912-3.908-11.112-2.607-3.204-5.97-4.808-10.09-4.808-8.871 0-15.823 5.998-20.854 17.98l-4.395-1.647c5.947-16.829 15.321-25.249 28.131-25.249 6.313 0 11.687 2.149 16.124 6.448 4.439 4.3 6.656 9.604 6.656 15.92 0 8.052-4.617 15.918-13.858 23.601l-9.604 7.956c-6.131 5.127-11.212 9.929-15.231 14.412-.28.273-.826.916-1.647 1.921h25.521c3.932 0 6.907-.776 8.918-2.335 1.735-1.372 3.434-3.98 5.08-7.821h4.249z' fill='#0D0F0F'/></g></svg>"))

  "Image Specification for “{2}”, the Generic Two-Mana Symbol.")

;;----------------------------------------------;;
;; Font Lock -----------------------------------;;
;;----------------------------------------------;;

(defconst mtg-font-lock-keywords-keyword

  (cons (mtg-keyword-regexp) 'mtg-keyword-face)

  "Highlighting keywords.")

;;----------------------------------------------;;

(defconst mtg-font-lock-keywords-shebang

  (list (rx buffer-start "#!" (0+ not-newline) eol)
       '(0 font-lock-comment-face))

  "Highlighting the “shebang line” (e.g. « #!/bin/env mtg »).")

;;----------------------------------------------;;

(defvar mtg-font-lock-keywords

  (list mtg-font-lock-keywords-keyword
        
        )

  "`font-lock-keywords' for `mtg-mode'.

a `listp' associating `regexpp's with `facep's.

(For “Search-based Fontification”,
a.k.a. “Keyword-based Syntax-Highlighting”).")

;;----------------------------------------------;;

(defvar mtg-font-lock-defaults

  (let ((mtg-font-lock-keywords-only             t)   ; Search-Based Fontification
        (mtg-font-lock-keywords-case-fold-search nil) ; Case-Insensitive
        )
    (list 'mtg-font-lock-keywords mtg-font-lock-keywords-only mtg-font-lock-keywords-case-fold-search))

  "`font-lock-defaults' for `mtg-mode'.")

;;----------------------------------------------;;
;; Syntax --------------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defvar mtg-mode-syntax-table

  (let ((TABLE (make-syntax-table))
        )

    ;; « - » is punctuation (as an operator),
    ;; but « -- » is a comment-starter:

    (modify-syntax-entry ?- ". 123" TABLE)

    ;; « \n » is a comment-ender:

    (modify-syntax-entry ?\n ">" TABLE)

    ;; Whitespace (i.e. spaces, tabs, newlines) is conventional:

    (modify-syntax-entry ?\  " " TABLE)
    (modify-syntax-entry ?\t " " TABLE)
    ;; (see above for the Syntax Class of « \n »):

    ;; Brackets (i.e. parens, curly braces, square braces):

    (modify-syntax-entry ?\( "()"    TABLE)
    (modify-syntax-entry ?\) ")("    TABLE)
    (modify-syntax-entry ?\[ "(]"    TABLE)
    (modify-syntax-entry ?\] ")["    TABLE)
    (modify-syntax-entry ?\{ "(}1nb" TABLE) ; « "n" » means: Multi-Line Coments can be nested.
    (modify-syntax-entry ?\} "){4nb" TABLE)

    ;; Operator identifiers are like Haskell

    (modify-syntax-entry ?~  "." TABLE)
    (modify-syntax-entry ?!  "." TABLE)
    (modify-syntax-entry ?@  "." TABLE)
    (modify-syntax-entry ?\# "." TABLE)
    (modify-syntax-entry ?$  "." TABLE)
    (modify-syntax-entry ?%  "." TABLE)
    (modify-syntax-entry ?^  "." TABLE)
    (modify-syntax-entry ?&  "." TABLE)
    (modify-syntax-entry ?*  "." TABLE)
    ;; (see above for the Syntax Class of « - »)
    (modify-syntax-entry ?=  "." TABLE) ; the equal sign is the definition operator.
    (modify-syntax-entry ?+  "." TABLE)
    (modify-syntax-entry ?,  "." TABLE) ; the comma is the delimiter within any bracket.
    (modify-syntax-entry ?.  "." TABLE) ; the period is the record selection operator.
    (modify-syntax-entry ?<  "." TABLE)
    (modify-syntax-entry ?>  "." TABLE)
    (modify-syntax-entry ?/  "." TABLE)
    (modify-syntax-entry ?:  "." TABLE)
    (modify-syntax-entry ?\? "." TABLE)
    (modify-syntax-entry ?\\ "." TABLE) ; the backslash is the Lacks-Constraint type operator.
    (modify-syntax-entry ?|  "." TABLE) ; the vertical bar is the record extension operator.

    ;; « " » is a string delimiter:

    (modify-syntax-entry ?\" "\"" TABLE)

    ;; Identifiers can have apostrophes and underscores (like Haskell)
    ;; (« _ » is the “Symbol” Syntax Class):

    (modify-syntax-entry ?\' "_" TABLE)
    (modify-syntax-entry ?\_ "_" TABLE)

    ;; Identifiers can have (uppercase or lowercase) letters
    ;: and digits (like Haskell).
    ;; (« w » is the “Word” Syntax Class):

    (modify-syntax-entry ?0 "w" TABLE)  ; digits...
    (modify-syntax-entry ?1 "w" TABLE)
    (modify-syntax-entry ?2 "w" TABLE)
    (modify-syntax-entry ?3 "w" TABLE)
    (modify-syntax-entry ?4 "w" TABLE)
    (modify-syntax-entry ?5 "w" TABLE)
    (modify-syntax-entry ?6 "w" TABLE)
    (modify-syntax-entry ?7 "w" TABLE)
    (modify-syntax-entry ?8 "w" TABLE)
    (modify-syntax-entry ?9 "w" TABLE)
    (modify-syntax-entry ?a "w" TABLE)  ; letters...
    (modify-syntax-entry ?A "w" TABLE)
    (modify-syntax-entry ?b "w" TABLE)
    (modify-syntax-entry ?B "w" TABLE)
    (modify-syntax-entry ?c "w" TABLE)
    (modify-syntax-entry ?C "w" TABLE)
    (modify-syntax-entry ?d "w" TABLE)
    (modify-syntax-entry ?D "w" TABLE)
    (modify-syntax-entry ?e "w" TABLE)
    (modify-syntax-entry ?E "w" TABLE)
    (modify-syntax-entry ?f "w" TABLE)
    (modify-syntax-entry ?F "w" TABLE)
    (modify-syntax-entry ?g "w" TABLE)
    (modify-syntax-entry ?G "w" TABLE)
    (modify-syntax-entry ?h "w" TABLE)
    (modify-syntax-entry ?H "w" TABLE)
    (modify-syntax-entry ?i "w" TABLE)
    (modify-syntax-entry ?I "w" TABLE)
    (modify-syntax-entry ?j "w" TABLE)
    (modify-syntax-entry ?J "w" TABLE)
    (modify-syntax-entry ?k "w" TABLE)
    (modify-syntax-entry ?K "w" TABLE)
    (modify-syntax-entry ?l "w" TABLE)
    (modify-syntax-entry ?L "w" TABLE)
    (modify-syntax-entry ?m "w" TABLE)
    (modify-syntax-entry ?M "w" TABLE)
    (modify-syntax-entry ?n "w" TABLE)
    (modify-syntax-entry ?N "w" TABLE)
    (modify-syntax-entry ?o "w" TABLE)
    (modify-syntax-entry ?O "w" TABLE)
    (modify-syntax-entry ?p "w" TABLE)
    (modify-syntax-entry ?P "w" TABLE)
    (modify-syntax-entry ?q "w" TABLE)
    (modify-syntax-entry ?Q "w" TABLE)
    (modify-syntax-entry ?r "w" TABLE)
    (modify-syntax-entry ?R "w" TABLE)
    (modify-syntax-entry ?s "w" TABLE)
    (modify-syntax-entry ?S "w" TABLE)
    (modify-syntax-entry ?t "w" TABLE)
    (modify-syntax-entry ?T "w" TABLE)
    (modify-syntax-entry ?u "w" TABLE)
    (modify-syntax-entry ?U "w" TABLE)
    (modify-syntax-entry ?v "w" TABLE)
    (modify-syntax-entry ?V "w" TABLE)
    (modify-syntax-entry ?w "w" TABLE)
    (modify-syntax-entry ?W "w" TABLE)
    (modify-syntax-entry ?x "w" TABLE)
    (modify-syntax-entry ?X "w" TABLE)
    (modify-syntax-entry ?y "w" TABLE)
    (modify-syntax-entry ?Y "w" TABLE)
    (modify-syntax-entry ?z "w" TABLE)
    (modify-syntax-entry ?Z "w" TABLE)

    TABLE)

  "Mtg Mode's `syntax-table-p'.

For example, the hyphen character (i.e. « - ») in Mtg Mode plays several roles:

• a punctuation character (« - ») — e.g. `(2 - 3)` or `(xs --. y)`.
• the characters of a (single-line) *start-of-comment* sequence (« -- ») — e.g. « -- ... ».
• the second character of a (multi-line) *start-of-comment* sequence (« {- ») — e.g. « {- ... ».
• the first character of a (multi-line) *end-of-comment* sequence (« -} ») — e.g. « ... -} ».

These roles (punctuation and single-line comment and multi-line comment) are represented by this Syntax Entry:

    (modify-syntax-entry ?- \". 123\" `mtg-mode-syntax-table')")

;;----------------------------------------------;;
;; Paragraphs:

(defcustom mtg-paragraph-start

  (concat " *{-\\| *-- |\\|" page-delimiter)

  "`paragraph-start' for `mtg-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'mtg)

;;----------------------------;;

(defcustom mtg-paragraph-separate

  (concat " *$\\| *\\({-\\|-}\\) *$\\|" page-delimiter)

  "`paragraph-separate' for `mtg-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'mtg)

;;----------------------------------------------;;
;; Comments ------------------------------------;;
;;----------------------------------------------;;

(defcustom mtg-comment-start "-- "

  "`comment-start' for `mtg-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'mtg)

;;----------------------------;;

(defcustom mtg-comment-start-skip

  (rx (or "--" "{-" (syntax comment-start)) (0+ blank))

  "`comment-start-skip' for `mtg-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-comment-padding 0

  "`comment-padding' for `mtg-mode'."

  :type '(choice (string  :tag "Padding (string)          ")
                 (integer :tag "Padding (number of spaces)"))
  :safe t
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-comment-end

  ""

  "`comment-end' for `mtg-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'mtg)

;;----------------------------;;

(defcustom mtg-comment-end-skip

  "\\(-}\\|\\s>\\)"

  (rx (0+ blank) (or "-}" (syntax comment-end)))

  "`comment-end-skip' for `mtg-mode'."

  :type '(regexp)
  :safe #'stringp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-comment-column 40

  "`comment-columnt' for `mtg-mode'."

  :type '(integer :tag "Column")
  :safe #'integerp
  :group 'mtg)

;;----------------------------------------------;;
;; Completion ----------------------------------;;
;;----------------------------------------------;;

(cl-defun mtg-completion-at-point ()

  "`completion-at-point' for `mtg-mode'.

Behavior:

• « import \" » completes with filenames of the proper extension."

  ())

;;----------------------------------------------;;
;; Indentation ---------------------------------;;
;;----------------------------------------------;;

(defcustom mtg-basic-offset 2

  "`basic-offset' (of indentation) for `mtg-mode'."

  :type '(integer :tag "Offset")
  :safe #'integerp
  :group 'mtg)

;;==============================================;;

(defun mtg-indent-line ()

  "`indent-line-function' for `mtg-mode'.

Inputs:

• .

Output:

• either:

    • nil — indentation was performed successfully.
    • ‘noindent’ — indentation isn't possible (e.g. within a string literal).

Effects:

• Point  — may move `point'.
• Buffer — may modify the current buffer by ❶ adding whitespace or ❷ removing whitespace.

Links:

• URL `'

Related:

• `'"

  (let* (
         )

    (save-excursion

      

      ())))

;; Users expect Emacs to indent code correctly regardless of its current state. You’ll need to examine the syntax around point to calculate the current nesting level.
;;
;; - (1) This is usually a matter of searching the buffer backwards from point, counting instances of { (or equivalent scope delimiter). You then adjust the current line to be indented (* my-mode-tab-width count). Provided you’re careful with { in strings and comments, this works.
;;
;; - (2) Alternatively, Emacs provides the Simple Minded Indentation Engine (SMIE). You write a BNF grammar and you get basic indentation and movement commands for free.

;;----------------------------------------------;;
;; ElDoc ---------------------------------------;;
;;----------------------------------------------;;

(defun mtg-doc-current-info (&optional symbol)

  "`eldoc-documentation-function' for `mtg-mode'.

Input:

• SYMBOL — a `stringp' or nil.
  Mtg symbol (a function or constant or type)
  whose signature and/or documentation will be output.
  Defaults to the “symbol-at-`point'”.

Output:

• a `stringp' or nil.
  a one-line message. 
  ❶ a signature and/or ❷ documentation.

Mtg Eldoc displays the types (& kinds) 
of standard library functions (& types) and of builtins.

Examples

• M-:  (substring-no-properties (mtg-doc-current-info \"map\"))
  ⇒ \"map : forall a b. (a -> b) -> [a] -> [b]\"

Related:

• `mtg-types-table'

Links:

• URL `https://github.com/willtim/Mtg/tree/0.1.2.0/lib'"

  (interactive)

  (when-let* ((CURRENT-SYMBOL (or symbol (thing-at-point 'symbol)))
              (CURRENT-TYPE   (gethash CURRENT-SYMBOL mtg-types-table))
              (CURRENT-DOC    CURRENT-TYPE)

              (DOC            (mtg-eldoc/fontify-text CURRENT-DOC))
              )

    (when (called-interactively-p 'any)
      (message "%s" DOC))

    DOC))

;; ^ e.g.
;;
;;   M-: (mtg-doc-current-info "map")
;;     ⇒ "map : forall a b. (a -> b) -> [a] -> [b]"
;;

;;----------------------------------------------;;
;; Keymaps -------------------------------------;;
;;----------------------------------------------;;

(defvar mtg-mode-map

  (let ((KEYMAP (make-sparse-keymap)))

    (define-key KEYMAP (kbd "C-c C-h") #'mtg-mode-help)

    KEYMAP)

  "Keymap for `mtg-mode'.

its “Prefix Command” (function `mtg-mode-map')
is bound to « \\[mtg-mode-keymap] ».

its current bindings are:

\\{mtg-mode-keymap}")

;;----------------------------------------------;;

(define-prefix-command 'mtg-mode-map nil "🍵 Mtg")

;;----------------------------------------------;;
;; Menu ----------------------------------------;;
;;----------------------------------------------;;

(easy-menu-define mtg-mode-menu mtg-mode-map

  "Menu for Mtg Mode."

  `("Mtg"

    ["Customize"          (customize-group 'mtg)]
    "---"
    ["Indent line"        indent-according-to-mode]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ,(if (default-boundp 'eldoc-documentation-function)
         ["Doc mode" eldoc-mode
          :style toggle :selected (bound-and-true-p eldoc-mode)]
       ["Doc mode" mtg-doc-mode
        :style toggle :selected (and (boundp 'mtg-doc-mode) mtg-doc-mode)])
    "---"
    ))

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

(define-derived-mode mtg-mode prog-mode "Mtg"

  "Major mode for editing Mtg files.

Mtg is a (lightweight) records-based expression language. 
From the language's homepage at URL `https://github.com/willtim/Mtg#readme':

“Mtg is a minimal statically-typed functional programming
 language, designed with embedding and/or extensibility in mind.
 Possible use cases for such a minimal language include configuration
 (à la Nix), data exchange (à la JSON) or even a starting point for a
 custom external DSL.”

========================================
= Configuration ========================
========================================

Examaple `use-package' declaration:

    (use-package mtg-mode
    
      :diminish (mtg-mode \" ☕\")
    
      :custom
    
      (mtg- t \"Enable \")
    
      :custom-face
    
      (mtg-keyword-face ((t (:weight bold :slant italic :underline t))) \"Keywords are bolded and italicized and underlined (but uncolored).\")
    
      :config
    
      ())

========================================
= Tutorial =============================
========================================



========================================
= Minor Modes ==========================
========================================

Indentation is provided by `mtg-indent-mode' (TODO!).

REPLs are provided by `interactive-mtg-mode' (TODO!).

Register other minor modes via `mtg-mode-hook'.

========================================
= Keymaps ==============================
========================================

(all bindings in this docstring are relative to `mtg-mode-map'.)\\<mtg-mode-map>

========================================
= Versions =============================
========================================

Call `mtg-version' to get the version of the currently-loaded Mtg Mode.

Call `mtg-program-version' to get the version of the currently-registered command `mtg'
(i.e. on the environment-variable `$PATH' or `%PATH%').

========================================
= Links ================================
========================================

• URL `https://github.com/willtim/Mtg#readme'

========================================"

  :group 'mtg

  :syntax-table mtg-mode-syntax-table

  (progn

    ;; Font Lock:

    (setq-local font-lock-defaults mtg-font-lock-defaults)
    (setq-local syntax-propertize-function #'mtg-syntax-propertize)

    ;; Comments:

    (setq-local comment-start      mtg-comment-start)
    (setq-local comment-padding    mtg-comment-padding)
    (setq-local comment-start-skip mtg-comment-start-skip)
    (setq-local comment-end        mtg-comment-end)
    (setq-local comment-end-skip   mtg-comment-end-skip)

    (setq-local comment-column mtg-comment-column)

    ;; Paragraphs:

    (setq-local paragraph-start    mtg-paragraph-start)
    (setq-local paragraph-separate mtg-paragraph-separate)

    (setq-local fill-paragraph-function #'mtg-fill-paragraph)
    (setq-local adaptive-fill-function  #'mtg-adaptive-fill)

    ;; Movement:

    (setq-local forward-sexp-function #'mtg-forward-sexp)
    (setq-local parse-sexp-ignore-comments nil)

    ;; Indentation:

    (setq-local indent-line-function #'mtg-indent-line)

    (setq-local basic-offset mtg-basic-offset)
    (setq-local indent-tabs-mode nil)
    (setq-local comment-auto-fill-only-comments t)

    ;; (when (boundp 'electric-indent-inhibit)
    ;;   (setq electric-indent-inhibit t))

    ;; ElDoc:

    (add-function :before-until (local 'eldoc-documentation-function) #'mtg-doc-current-info)

    ;; IMenu:

    (setq-local imenu-create-index-function #'mtg-ds-create-imenu-index)

    ;; Effects:

    (font-lock-fontify-buffer)
    (eldoc-mode +1)

    #'mtg-mode))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun mtg-version ()

  "Returns the (currently-loaded) version of `mtg-mode'.

Output:

• a `listp' of `numberp's."

  (interactive)

  (let ((ECHO-VERSION? (called-interactively-p 'any))
        )

    (pkg-info-package-version 'mtg-mode ECHO-VERSION?)))

;;----------------------------------------------;;

(defun mtg-program-version ()

  "Returns the (currently-loaded) version of `mtg-mode'.

Output:

• a `stringp'."

  (interactive)

  (let ((ECHO-VERSION?   (called-interactively-p 'any))
        (PROGRAM-VERSION (mtg-program-execute "--numeric-version"))
        )

   (when ECHO-VERSION?
     (message "« mtg » program version: %s" ROGRAM-VERSION))

   PROGRAM-VERSION))

;;----------------------------------------------;;

(defun mtg-mode-help ()

  "Open a (Help Buffer) tutorial for `mtg-mode'."

  (interactive)

  (describe-function #'mtg-mode))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun mtg-setup ()

  "Setup Mtg when Requiring (or Autoloading) « mtg-mode.el ».

”Setup“ includes:

• Registering `mtg-mode' with `auto-mode-alist'.
• Registering `mtg-mode' with `interpreter-mode-alist'.

Related:

• Gated by `mtg-setup-p'.
• Inverted by `mtg-unload-function'."

  (progn

    (add-to-list 'auto-mode-alist        (cons mtg-filepath-regexp #'mtg-mode))
    (add-to-list 'interpreter-mode-alist (cons "mtg" #'mtg-mode))

    ()))

;;----------------------------------------------;;
;;; Utilities ----------------------------------;;
;;----------------------------------------------;;

(defun mtg/regexp-opt (strings)

  "Return a regular expression matching anything in STRINGS.

Inputs:

• STRINGS — a `listp' of `stringp's.

Output:

• a `regexp'.
  Matches a syntactic symbol (see Info Node `(emacs) ') which is in STRINGS.

Examples:

• M-: (mtg/regexp-opt '(\"abc\" \"123\"))
      \"\\_<\\(123\\|abc\\)\\_>\"

Notes:

• Boundaries are respected.
  i.e. the output doesn't match substrings
  within a word or symbol, only the entire string.

Related:

• Calls `regexp-opt'"

  (let* ((STRINGS (identity strings))
         )
    (regexp-opt STRINGS 'symbols)))

;; ^ e.g.:
;;
;; • M-: (mtg/regexp-opt '("def" "123"))
;;     → "\\_<\\(123\\|def\\)\\_>"
;;
;; • M-: (if (string-match-p (mtg/regexp-opt '("def" "123")) "def") t nil)
;;     → t
;; • M-: (if (string-match-p (mtg/regexp-opt '("def" "123")) "abcdef") t nil)
;;     → nil
;; • M-: (if (string-match-p (mtg/regexp-opt '("def" "123")) "defghi") t nil)
;;     → nil
;;
;; 

;;----------------------------------------------;;

(defun mtg/fontify-text (text regexp &rest faces)

  "Fontify any substrings in TEXT matching REGEXP with FACES.

Inputs:

• TEXT   — a `stringp'.
• REGEXP — a `regexpp'.
• FACES  — a `listp' of `symbolp's (representing `facep's).

Output:

• a `stringp'.
  Same characters as TEXT,
  but with extra Text Properties."

  (when text
    (-each
        (s-matched-positions-all regexp text)
      (-lambda ((beg . end))
        (/each faces
          (add-face-text-property beg end it nil text))))))

;;----------------------------------------------;;

(defun mtg-eldoc/fontify-text (text)

  "Fontify TEXT for Eldoc.

Inputs:

• TEXT — a `stringp'.

Output:

• a `stringp'.
  Same characters as TEXT,
  but with extra Text Properties."

  (let ((REGEXP-KWD
         (rx string-start (1+ (not (any space ":"))) ":"))
        (REGEXP-KWARGS
         (rx symbol-start "&" (1+ word)))
        (REGEXP-QUOTED-ARGS
         (rx "`" (1+ (not space)) "`"))
        )

    (mtg/fontify-text
     text REGEXP-KWD 'font-lock-keyword-face)

    (mtg/fontify-text
     text REGEXP-KWARGS 'font-lock-type-face)

    (mtg/fontify-text
     text REGEXP-QUOTED-ARGS 'font-lock-constant-face 'bold-italic)

    text))
 
;;----------------------------------------------;;
;; Unloading -----------------------------------;;
;;----------------------------------------------;;

(defun mtg-unload-function ()

  "`unload-feature' for `mtg'.

Inverts `mtg-setup'
(which gets executed by « (load \"mtg.el\") »).

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

(mtg-setup)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `create-image':
;;
;; M-: (insert-image (create-image "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M85.332 59.918h-36.004l13.185-9.383c-4.898-3.887-10.566-5.828-16.984-5.828-3.211 0-5.414.613-6.59 1.836-1.184 1.227-1.777 3.445-1.777 6.654 0 8.873 4.563 18.34 13.691 28.396l-10.391 10.521c-12.09-14.705-18.129-27.844-18.129-39.424 0-6.928 2.086-12.447 6.27-16.545 4.18-4.098 9.746-6.148 16.668-6.148 8.453 0 17.664 3.215 27.641 9.635l7.728-13.182 4.692 33.468z' fill='#0D0F0F'/></g></svg>" 'svg t))
;;
;; M-: (insert-image (create-image (base64-decode-string "PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHZpZXdC b3g9JzAgMCAxMDAgMTAwJz48ZyB0cmFuc2Zvcm09J3RyYW5zbGF0ZSgwIC0x KScgZmlsbD0nbm9uZSc+PGNpcmNsZSBmaWxsPScjQ0FDNUMwJyBjeD0nNTAn IGN5PSc1MC45OTgnIHI9JzUwJy8+PHBhdGggZD0nTTg1LjMzMiA1OS45MTho LTM2LjAwNGwxMy4xODUtOS4zODNjLTQuODk4LTMuODg3LTEwLjU2Ni01Ljgy OC0xNi45ODQtNS44MjgtMy4yMTEgMC01LjQxNC42MTMtNi41OSAxLjgzNi0x LjE4NCAxLjIyNy0xLjc3NyAzLjQ0NS0xLjc3NyA2LjY1NCAwIDguODczIDQu NTYzIDE4LjM0IDEzLjY5MSAyOC4zOTZsLTEwLjM5MSAxMC41MjFjLTEyLjA5 LTE0LjcwNS0xOC4xMjktMjcuODQ0LTE4LjEyOS0zOS40MjQgMC02LjkyOCAy LjA4Ni0xMi40NDcgNi4yNy0xNi41NDUgNC4xOC00LjA5OCA5Ljc0Ni02LjE0 OCAxNi42NjgtNi4xNDggOC40NTMgMCAxNy42NjQgMy4yMTUgMjcuNjQxIDku NjM1bDcuNzI4LTEzLjE4MiA0LjY5MiAzMy40Njh6JyBmaWxsPScjMEQwRjBG Jy8+PC9nPjwvc3ZnPgo= ") 'svg t))
;;

;; `base64-decode-string':
;;
;; M-: (base64-decode-string "PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHZpZXdC b3g9JzAgMCAxMDAgMTAwJz48ZyB0cmFuc2Zvcm09J3RyYW5zbGF0ZSgwIC0x KScgZmlsbD0nbm9uZSc+PGNpcmNsZSBmaWxsPScjQ0FDNUMwJyBjeD0nNTAn IGN5PSc1MC45OTgnIHI9JzUwJy8+PHBhdGggZD0nTTg1LjMzMiA1OS45MTho LTM2LjAwNGwxMy4xODUtOS4zODNjLTQuODk4LTMuODg3LTEwLjU2Ni01Ljgy OC0xNi45ODQtNS44MjgtMy4yMTEgMC01LjQxNC42MTMtNi41OSAxLjgzNi0x LjE4NCAxLjIyNy0xLjc3NyAzLjQ0NS0xLjc3NyA2LjY1NCAwIDguODczIDQu NTYzIDE4LjM0IDEzLjY5MSAyOC4zOTZsLTEwLjM5MSAxMC41MjFjLTEyLjA5 LTE0LjcwNS0xOC4xMjktMjcuODQ0LTE4LjEyOS0zOS40MjQgMC02LjkyOCAy LjA4Ni0xMi40NDcgNi4yNy0xNi41NDUgNC4xOC00LjA5OCA5Ljc0Ni02LjE0 OCAxNi42NjgtNi4xNDggOC40NTMgMCAxNy42NjQgMy4yMTUgMjcuNjQxIDku NjM1bDcuNzI4LTEzLjE4MiA0LjY5MiAzMy40Njh6JyBmaWxsPScjMEQwRjBG Jy8+PC9nPjwvc3ZnPgo= ")
;;

;;----------------------------------------------;;

;; CSS... 
;;
;; Embedded SVG Data. e.g the Tap-Symbol:
;;
;;     .card-symbol-T{background-image:url("data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHZpZXdC b3g9JzAgMCAxMDAgMTAwJz48ZyB0cmFuc2Zvcm09J3RyYW5zbGF0ZSgwIC0x KScgZmlsbD0nbm9uZSc+PGNpcmNsZSBmaWxsPScjQ0FDNUMwJyBjeD0nNTAn IGN5PSc1MC45OTgnIHI9JzUwJy8+PHBhdGggZD0nTTg1LjMzMiA1OS45MTho LTM2LjAwNGwxMy4xODUtOS4zODNjLTQuODk4LTMuODg3LTEwLjU2Ni01Ljgy OC0xNi45ODQtNS44MjgtMy4yMTEgMC01LjQxNC42MTMtNi41OSAxLjgzNi0x LjE4NCAxLjIyNy0xLjc3NyAzLjQ0NS0xLjc3NyA2LjY1NCAwIDguODczIDQu NTYzIDE4LjM0IDEzLjY5MSAyOC4zOTZsLTEwLjM5MSAxMC41MjFjLTEyLjA5 LTE0LjcwNS0xOC4xMjktMjcuODQ0LTE4LjEyOS0zOS40MjQgMC02LjkyOCAy LjA4Ni0xMi40NDcgNi4yNy0xNi41NDUgNC4xOC00LjA5OCA5Ljc0Ni02LjE0 OCAxNi42NjgtNi4xNDggOC40NTMgMCAxNy42NjQgMy4yMTUgMjcuNjQxIDku NjM1bDcuNzI4LTEzLjE4MiA0LjY5MiAzMy40Njh6JyBmaWxsPScjMEQwRjBG Jy8+PC9nPjwvc3ZnPgo= ")}
;;

;; HTML... 
;;
;; Embedded SVG Data. e.g the Tap-Symbol:
;;
;;     <img src="data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHZpZXdC b3g9JzAgMCAxMDAgMTAwJz48ZyB0cmFuc2Zvcm09J3RyYW5zbGF0ZSgwIC0x KScgZmlsbD0nbm9uZSc+PGNpcmNsZSBmaWxsPScjQ0FDNUMwJyBjeD0nNTAn IGN5PSc1MC45OTgnIHI9JzUwJy8+PHBhdGggZD0nTTg1LjMzMiA1OS45MTho LTM2LjAwNGwxMy4xODUtOS4zODNjLTQuODk4LTMuODg3LTEwLjU2Ni01Ljgy OC0xNi45ODQtNS44MjgtMy4yMTEgMC01LjQxNC42MTMtNi41OSAxLjgzNi0x LjE4NCAxLjIyNy0xLjc3NyAzLjQ0NS0xLjc3NyA2LjY1NCAwIDguODczIDQu NTYzIDE4LjM0IDEzLjY5MSAyOC4zOTZsLTEwLjM5MSAxMC41MjFjLTEyLjA5 LTE0LjcwNS0xOC4xMjktMjcuODQ0LTE4LjEyOS0zOS40MjQgMC02LjkyOCAy LjA4Ni0xMi40NDcgNi4yNy0xNi41NDUgNC4xOC00LjA5OCA5Ljc0Ni02LjE0 OCAxNi42NjgtNi4xNDggOC40NTMgMCAxNy42NjQgMy4yMTUgMjcuNjQxIDku NjM1bDcuNzI4LTEzLjE4MiA0LjY5MiAzMy40Njh6JyBmaWxsPScjMEQwRjBG Jy8+PC9nPjwvc3ZnPgo= "/>
;;

;; SVG... 
;;
;; e.g the Tap-Symbol:
;;
;;     <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M85.332 59.918h-36.004l13.185-9.383c-4.898-3.887-10.566-5.828-16.984-5.828-3.211 0-5.414.613-6.59 1.836-1.184 1.227-1.777 3.445-1.777 6.654 0 8.873 4.563 18.34 13.691 28.396l-10.391 10.521c-12.09-14.705-18.129-27.844-18.129-39.424 0-6.928 2.086-12.447 6.27-16.545 4.18-4.098 9.746-6.148 16.668-6.148 8.453 0 17.664 3.215 27.641 9.635l7.728-13.182 4.692 33.468z' fill='#0D0F0F'/></g></svg>
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg-mode)

;;; mtg-mode.el ends here