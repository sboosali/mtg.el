;;; mtg.el --- Magic The Gathering custom card editor -*- coding: utf-8; lexical-binding: t -*-

;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (seq "2.16"))
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

;; Editor for “Magic: The Gathering”.
;;
;; Contents include:
;;
;; • ‘mtg-mode-*’     — the `major-mode'.
;; • ‘mtg-insert-*’   — Insert template ‘skeleton’s (e.g. « M-x `mtg-insert-*' »).
;; • ‘mtg-abbrev-*’   — Expand abbreviations (e.g. « etb<TAB> » → « enters the battlefield, »).
;; • ‘mtg-*-face’     — `facep's for `font-lock'.
;; • ‘mtg-’           — 
;; 
;; Features include:
;;
;; ① Completion.
;;
;;    Complete these groups of words/phrases:
;; 
;;        • Card Names  — There are ~20,000 card names.
;;        • Keywords    — i.e. Keyword Abilities / Keyword Actions / Ability Words.
;;        • Types       — i.e. Card Types / Sub Types / Super Types.
;;        • Editions    — i.e. Set Codes / Set Names.
;;
;; ② Formatting.
;; 
;;    Formats:
;; 
;;        • Capitalization — 
;;        • Keywords       — i.e. Keyword Abilities / Keyword Actions / Ability Words.
;;        • Types          — i.e. Card Types / Sub Types / Super Types.
;;        • Editions       — i.e. Set Codes / Set Names.
;;
;; ③ Skeletons. 
;;
;;        • Cycles  — e.g. by Color, by Rarity.
;;        • Phrases — e.g. typing “etb ” (i.e. e-t-b-SPC) automatically expands to “enters the battlefield ”.
;;
;; ④ Linting.
;; 
;;    Checks for:
;; 
;;        • Pitfalls — e.g. « The rules text “if ~ would die” isn't valid; instead, write “if ~ would be put into a graveyard from anywhere” or “when ~ dies” ».
;;        •  — 
;;        •  — 
;;        •  — 
;;
;; ⑤ .
;;
;;    for:
;; 
;;        •  — 
;;        •  — 
;;        •  — 
;;
;; ⑨ Export.
;;
;;   Export your custom set as:
;;
;;     • [✓] MTGJSON          — as a ‹.json› file (a.k.a. a JSON Object), with the schema.
;;     • [✓] Magic Set Editor — as a ‹.tar› file (a.k.a. a Tar Archive), with the ‹.mse-set› schema.
;;     • [❌] /r/custommagic   — as a ‹.md› file (a.k.a. Markdown), in Reddit-flavored Markdown, with the Subreddit-specific pseudo-links.
;;     • [❌] MTG Salvation    — as a ‹.bb› file (a.k.a. BBCode). 
;;
;;    Render your custom as:
;;
;;     • [✓] a webpage — as a ‹.html› file (a.k.a. a Web Page), that's beautifully styled and completely standalone.
;;
;;         • HTML Styling    — via SVG mana symbols, the “Belern” font, and appropriate Bold/Italic (e.g. italics for flavor/remdinder text).
;;         • Standalone HTML — all assets (e.g. artwork PNGs, CSS symbols) are embedded (via “Data URIs” and inline <style>s).
;;           Thus, no 
;;
;;     • [❌] a document — as a ‹.pdf› file (?),
;;
;;
;;

;;; Code:

;;----------------------------------------------;;
;;; Imports ------------------------------------;;
;;----------------------------------------------;;

;; builtin requirements:

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

;;==============================================;;

;; project requirements:

(progn
;;TODO  (require 'mtg)
  (require 'mtg-image nil :no-error))

;;----------------------------------------------;;
;;; Constants ----------------------------------;;
;;----------------------------------------------;;

(defconst mtg-file-regexp

  (rx ".mtg" eos)

  "Match a filename whose contents should be MTG cards.

a `regexpp's

(Conforms to `auto-mode-alist'.)")

;;----------------------------------------------;;

(defconst mtg-block-separator-regexp

  (rx "\n" (0+ (any " \t\n\f")) "\n")  ; "\n[\n\t\f ]*\n"

  "Regular expression for matching block boundaries.")

;;----------------------------------------------;;
;;; Groups -------------------------------------;;
;;----------------------------------------------;;

(defgroup mtg-mode nil

  "“Magic: The Gathering” Search Engine and (Custom-)Card Editor."

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/mtg.el")

  :prefix "mtg-"
  :group 'mtg)

;;----------------------------------------------;;
;;; Macros: `rx' -------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  ;;--------------------------;;

  (defconst mtg-rx-constituents

    `((block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with")
                                   symbol-end))
      (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start (or "def" "class") symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''"))))))

    "Additional Mtg specific sexps for `mtg-rx'")

  ;;--------------------------;;

  (defmacro mtg-rx (&rest regexps)

    "`rx' extended with `mtg-mode' regexps.

This variant of `rx' supports common mtg named REGEXPS."

    ;; (declare (indent 1))    

    (let ((rx-constituents (append mtg-rx-constituents rx-constituents))
          )

      (cond ((null regexps)
             (error "No regexp"))

            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))

            (t
             (rx-to-string (car regexps) t))))))

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
;;; Variables ----------------------------------;;
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
;; Accessors: `listp's -------------------------;;
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
;; Accessors: `regexpp's -----------------------;;
;;----------------------------------------------;;

(defconst mtg-quotation-regexp

  (rx (or ?“ ?\") (not (or ?“ ?” ?\")) (or ?” ?\"))

  "Match 

a `regexpp'.

Examples:

• Llanowar Mentor includes the following sentence in its rules text —
  « Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.” ».")

;;----------------------------------------------;;

(defconst mtg-card-namesake-regexp

  (rx (or (and word-start "~" word-end))
          (and word-start (or ?t ?T) "his creature" word-end))

  "Match namesakes of the current card.

a `regexpp'.")

;;----------------------------------------------;;

(defconst mtg-card-name-reference-regexp

  (mtg-rx word-start "named" word-end
          mtg-card-name)

  "Match-Maximally a reference to a known card name.

a `regexpp'.")

;;----------------------------------------------;;

(defconst mtg-symbol-regexp

  (rx "{" (group-n 1 (1+ (any "-" "@#$_&+/*:;!?" alnum))) "}")

  "Match 

a `regexpp'.

See URL `https://mtg.gamepedia.com/Numbers_and_symbol'.")

;;----------------------------------------------;;

(defconst mtg-color-white-regexp

  (rx word-start
      (or (and (any "wW") "hite")
          (and "{" (any "wW") "}")
          "Plains")
      word-end)

  "Match a word/symbol associated with the color white.

a `regexpp'.")

;;----------------------------------------------;;

(defconst mtg-color-blue-regexp

  (rx word-start
      (or (and (any "Uu") "blue")
          (and "{" (any  "Uu") "}")
          "Island")
      word-end)

  "Match a word/symbol associated with the color ue.

a `regexpp'.")

;;----------------------------------------------;;

(defconst mtg-color-black-regexp

  (rx word-start
      (or (and (any "Bb") "black")
          (and "{" (any  "Bb") "}")
          "Swamp")
      word-end)

  "Match a word/symbol associated with the color black.

a `regexpp'.")

;;----------------------------------------------;;

(defconst mtg-color-red-regexp

  (rx word-start
      (or (and (any "Rr") "red")
          (and "{" (any  "Rr") "}")
          "Mountain")
      word-end)

  "Match a word/symbol associated with the color red.

a `regexpp'.")

;;----------------------------------------------;;

(defconst mtg-color-green-regexp

  (rx word-start
      (or (and (any "Gg") "green")
          (and "{" (any  "Gg") "}")
          "Forest")
      word-end)

  "Match a word/symbol associated with the color green.

a `regexpp'.")

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

;;==============================================;;
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

;;==============================================;;
;; Accessors:

(defsubst mtg-syntax-within-comment-p (&optional position)

  "Return non-nil if POSITION is within a comment.

Inputs:

• POSITION — a `number-or-marker-p'.
  Defaults to `point'."

  (nth 4 (syntax-ppss position)))

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

(defcustom mtg-comment-column 0

  "`comment-columnt' for `mtg-mode'."

  :type '(integer :tag "Column")
  :safe #'integerp
  :group 'mtg)

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
;;; Faces --------------------------------------;;
;;----------------------------------------------;;

(defgroup mtg-faces nil

  "Fonts and colors for Mtg Mode.

Customize the appearence of `mtg-mode'."

  :prefix "mtg-"
  :group  'mtg)

;;==============================================;;

(defface mtg-card '((t))
  "Base face for MTG Cards."
  :group 'mtg-faces)

;; ^ face ‘mtg-card’, being a Base-Face, specifies no Face-Attributes (in particularm ‘:inherit’s no faces);
;;   this prevents shadowing the ‘font-lock-*-face’s (e.g. in face ‘mtg-card-name’),
;;   unless explicitly/intentionally specified.
;;

;;----------------------------------------------;;

(defface mtg-card-name '((t :underline t :inherit (mtg-card font-lock-variable-name-face)))
  "Face for MTG Card Names."
  :group 'mtg-faces)

;;----------------------------------------------;;

(progn

  (defface mtg-color-white '((t :foreground white :inherit mtg-card))
    "Face for white Mana Symbols and Plains."
    :group 'mtg-faces)

  (put 'white 'mtg-face 'mtg-color-white))

;;----------------------------------------------;;

(progn

  (defface mtg-color-blue '((t :foreground blue :inherit mtg-card))
    "Face for blue Mana Symbols and Islands"
    :group 'mtg-faces)

  (put 'blue 'mtg-face 'mtg-color-blue))

;;----------------------------------------------;;

(progn

  (defface mtg-color-black '((t :foreground black :inherit mtg-card))
    "Face for black Mana Symbols and Swamps."
    :group 'mtg-faces)

  (put 'black 'mtg-face 'mtg-color-black))

;;----------------------------------------------;;

(progn

  (defface mtg-color-red '((t :foreground red :inherit mtg-card))
    "Face for red Mana Symbols and Mountains."
    :group 'mtg-faces)

  (put 'red 'mtg-face 'mtg-color-red))

;;----------------------------------------------;;

(progn

  (defface mtg-color-green '((t :foreground green :inherit mtg-card))
    "Face for green Mana Symbols and Forests."
    :group 'mtg-faces)

  (put 'green 'mtg-face 'mtg-color-green))

;;----------------------------------------------;;

(defface mtg-rules-text '((t :inherit mtg-card))
  "Face for MTG Rules Text."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-rules-keyword '((t :weight bold :inherit mtg-rules-text))
  "Face for keywords within MTG Rules Text."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-flavor-text '((t :slant italic :inherit mtg-card))
  "Face for MTG Flavor Text."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-typeline '((t :slant italic :inherit (mtg-card font-lock-type-face)))
  "Base face for MTG Card/Sub/Super Types."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-card-type '((t :inherit (mtg-typeline font-lock-builtin-face)))
  "Base face for MTG Card Types."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-sub-type '((t :inherit mtg-typeline))
  "Base face for MTG Card Subtypes."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-super-type '((t :inherit mtg-typeline))
  "Base face for MTG Card Supertypes."
  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-comment '((t :inherit font-lock-comment-face))

  "Face for comments (beneath MTG Cards)."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-string '((t :inherit (mtg-card font-lock-string-face)))

  "Face for strings (within MTG Cards)."

  :group 'mtg-faces)

;;----------------------------------------------;;

(defface mtg-preprocessor '((t :inherit (mtg-card font-lock-preprocessor-face)))

  "Face for preprocessor directives (within MTG Cards)."

  :group 'mtg-faces)

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

(defun mtg-toggle-inline-images (&optional force)

  "Display Image URIs inline as Images.

Inputs:

 • FORCE — an optional `numberp'.
  the Prefix-Argument.
  Values:
      ° positive — enable.
      ° `zerop' or `nil' — toggle. the default.
      ° negative — disable.

Effects:

• Toggles `iimage-mode'."

  (interactive "P")

  (let* ((FORCE (cond
                 ((or (not force) (zerop force)) 'toggle)
                 ((> force 0) 'enable)
                 ((< force 0) 'disable)))
         )

    (save-excursion

      (goto-point (point-min))

      (pcase FORCE
        ('toggle (iimage-mode))
        ('enable (turn-on-iimage-mode))
        ('disable (turn-off-iimage-mode))
        (_ nil))

      ())))

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

(defconst mtg-font-lock-keywords

  (list mtg-font-lock-keywords-keyword
        
        )

  "`font-lock-keywords' for `mtg-mode'.

a `listp' associating `regexpp's with `facep's.

(For “Search-based Fontification”,
a.k.a. “Keyword-based Syntax-Highlighting”).")

;;----------------------------------------------;;

(defconst mtg-font-lock-defaults

  (let ((mtg-font-lock-keywords-only             t)   ; Search-Based Fontification
        (mtg-font-lock-keywords-case-fold-search nil) ; Case-Insensitive
        )
    (list 'mtg-font-lock-keywords mtg-font-lock-keywords-only mtg-font-lock-keywords-case-fold-search))

  "`font-lock-defaults' for `mtg-mode'.")

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

(defun mtg-eldoc (&optional symbol)

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

• M-:  (substring-no-properties (mtg-eldoc \"map\"))
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
;;   M-: (mtg-eldoc "map")
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

(define-prefix-command 'mtg-mode-map nil "🂠 MTG")

;;----------------------------------------------;;

(defvar markdown-mode-mouse-map

  (let ((KEYMAP (make-sparse-keymap)))

    (define-key KEYMAP [follow-link] #'mouse-face)
    (define-key KEYMAP [mouse-2]     #'markdown-follow-link-at-point)

    KEYMAP)

  "Keymap for following links with mouse.")

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
;;; IMenu --------------------------------------;;
;;----------------------------------------------;;

(defun mtg-imenu-create-index ()

  "`imenu-create-index-function' for `mtg-mode'.

Output:

• a “Simple Index AList”:
  a `listp' of `consp's, with `stringp' ‘car’s and `number-or-marker-p' ‘cdr’s."

  ())

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(define-derived-mode mtg-mode text-mode "MTG Cards"

  "Major mode for editing Magic cards.

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

    (setq-local comment-end      mtg-comment-end)
    (setq-local comment-end-skip mtg-comment-end-skip)

    (setq-local comment-column mtg-comment-column)

    (setq-local comment-use-syntax t)

    ;; Movement:

    (setq-local forward-sexp-function #'mtg-forward-sexp)
    (setq-local parse-sexp-ignore-comments nil)

    (setq-local beginning-of-defun-function #'mtg-beginning-of-defun)
    (setq-local end-of-defun-function       #'mtg-end-of-defun)

    ;; Indentation:

    (setq-local indent-line-function #'mtg-indent-line)

    (setq-local basic-offset mtg-basic-offset)
    (setq-local indent-tabs-mode nil)
    (setq-local comment-auto-fill-only-comments t)

    ;; (when (boundp 'electric-indent-inhibit)
    ;;   (setq electric-indent-inhibit t))

    ;; Filling:

    (setq-local paragraph-start    mtg-paragraph-start)
    (setq-local paragraph-separate mtg-paragraph-separate)

    (setq-local fill-paragraph-function #'mtg-fill-paragraph)
    (setq-local adaptive-fill-function  #'mtg-adaptive-fill)

    ;; ElDoc

    (if (eval-when-compile (fboundp #'add-function))
        (add-function :before-until (local 'eldoc-documentation-function) #'mtg-eldoc)
      (setq-local eldoc-documentation-function #'mtg-eldoc))

    (setq-local eldoc-argument-case #'mtg-eldoc/argument-case)

    ;; Outline Mode:

    (setq-local outline-regexp mtg-regex-header)
    (setq-local outline-level #'mtg-outline-level)

    ;; IMenu:

    (setq-local imenu-create-index-function #'mtg-imenu-create-index)

    ;; Menu Bar (for XEmacs):

    (easy-menu-add mtg-mode-menu mtg-mode-map)

    ;; Flyspell:

    ;;(setq-local flyspell-generic-check-word-predicate #'mtg-flyspell-check-word-p)

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
     (message "« mtg » program version: %s" PROGRAM-VERSION))

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
;; Utilities -----------------------------------;;
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