________________




;;==============================================;;


COMMENTARY


;;==============================================;;


;; Completion:
;;
;; • ‘mtg-read-color’
;; • ‘mtg-read-rarity’
;; • ‘mtg-read-language’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; • ‘mtg-read-’
;; 


;; ‘mtg-*-text’:
;;
;; • ‘mtg-abbreviate-text’ — 
;;   Replace substrings with abbreviations. The substrings are mostly words and phrases. The abbreviations are shorter, but still alphanumeric, words. e.g. (mtg-abbreviate-text "converted mana cost") evaluates to "cmc".
;;
;; • ‘mtg-symbolize-text’ —
;;   Replace substrings with symbols. The symbols are often a single unicode character. e.g. (mtg-symbolize-text "{T}") evaluates to "Ⓣ".
;;
;; • ‘mtg-iconize-text’ —
;;   Replace substrings with images, or insert images alongside substrings in a buffer. Such images can be how symbols actually look.
;;
;; • ‘mtg-truncate-text’ —
;;   Truncate text .
;;   Wraps ‘mtg-symbolize-text’ & ‘mtg-abbreviate-text’.
;;
;;


;; in Flavor Text, the Attribution (following a long-hyphen, "—", meaning "by"), is Right-Justified (for Left-to-Right languages).
;; (TODO: how do I justify text in emacs, for display only?)
;;
;;e.g. ‹Envelop›


;; ‘mtg-query’…
;;
;; (1) ‹,› vs ‹;›
;;
;;      ‹;› is disjunction (“ring addition”), in general.
;;
;;      ‹,› is conjunction (“ring  multiplication”), in general.
;;
;;     e.g. « *artifact,!creature;!artifact,creature »
;;     ≈ « *artifact,creature,!(artifact,creature) »
;;     matches “either Noncreature Artifacts or Nonartifact Creatures”,
;;     a.k.a. “all Artifacts and Creature, except Artifact Creatures”. 
;;




;;==============================================;;




























REQUIREMENTS


;;==============================================;;


(require 'cl-generic)


(require 'format)


(require 'let-alist)


(require ')


;;


(require 'button)


(require 'browse-url)


(require 'electric)


;;(require 'elec-pair)


(require ')


;;==============================================;;










































;;; CONSTANTS:


;;==============================================;;


(defconst mtg-vertical-whitespace-regexp


  (rx (any ?\u000a ?\u000b ?\u000c ?\u000d
           ?\u0085 ?\u2028 ?\u2029))


  "Regexp that matches any Vertical-Whitespace character.


The ASCII Vertical-Whitespace characters are:


• U+000A — “LINE FEED (LF)”;
a.k.a. “\\n” a.k.a. “EOL” a.k.a. “Line-Break” a.k.a “^J”.


• U+000B — “VERTICAL TAB (VT)”;
a.k.a. “\\v” a.k.a. “LINE TABULATION” a.k.a “^K”.


• U+000C — “FORM FEED (FF)”;
a.k.a. “\\f” a.k.a. “Page-Break” a.k.a “^L”.


• U+000D — “CARRIAGE RETURN (CR)”;
a.k.a. “\\r” a.k.a “^M”.


The Unicode Vertical-Whitespace characters are:


• U+0085 — “NEXT LINE (NEL)”.


• U+2028 — “LINE SEPARATOR (LS)”.


• U+2029 — “PARAGRAPH SEPARATOR (PS)”.


Links:


• URL ‘https://en.m.wikipedia.org/wiki/Newline#Representation’")


;;==============================================;;
























































;;; MACROS:


(define-inline mtg- (object)


  " OBJECT."


  (inline-letevals ((OBJECT object))
    (inline-quote
      ( ,OBJECT))))


;;==============================================;;


(define-inline mtg--p (object)


  "Return non-nil if OBJECT is a ."


  (inline-letevals ((OBJECT object))
    (inline-quote
      (eq ' (car-safe ,OBJECT)))))


;;----------------------------------------------;;


(define-inline mtg- (object)


  " OBJECT."


  (inline-letevals ((OBJECT object))
    (inline-quote
      ( ,OBJECT))))


(define-inline mtg-assert-textual (object &optional message)


  "Assert that OBJECT has a “textual” type.


Check that OBJECT is a ‘symbolp’ (including nil) or ‘stringp’.
Otherwise, error with symbol ‘wrong-type-argument’.


(Wraps function ‘cl-check-type’, which see.)"


  (inline-letevals ((OBJECT object) (MESSAGE message))
    (inline-quote
      (cl-check-type ,OBJECT '(or symbol string) ,MESSAGE))))


;;----------------------------------------------;;


(define-inline mtg-assert-numeral (object &optional message)


  "Assert that OBJECT has a “numeral” type.


Check that OBJECT is an ‘integerp’ or ‘symbolp’ (including nil).
Otherwise, error with symbol ‘wrong-type-argument’.


(Wraps function ‘cl-check-type’, which see.)"


  (inline-letevals ((OBJECT object) (MESSAGE message))
    (inline-quote
      (cl-check-type ,OBJECT '(or integer symbol) ,MESSAGE))))


;;==============================================;;


(define-inline mtg-get-xyz (name &optional table)
  "Return the ‘mtg-xyz’ named NAME."


  (declare (side-effect-free t))


  (mtg-assert-textual name)


  (inline-letevals ((NAME  (mtg-intern-name name))
                    (TABLE (or table mtg-xyz-table)))


    (inline-quote
      (gethash ,NAME ,TABLE)))))


;;----------------------------------------------;;


(define-inline mtg-get-card (name &optional table)
  "Get ‘mtg-card’ named NAME."


  (declare (side-effect-free t))


  (mtg-assert-textual name)


  (inline-letevals ((NAME  (mtg-intern-card-name name))
                    (TABLE (or table mtg-card-table)))


    (inline-quote
      (gethash ,NAME ,TABLE))))


;;----------------------------------------------;;


(define-inline mtg-get-edition (name &optional table)
  "Return the ‘mtg-edition’ named NAME."


  (declare (side-effect-free t))


  (mtg-assert-textual name)


  (inline-letevals ((NAME  (mtg-intern-edition-name name))
                    (TABLE (or table mtg-edition-table)))


    (inline-quote
      (gethash ,NAME ,TABLE))))


;;----------------------------------------------;;


(define-inline mtg-get-keyword (name &optional table)
  "Return the ‘mtg-keyword’ named NAME."


  (declare (side-effect-free t))


  (mtg-assert-textual name)


  (inline-letevals ((NAME  (mtg-intern-keyword-name name))
                    (TABLE (or table mtg-keyword-table)))


    (inline-quote
      (gethash ,NAME ,TABLE))))


;;----------------------------------------------;;


(define-inline mtg-get-color (name &optional table)
  "Return the ‘mtg-color’ named NAME."


  (declare (side-effect-free t))


  (mtg-assert-textual name)


  (inline-letevals ((NAME  (mtg-intern-name name))
                    (TABLE (or table mtg-color-table)))


    (inline-quote
      (gethash ,NAME ,TABLE))))


;;==============================================;;


;; ‘define-mtg-*’:
;;
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;; • ‘define-mtg-’ — 
;;
;;
;;
;; (The ‘define-mtg-*’ are kinda like ‘define-abbrev-table’.)
;;
;; 


;;==============================================;;


(defmacro mtg-with-slots (struct-type slot-names object &rest body)


  "Bind SLOT-NAMES in OBJECT of ‘defstruct’ STRUCT-TYPE, and execute BODY.


If nil, SLOT-NAMES defaults to all slots of STRUCT-TYPE.


=== Examples ===


(mtg-with-slots point (x y) (make-point :x 1 : y 2 :z 3)
  (+ x y))
↪ 3


(mtg-with-slots point () (make-point :x 1 : y 2 :z 3)
  (+ x y z))
↪ 6


[TODO or...]


(mtg-with-slots (point x y) (make-point :x 1 : y 2 :z 3)
  (+ x y))
↪ 3


(mtg-with-slots point (make-point :x 1 : y 2 :z 3)
  (+ x y z))
↪ 6


\(fn STRUCT-TYPE SLOT-NAMES OBJECT BODY...)"


  (declare (indent 3)
           (debug (symbol sexp sexp body)))


  (cl-check-type struct-type 'symbol)
  (cl-check-type slot-names  'list)


  (cl-assert (recordp struct-type) "STRUCT-TYPE must be a defined ‘defstruct’.")


  (let* ((SLOT-NAMES
           (or slot-names
               (cl-loop for (SLOT-NAME) on (cdr (cl-struct-slot-info struct-type))
                        ;; TODO Default to only public slots (i.e. which don't start with a hyphen)? 
                        ;; unless (string-prefix-p "-" (symbol-name SLOT-NAME))
                        collect SLOT-NAME)))


         (SLOT-BINDINGS 
           (cl-loop for SLOT-NAME in SLOT-NAMES
                    with SLOT-VALUE = (cl-struct-slot-value struct-type SLOT-NAME object)
                    collect `(,SLOT-NAME ,SLOT-VALUE)))
           )


  `(cl-symbol-macrolet ,SLOT-BINDINGS
     ,@body))


;; example
(progn
  (defstruct testobj a b)
  (let ((obj (make-testobj :a 1 :b 2)))
    (mtg-with-slots testobj (a b)  obj
      (message "value: %s:"  (list a b))
      (setq a 3 b 4)
      (message "value: %s:"  (list (testobj-a obj) (testobj-b obj))))))


;;==============================================;;


(defmacro define-mtg-card


(defmacro define-mtg-printing


(defmacro define-mtg-edition


(defmacro define-mtg-keyword


(defmacro define-mtg-












(defmacro define-mtg-card (name &rest props)


  (declare (debug t)
(indent 1))


  (let ((PROPS `(:name ,name ,@props))


    (set-mtg-card name PROPS)))




(defun set-mtg-card (name &rest props)


  (








 (tablename definitions
                                      &optional docstring &rest props)
  "Define TABLENAME (a symbol) as an abbrev table name.
Define abbrevs in it according to DEFINITIONS, which is a list of elements
of the form (ABBREVNAME EXPANSION ...) that are passed to `define-abbrev'.
PROPS is a property list to apply to the table.
Properties with special meaning:
- `:parents' contains a list of abbrev tables from which this table inherits
  abbreviations.
- `:case-fixed' non-nil means that abbreviations are looked up without
  case-folding, and the expansion is not capitalized/upcased.
- `:regexp' is a regular expression that specifies how to extract the
  name of the abbrev before point.  The submatch 1 is treated
  as the potential name of an abbrev.  If :regexp is nil, the default
  behavior uses `backward-word' and `forward-word' to extract the name
  of the abbrev, which can therefore only be a single word.
- `:enable-function' can be set to a function of no argument which returns
  non-nil if and only if the abbrevs in this table should be used for this
  instance of `expand-abbrev'."


  ;; We used to manually add the docstring, but we also want to record this
  ;; location as the definition of the variable (in load-history), so we may
  ;; as well just use `defvar'.


  (when (and docstring props (symbolp docstring))
    ;; There is really no docstring, instead the docstring arg
    ;; is a property name.
    (push docstring props) (setq docstring nil))
  (eval `(defvar ,tablename nil ,@(if docstring (list docstring))))
  (let ((table (if (boundp tablename) (symbol-value tablename))))
    (unless table
      (setq table (make-abbrev-table))
      (set tablename table)
      (unless (memq tablename abbrev-table-name-list)
        (push tablename abbrev-table-name-list)))
    ;; We used to just pass them to `make-abbrev-table', but that fails
    ;; if the table was pre-existing as is the case if it was created by
    ;; loading the user's abbrev file.
    (while (consp props)
      (unless (cdr props) (error "Missing value for property %S" (car props)))
      (abbrev-table-put table (pop props) (pop props)))
    (dolist (elt definitions)
      (apply 'define-abbrev table elt))))




























































































ERRORS




mtg-error
error "%s" args




mtg-user-error
user-error "%s" args




(define-error 'mtg-error      nil)
(define-error 'mtg-user-error 'user-error)








(define-error 'nxml-error nil)
(define-error 'nxml-file-parse-error "Error parsing file" 'nxml-error)




  Fput (Quser_search_failed, Qerror_conditions,
        listn (CONSTYPE_PURE, 4,
               Quser_search_failed, Quser_error, Qsearch_failed, Qerror));


  Fput (Quser_search_failed, Qerror_message,
        build_pure_c_string ("Search failed"));
















































































LISTS


;;==============================================;;


(defun list-mtg-cards (&optional )


  "List all (known) ‘mtg-card’s."


  (interactive )


  ())


;;==============================================;;


(defun list-mtg-editions (&optional )


  "List all (known) ‘mtg-edition’s."


  (interactive )


  ())


;;==============================================;;








;;==============================================;;










;;==============================================;;
































;;; COMPLETION:


;;==============================================;;


(defun mtg-completion-symbols-within-backticks-in-comments ()


  "Return Completion-Candidates by scanning within backticks.


Examples:


• Given « # The \\`expropriate\\` keyword: », returns « (list \"extropriate\") ».


Links:


• Ported from URL ‘https://oremacs.com/page4/’'


      (save-match-data
        (save-excursion
          (goto-char (point-min))


    (let* ((REGEXP (rx (: "`" (group (+ alphanum space)) "`")))


(CANDIDATES 
          (cl-loop while (re-search-forward REGEXP nil t)
            collect (or (match-string-no-properties 1) (match-string-no-properties 2)))


  CANDIDATES))))


;;----------------------------------------------;;


(defun mtg-completing-symbols-within-backticks-in-comments ()


  "Complete after a backtick.


Examples:


• Given « # The \\`expropriate\\` and \\`f█ keywords: », returns « (list \"extropriate\") ».


Links:


• Ported from URL ‘https://oremacs.com/page4/’'


    (let* ((REGEXP (rx (: "`" (+ alphanum space))))


(CANDIDATES (mtg-completion-symbols-within-backticks-in-comments)))


  (save-match-data


  (when (and (looking-back REGEXP) CANDIDATES)


        (list (match-beginning 0) (match-end 0) CANDIDATES))))


;;----------------------------------------------;;






































































;;; READERS:


;;==============================================;;


(cl-defun mtg-read-multiple-choice (prompt choices)


  "Read from CHOICES in the Minibuffer.


Output:


• an element of CHOICES.


Inputs:


• PROMPT — a ‘stringp’.


• CHOICES — a (homogeneous) ‘sequencep’,
where each element satisfies/implements:


    • generic function ‘mtg-name-string’.
    • generic function ‘mtg-abbr-char’.


Effects:


• Minibuffer — Open a multiple-choice Minibuffer.
User enters a single character.


Related:


• Wraps function ‘read-multiple-choice’."


  (cl-destructuring-bind (RMC-CHOICES ALIST-CHOICES)


  ;; ^ “RMC” abbreviates ‘read-multiple choice’.


           (cl-loop for CHOICE being each element of choices


              with CHAR   = (mtg-abbr-char CHOICE)
              with STRING = (mtg-name-string CHOICE)


              collect (list CHAR STRING) into RMC-CHOICES
              collect (cons CHAR CHOICE) into ALIST-CHOICES
              finally return (list RMC-CHOICES ALIST-CHOICES))))


 ;; ^ TODO: disambiguate intelligently (e.g. if two separate CHARs are both « ?b », change one to « ?B »).


    (cl-destructuring-bind (CHAR &rest) 
        (read-multiple-choice prompt RMC-CHOICES)


      (assq CHAR ALIST-CHOICES))))


;;==============================================;;


;;----------------------------------------------;;


(cl-defun mtg-read- (&optional (prompt ": "))


  "Read an ‘mtg-’ from the Minibuffer.


Output:


• an ‘mtg-’.


Inputs:


• PROMPT — an optional ‘stringp’.


Examples:


• M-: (mtg-- )
   ↪ \\='()"


  (declare)


  (cl-check-type  ')


  (cl-assert ())


  (let* (( ())
         ( ())
         )


    (read- )))


;;----------------------------------------------;;


(cl-defun mtg-read-rarity (&optional (prompt "Rarity: ") (rarities mtg-rarity-list))


  "Read an ‘mtg-rarity’ from the Minibuffer.


Output:


• an ‘mtg-rarity’.


Inputs:


• PROMPT — an optional ‘stringp’.


• RARITIES — Defaults to variable ‘mtg-rarity-list’.


  (mtg-read-multiple-choice prompt rarities))


;;----------------------------------------------;;


(cl-defun mtg-read-color (&optional (prompt "Color: ") (colors mtg-color-list))


  "Read an ‘mtg-color’ from the Minibuffer.


Output:


• an ‘mtg-color’.


Inputs:


• PROMPT — an optional ‘stringp’.


• COLORS — Defaults to variable ‘mtg-color-list’.


Links:


• Wraps function ‘mtg-read-multiple-choice’."


  (cl-check-type prompt 'string)
;;TODO:  (cl-check-type colors '(seq-of mtg-color))


  (mtg-read-multiple-choice prompt colors))


;;----------------------------------------------;;


;;==============================================;;














































BOOL-VECTOR


;;----------------------------------------------;;


(defalias 'mtg-colorless #'mtg-make-colors)


;;==============================================;;


(defvar mtg-colors


  `(,(make-mtg-color :abbr 'w :name 'white :names '(:en "white" :es "blanco" :fr "" :de "" :it "" :pt "" :ru "" :zh "" :ko "" :ja ""))
    ;; ^


    ,(make-mtg-color :abbr 'u :name 'blue :names '(:en "blue" :es "azul" :fr "bleu" :it "blu" :pt "azul" :de "blau" :ru "синий" :zh "蓝色" :ko "파란색" :ja "青")
    ;; ^ See “Ricochet Trap”.
;;TODO 蓝色? 色?


    ,(make-mtg-color :abbr 'b :name 'black :names '(:en "black" :es "" :fr "" :de "" :it "" :pt "" :ru "" :zh "" :ko "" :ja ""))
    ;; ^ See “”.


    ,(make-mtg-color :abbr 'r :name 'red :names '(:en "red" :es "" :fr "" :de "" :it "" :pt "" :ru "" :zh "" :ko "" :ja ""))
    ;; ^ See “”.


    ,(make-mtg-color :abbr 'g :name 'green :names '(:en "green" :es "" :fr "" :de "" :it "" :pt "" :ru "" :zh "" :ko "" :ja ""))
    ;; ^ See “”.


    ;; ,(make-mtg-color :abbr ' :name ' :names '(:en "" :es "" :fr "" :de "" :it "" :pt "" :ru "" :zh "" :ko "" :ja ""))
    ;; ;; ^ See “”.
   )


  "All ‘mtg-color’s.


Type: a ‘listp’ of ‘mtg-color-p’s.


This variable is extensible, but only by appending new ‘mtg-color’s, i.e.: 


• DON'T prepend!
• DON'T re-order!")


;; Notes:
;;
;; • Color Words in non-English Languages:
;;   e.g. “* Elemental Blast” (URL ‘https://scryfall.com/search?as=grid&order=name&q=%22Elemental+Blast%22’)
;;   e.g. “Ricochet Trap” says “blue”, and has been printed in most languages (URL ‘https://scryfall.com/search?q=%21%E2%80%9CRicochet+Trap%E2%80%9D+set%3Awwk+lang%3Aany&unique=prints’).
;;
;;


;; Translations of “(a) blue spell”:
;;
;; • “a blue spell” (en)
;;
;; • “un hechizo azul” (es)
;;
;; • “un sort bleu” (fr)
;;
;; • “una magia blu” (it)
;;
;; • “uma mágica azul” (pt)
;;
;; • “einen blauen Zauberspruch” (de)
;;
;; • “синее заклинание” (ru)
;;
;; • “” (zh)
;;
;; • “” (ko)
;;
;; • “青の呪文” (ja)
;;
;; 
;;


;; TODO Korean:


횐색 (hwoensaek) – white


파란색 (pharansaek) – blue


검은색 (geomeunsaek) – black


빨간 색 (bbalkan saek) – red


녹색 (noksaek) – green


;; TODO: 5+? mtg-known-colors-how-many (statically)?


;;----------------------------------------------;;


(deftype mtg-color-bool-vector


  '(satisfies mtg-color-bool-vector-p)


  "Power-Set of ‘mtg-colors’ (as a CL Type).


This type represents the Power-Set of variable ‘mtg-colors’;
each ‘mtg-color-bool-vector-p’ represents a Set of ‘mtg-color-p’s.")


;;----------------------------------------------;;


(defun mtg-color-bool-vector-p (object)


  "Whether OBJECT satisfies the type variable ‘mtg-color-bool-vector’."


  (declare (error-free t) (side-effect-free t))


  (and (bool-vector-p object)
       (>= (length object) (mtg-known-colors-how-many))))


;;----------------------------------------------;;


(define-inline mtg-known-colors-how-many ()


  "Return the number of ‘mtg-color’s (in ‘mtg-color-list’).


(There are five MTG Colors, by default.)"


  (declare (side-effect-free t))


  (inline-quote
    (length mtg-color-list)))


;;----------------------------------------------;;


(defun mtg-make-colors (&rest colors)


  "Return an ‘mtg-colors-bool-vector-p’ representing COLORS.


=== Inputs ===


• COLORS — a ‘listp’ of ‘symbolp’s.


=== Output ===


• an ‘mtg-colors-bool-vector-p’.


=== Examples ===


• M-: (mtg-make-colors \\='u \\='g)
  ↪ #&5\"\"


• M-: (equals (mtg-make-colors \\='u \\='g) (make-bool-vector nil t nil nil t))
  ↪ t


=== Laws ===


• (‘mtg-make-colors’) ≡ (‘mtg-make-colorless’)"


  (declare (side-effect-free t));;TODO internal-only effects okay? (has no externally-visible effects)


  (cl-loop
    with BOOL-VECTOR = (mtg-make-colorless)


    for COLOR in colors
      do (aset BOOL-VECTOR (mtg-get-color-index COLOR) t)


    finally return BOOL-VECTOR))


;;----------------------------------------------;;


(defun mtg-make-colorful ()


  "Return a colorful/maximal ‘mtg-color-bool-vector-p’ (i.e. “WUBRG”, by default)."


  (declare (side-effect-free t))  ;;TODO  (error-free t)?


  (let ((LENGTH (mtg-known-colors-how-many)))


    (make-bool-vector LENGTH t)))


;;----------------------------------------------;;


(defun mtg-make-colorless ()


  "Return a colorless/minimal ‘mtg-color-bool-vector-p’."


  (declare (side-effect-free t))  ;;TODO  (error-free t)?


  (let ((LENGTH (mtg-known-colors-how-many)))


    (make-bool-vector LENGTH nil)))


;;----------------------------------------------;;


(define-inline mtg-colors-how-many (bv)


  "How many colors BV represents.


See ‘bool-vector-count-population’.


Examples:


• M-: (mtg-colors-how-many (mtg-make-colorless))
  ↪ 0


• M-: (mtg-colors-how-many (mtg-make-colorful))
  ↪ 5"


  (declare (side-effect-free t))


  (inline-quote (bv)
    (inline-quote
      (bool-vector-count-population ,bv))))


;;----------------------------------------------;;


(define-inline mtg-colors-not (bv)


  "Logical-“NOT” (Set-Complement) of BV.


See ‘bool-vector-not’.


Examples:


• (mtg-colors-not (mtg-make-colors \\='black)) represents “nonblack”."


  (declare (side-effect-free t))


  (inline-quote (bv)
    (inline-quote
      (bool-vector-not ,bv))))


;;----------------------------------------------;;


(define-inline mtg-colors-either (bv1 bv2)


  "Logical-“OR” (Set-Union) of BV1 and BV2.


See ‘bool-vector-union’."


  (declare (side-effect-free t))


  (inline-quote (bv1 bv2)
    (inline-quote
      (bool-vector-union ,bv1 ,bv2))))


;;----------------------------------------------;;


(define-inline mtg-colors-both (bv1 bv2)


  "Logical-“AND” (Set-Intersection) of BV1 and BV2.


See ‘bool-vector-intersection’."


  (declare (side-effect-free t))


  (inline-quote (bv1 bv2)
    (inline-quote
      (bool-vector-intersection ,bv1 ,bv2))))


;;----------------------------------------------;;


(define-inline mtg-colors- (bv1 bv2)


  "Logical-“” (Set-) of BV1 and BV2.


See ‘bool-vector-’."


  (inline-quote (bv1 bv2)
    (inline-quote
      (bool-vector- ,bv1 ,bv2))))


;;----------------------------------------------;;


(define-inline mtg-subcolors-p (bv1 bc2)


  "Whether BV1 is subsumed by BV2.


See ‘bool-vector-subsetp’.


Inputs:


• BV1 — a ‘bool-vector-p’.
• BV2 — a ‘bool-vector-p’


Output:


• a ‘booleanp’.
• t — if each color of BV1 is a color of BV2 too.


=== Notes ===


This function is a simple comparator for ‘mtg-color-bool-vector’. Also see function ‘mtg-compare-colors’.


TODO For example, to sort ‘mtg-color-bool-vector-p’s, in “ascending order” from colorless (function ‘mtg-make-colorless’) to more colorful:


    (cl-sort _ #'mtg-subcolors-p)"


  (declare (side-effect-free t))


  (inline-quote (bv1 bv2)
    (inline-quote
      (bool-vector-subsetp ,bv1 ,bv2))))


;; TODO Naming: mtg-subcolors-p? mtg-color-bool-vector-subset-p? mtg-color-bool-vector/less-than-or-equal-to-p? mtg-color-bool-vector/subsumed-by-p?


;;----------------------------------------------;;


(defun mtg-compare-colors (colors1 colors2)


  "Whether COLORS1 and COLORS2 are comparable, and how they compare.


Inputs:


• COLORS1 — a ‘bool-vector-p’.
• COLORS2 — a ‘bool-vector-p’.


Output a ‘symbolp’, one of:


• nil — COLORS1 and COLORS2 aren't comparable.
e.g. COLORS1 is Simic (Blue & Green), COLORS2 is Izzet (Blue & Red).


• \\='lt — COLORS1 is strictly subsumed by COLORS2.
e.g. COLORS1 is Simic, COLORS2 is Temur (Blue & Red & Green).


• \\='eq — COLORS1 equals COLORS2.
e.g. COLORS1 is Simic, COLORS2 is Simic too.


• \\='gt — COLORS1 strictly subsumes COLORS2.
e.g. COLORS1 is Simic, COLORS2 is Green.


=== Notes ===


This function is the canonical comparator between ‘mtg-color-bool-vector’s,
inducing a Complete Partial Order.


=== Laws ===


• M-: (mtg-compare-colors (‘mtg-make-colorless’) (‘mtg-make-colorful’))
   ↪ \\='lt


=== Links ===


• URL ‘https://en.m.wikipedia.org/wiki/Complete_partial_order’"


  (declare (side-effect-free t))


  (inline-quote (colors1 colors2)
    (inline-quote


  (cond ((equal ,colors1 ,colors2) 'eq)


        ((bool-vector-subsetp ,colors1 ,colors2) 'lt)


        ((bool-vector-subsetp ,colors2 ,colors1) 'gt)


        (otherwise nil)))))


;;----------------------------------------------;;


(define-inline mtg-get-color-index (c)


  "Return the index, w.r.t. ‘mtg-color-list’, of C (an ‘mtg-color-abbrev’ or ‘mtg-color-name’).


C — a ‘symbolp’."


  (declare (side-effect-free t))


  (inline-letevals ((C c))
    (inline-quote


  (cl-loop for COLOR being each element of mtg-color-list using (index INDEX)


      thereis (when (or (eq ,C (mtg-color-abbrev COLOR))
             (eq ,C (mtg-color-name COLOR)))
      INDEX)))))


;;----------------------------------------------;;


(define-inline mtg-index-color (index)
  "Index ‘mtg-color-list’ at INDEX."


  (declare (side-effect-free t))


  (inline-letevals ((INDEX index))
    (inline-quote
      (elt mtg-color-list ,INDEX))))


;;----------------------------------------------;;


(defun mtg-print-colors (colors)


  "Pretty-Print COLORS (an ‘mtg-color-bool-vector-p’)."


  (declare (side-effect-free t))


  (seq-let [W U B R G &rest COLORS] colors  ;TODO make extensible.


    (concat (if W "W" "")
            (if U "U" "")
            (if B "B" "")
            (if R "R" "")
            (if G "G" "")
            (if COLORS
                (cl-loop for BOOL across COLORS using (index INDEX)
                         if BOOL concat (upcase (symbol-name (mtg-color-abbr (mtg-index-color (+ 5 INDEX))))))
              ""))))


;;==============================================;;


(defstruct (mtg-pt
             (:constructor mtg-pt--make)
             (:copier      copy-mtg-pt))


  "Power/Toughness of an MTG Creature.


“P/T” as in ‘mtg-card-pt’."


  (power     0 :type (or integer symbol))
  (toughness 0 :type (or integer symbol))
  )


;;----------------------------------------------;;


(cl-defun mtg-pt (pow &optional tou)


  "Make an ‘mtg-pt’.


If POU is a ‘stringp’, then TOU must be nil, and POW is parsed as both.


=== Examples ===


e.g. “Squire”:


(mtg-pt 1 2)
↪ #s(mtg-pt 1 2)


(mtg-pt \"1/2\")
↪ #s(mtg-pt 1 2)


e.g. “Tarmogoyf”:


(mtg-pt \"*/1+*\")
↪ #s(mtg-pt * 1+*)


(mtg-pt '* '1+*)
↪ #s(mtg-pt * 1+*)


(mtg-pt (quote *) (quote 1+*))
↪ #s(mtg-pt * 1+*)


Links:


• Wraps ‘make-mtg-pt’."


  (cl-check-type pow '(or integer symbol string))
  (cl-check-type tou '(or integer symbol))


  ())


;;----------------------------------------------;;


(cl-defun make-mtg-pt (&key power toughness pow tou)


  "Make an ‘mtg-pt’, with validation & defaulting.


• POW is alias for POWER.
• TOU is alias for TOUGHNESS."


  (mtg-assert-numeral power)
  (mtg-assert-numeral toughness)
  (mtg-assert-numeral pow)
  (mtg-assert-numeral tou)


  (let* ((POWER     (or power     pow))
         (TOUGHNESS (or toughness tou)))


    (mtg-pt--make :power POWER :toughness TOUGHNESS)))


;;----------------------------------------------;;


(define-inline mtg-pt-integer-power (pt &optional star)


  "Convert PT's ‘mtg-pt-power’ to an actual ‘integerp’.


• PT — an ‘mtg-pt-p’.


• STAR — an optional ‘integerp’.
See function ‘mtg-resolve-numeral’.


M-: (let ((TARMOGOYF-PT (mtg-pt \"*/1+*\")))
      (mtg-pt-integer-power TARMOGOYF-PT))
 ↪ 0"


  (inline-letevals ((POWER (mtg-pt-power PT))
                    (STAR (or star 0)))
    (inline-quote
      (cl-typecase ,POWER
        (integer ,POWER)
        (symbol (mtg-resolve-numeral ,POWER :star ,STAR))))))


;;----------------------------------------------;;


(define-inline mtg-pt-integer-toughness (pt &optional star)


  "Convert PT's ‘mtg-pt-toughness’ to an actual ‘integerp’.


• PT — an ‘mtg-pt-p’.


• STAR — an optional ‘integerp’.
See function ‘mtg-resolve-numeral’.


M-: (let ((TARMOGOYF-PT (mtg-pt \"*/1+*\")))
      (mtg-pt-integer-toughness TARMOGOYF-PT))
 ↪ 1"


  (inline-letevals ((TOUGHNESS (mtg-pt-toughness PT))
                    (STAR (or star 0)))
    (inline-quote
      (cl-typecase ,TOUGHNESS
        (integer ,TOUGHNESS)
        (symbol  (mtg-resolve-numeral ,TOUGHNESS :star ,STAR))))))


;;----------------------------------------------;;


(cl-defun mtg-resolve-numeral (numeral &key (star 0) &allow-other-keys)
  
  "Resolve NUMERAL to an ‘integerp’.


• NUMERAL — a ‘symbolp’ or a ‘listp’ (S-Expression of ‘integerp’s and/or ‘symbolp’s).
e.g. “1+*” or “6” or “-1” or \\='(1 + *);
MTG numerals are (simple) integral arithmetic expressions,
including integer constants.


• STAR — an optional ‘integerp’.
Defaults to 0.
STAR is the value use for an asterix,
i.e. the “*” in “1+*”.
Defaults to 0.


M-: (mtg-resolve-numeral \"6")
 ↪ 1


M-: (mtg-resolve-numeral \"1+*\")
 ↪ 1"


  (cl-check-type numeral '(or aymbol (list-of (or symbol integer))))
  (cl-check-type star 'integer)


  (let* ((VARS (unless (eql 0 star)
                 `((* . ,star))))
         (SEXP (TODO (symbol-name numeral)))
         )


    (mtg-eval-numeral SEXP VARS)))


;;----------------------------------------------;;


(cl-defun mtg-eval-numeral (sexp &optional (vars '((* . 0))))
 
  "Evaluate an MTG numerical expression.


• SEXP — either a ‘stringp’, or an S-Expression of ‘integerp’s and/or ‘symbolp’s.
Binary Operations are infix.
Examples:


    • “1+*”
    • \\='(1 + *) 
    • 6
    • \\='(13 - _)


• VARS — an ‘integerp’, or an Association-‘listp’ from ‘symbolp’s (variables) to ‘integerp’s (values).
Defaults to \\='((* . 0)).
An ‘integerp’ (e.g. ‘1’) is equivalent to “binding” the symbol ‘*’ to that ‘integerp’ (e.g. \\='((* . 1))).
VARS are “variable bindings”.


M-: (mtg-eval-numeral 0)
 ↪ 0


M-: (mtg-eval-numeral \\='*)
 ↪ 0


M-: (mtg-eval-numeral \"1+*\")
 ↪ 1


M-: (mtg-eval-numeral \"1+*\" 0)
 ↪ 1


M-: (mtg-eval-numeral \"1+*\" 6)
 ↪ 7


M-: (mtg-eval-numeral \"1+*\" \\='((* . 6)))
 ↪ 7


M-: (mtg-eval-numeral \\='(1 + *))
 ↪ 1


M-: (mtg-eval-numeral \\='(13 - _) \\='((_ . 12)))
 ↪ 1"


  (cl-check-type sexp '(or string (or symbol integer (list-of (or symbol integer)))))
  (cl-check-type vars '(list-of (cons-of symbol integer)))


  ;;TODO: non-string SEXP.




  (let* ((ARITHMETIC-EXPRESSION 
(mtg--replace-string-in-string sexp "*" "$"))
         (VARIABLE-BINDINGS
(cl-typecase vars
        (integer `((* . ,vars)))
        (list vars))))


    (calc-eval ARITHMETIC-EXPRESSION nil (assq '* VARIABLE-BINDINGS))))


;; ‘calc-eval’:
;;
;; e.g. (calc-eval "$/$$" nil "7" "1+1")
;;


;;----------------------------------------------;;




;;==============================================;;


(defstruct (mtg-mana-cost (:constructor make-mtg-mana-cost)
                          (:copier      copy-mtg-mana-cost))


  "MTG Mana Cost.


Represents a histogram (a.k.a. an “unsorted multiset”) of ‘mtg-mana-symbol’s.


Related:


- This type is the “cost” as in ‘mtg-card-cost’."


  ;; Generic Mana:


  (generic 0 :type integer)  ; e.g. 2 means “{2}”.


  (variable-x 0 :type integer)  ; e.g. 2 means “{X}{X}”.
  (variable-y 0 :type integer)  ; e.g. 2 means “{Y}{Y}”.
  (variable-z 0 :type integer)  ; e.g. 2 means “{Z}{Z}”.


  ;; Colored Mana:


  (white 0 :type integer)  ; e.g. 2 means “{W}{W}”.
  (blue  0 :type integer)  ; e.g. 2 means “{U}{U}”.
  (black 0 :type integer)  ; e.g. 2 means “{B}{B}”.
  (red   0 :type integer)  ; e.g. 2 means “{R}{R}”.
  (green 0 :type integer)  ; e.g. 2 means “{G}{G}”.


  (colorless 0 :type integer)  ; e.g. 2 means “{C}{C}”.


  ;; Phyrexian Mana:


  (white-phyrexian 0 :type integer)  ; e.g. 2 means “{pW}{pW}”.
  (blue-phyrexian  0 :type integer)  ; e.g. 2 means “{p}{p}”.
  (black-phyrexian 0 :type integer)
  (red-phyrexian   0 :type integer)
  (green-phyrexian 0 :type integer)


  ;; One-Color Hybrid Mana:


  (white-hybrid 0 :type integer)  ; e.g. 2 means “{2/W}{2/W}”.
  (blue-hybrid  0 :type integer)  ; e.g. 2 means “{2/}{2/}”.
  (black-hybrid 0 :type integer)  ; e.g. 2 means “{2/}{2/}”.
  (red-hybrid   0 :type integer)  ; e.g. 2 means “{2/}{2/}”.
  (green-hybrid 0 :type integer)  ; e.g. 2 means “{2/}{2/}”.


  ;; Two-Color Hybrid Mana:


  (azorius  0 :type integer)  ; e.g. 2 means “{WU}{WU}”.
  (dimir    0 :type integer)  ; e.g. 2 means “{}{}”.
  (rakdos   0 :type integer)
  (gruul    0 :type integer)
  (selesnya 0 :type integer)
  (orzhov   0 :type integer)
  (golgari  0 :type integer)
  (simic    0 :type integer)
  (izzet    0 :type integer)
  (boros    0 :type integer)


  ;; Other Mana:


  (snow 0 :type integer)  ; e.g. 2 means “{S}{S}”.


  ;; ( 0 :type integer)  ; e.g. 2 means “{}{}”.


  )


;; 107.4…
;;
;; “107.4. The mana symbols are {W}, {U}, {B}, {R}, {G}, and {C}; the numerical symbols {0}, {1}, {2}, {3}, {4}, and so on; the variable symbol {X}; the hybrid symbols {W/U}, {W/B}, {U/B}, {U/R}, {B/R}, {B/G}, {R/G}, {R/W}, {G/W}, and {G/U}; the monocolored hybrid symbols {2/W}, {2/U}, {2/B}, {2/R}, and {2/G}; the Phyrexian mana symbols {W/P}, {U/P}, {B/P}, {R/P}, and {G/P}; and the snow symbol {S}.”


mtg-mana-cost


struct OR ‘hash-table-p’? to map mana-symbols / mana-numbers to ‘natnump’s.


e.g. XX2UU:
'U 2 
'X 2
*generic* 2


;;==============================================;;


;;==============================================;;


(defstruct (mtg-cost      (:include mtg-mana-cost)
                          (:constructor make-mtg-mana-cost)
                          (:copier      copy-mtg-mana-cost))


  "MTG Cost.


MTG Cost extends MTG Mana Cost (i.e. variable ‘mtg-mana-cost’) with nonmana costs.


=== Examples ===


For example, the card ‹Sinister Concoction› has an activated ability costing « {B}, Pay 1 life, Put the top card of your library into your graveyard, Discard a card, Sacrifice Sinister Concoction ». This ‘mtg-rules-text’ splits into these actions / payments:


• « {B} » — 
• « Pay 1 life » — 
• « Put the top card of your library into your graveyard » — 
• « Discard a card » — 
• « Sacrifice ~ » — 


which can be parsed into this ‘mtg-cost’:


    M-: (make-mtg-cost :black 1 :life 1 :mill 1 :discard 1 :sacrifice-this t)


=== Links ===


• URL ‘https://www.yawgatog.com/resources/magic-rules/’"


  ;; Player costs (i.e. stuff YOU pay or do):


  (energy 0 :type integer)  ; e.g. 2 means “{E}{E}”.
  (life 0 :type integer)  ; e.g. 2 means “Pay 2 life”.
  (discard 0 :type integer)  ; e.g. 2 means “Discard two cards”.
  (mill 0 :type integer)  ; e.g. 2 means “Put the top two cards of your library into your graveyard”.
  (cremate 0 :type integer)  ; e.g. 2 means “Exile two cards from your graveyard”.


  ;; Permanent costs (i.e. actions upon THIS permanent):


  (tap-this nil :type boolean)  ; e.g. t means “{T}”.
  (untap-this nil :type boolean)  ; e.g. t means “{Q}”.
  (sacrifice-this nil :type boolean)  ; e.g. t means “Sacrifice ~”.
  (rem-these-plus-counters 0 :type integer)  ; e.g. 2 means “Remove two +1/+1 counters from ~”.
  (rem-these-minus-counters 0 :type integer)  ; e.g. 2 means “Remove two -1/-1 counters from ~”
  (put-these-minus-counters 0 :type integer)  ; e.g. 2 means “Put two -1/-1 counters on ~”
  (exert-this nil :type boolean)  ; e.g. t means “Exert ~”.


;;  (-this nil :type boolean)  ; e.g. t means “ ~”.
;;  ( 0 :type integer)  ; e.g. 2 means “ two ”.


  ;; Unknown/Unparseable costs ():


  (rest nil :type (or null string))  ; e.g. “???”.


  )


;; 118. Costs…
;;
;; “118.1. A cost is an action or payment necessary to take another action or to stop another action from taking place. To pay a cost, a player carries out the instructions specified by the spell, ability, or effect that contains that cost.”


;; 107.5…
;;
;; “107.5. The tap symbol is {T}. The tap symbol in an activation cost means "Tap this permanent."”


;; 107.6…
;;
;; “107.6. The untap symbol is {Q}. The untap symbol in an activation cost means "Untap this permanent."”


;; 107.7…
;;
;; “107.7. [+N] means "Put N loyalty counters on this permanent," [-N] means "Remove N loyalty counters from this permanent," and [0] means "Put zero loyalty counters on this permanent."


;; 107.8…
;;
;; “107.8a. "{LEVEL N1-N2}[Abilities] [P/T]" means "As long as this creature has at least N1 level counters on it, but no more than N2 level counters on it, it has base power and toughness [P/T] and has [abilities]."”
;; “107.8b. "{LEVEL N3+} [Abilities] [P/T]" means "As long as this creature has N3 or more level counters on it, it has base power and toughness [P/T] and has [abilities]."”


;; 107.14…
;;
;; “107.14. The energy symbol is {E}. It represents one energy counter. To pay {E}, a player removes one energy counter from themselves.”


;; 107.15…
;;
;; “107.15. The text box of a Saga card contains chapter symbols... A chapter symbol includes a Roman numeral, indicated here as "rN".”
;; “107.15a. "{rN}--[Effect]" means "When one or more lore counters are put onto this Saga, if the number of lore counters on it was less than N and became at least N, [effect]."”
;; “107.15b. "{rN1}, {rN2}--[Effect]" is the same as "{rN1}--[Effect]" and "{rN2}--[Effect]."”


;; 602.…
;;
;; “602.1a The activation cost is everything before the colon (:). An ability’s activation cost must be paid by the player who is activating it.”
;;
;; “602.1b If an activated ability has any activation instructions, they appear last, after the ability’s effect.”
;;
;; “”


;; .…
;;
;; “”


;; .…
;;
;; “”


;;----------------------------------------------;;


(defun mtg-activation-cost ()


  "Make an ‘mtg-cost’ from an activation cost."


  ())


;;----------------------------------------------;;


(defun mtg-parse-activation-cost (text)


  "Parse TEXT into an activation cost.


Output: an ‘mtg-cost’.


Inputs:


• TEXT — a ‘stringp’."


  (declare (side-effect-free t))


  (cl-check-type text 'string)
  (cl-check-type  ')


  (cl-assert ())


  (let* ((TEXT (mtg-normalize-string text))
         (COST (mtg-cost--make))) 


    (cl-loop for COST in (mtg-cost--split-activation-cost TEXT)
TODOTODO
)))


;;----------------------------------------------;;


(defconst mtg-english--activation-cost-separator ",")


(defconst mtg-english--activation-cost-separator-regexp ",[ \t\n]*")


;;----------------------------------------------;;


(define-inline mtg-cost--split-activation-cost (text)


  "Split TEXT into its costs.


Output: a ‘listp’ of ‘stringp’s.
See function ‘split-string’.


Inputs:


• TEXT A — a ‘stringp’.


Examples:


• M-: (mtg-cost--split-activation-cost \"{B}, Pay 1 life, Put the top card of your library into your graveyard, Discard a card, Sacrifice Sinister Concoction\")


   ↪ \\='(\"{B}\" \"Pay 1 life\" \"Put the top card of your library into your graveyard\" \"Discard a card\" \"Sacrifice Sinister Concoction\")"


  (inline-letevals (text)
    (inline-quote
      (split-string ,text ",[ \t\n]*" t))))




;;----------------------------------------------;;




;;----------------------------------------------;;


;;==============================================;;


;;==============================================;;


;; “Type Line: Part of a card. The type line is printed directly below the illustration and contains the card's card type(s), subtype(s), and/or supertype(s). See rule 205, "Type Line."”


;;==============================================;;


defstruct mtg-counter


"


Kinds of Counters for Permanents:


• -1/-1
• +1/+1
• charge
• loyalty
• level
• lore
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 


Kinds of Counters for Players:


• poison
• energy
• experience


"


;;==============================================;;


;;----------------------------------------------;;


(defun mtg-mana-cost-cmc (mana-cost)


  "Return the Converted MANA-COST.


Inputs:


• MANA-COST — a ‘mana-cost-p’.


Output:


• a ‘natnump’."


  (declare (side-effect-free t))


  (cl-check-type mana-cost mtg-mana-cost)


  (mtg-with-slots mana-cost 


    (+ generic  
       snow


       white
       blue 
       black
       red  
       green


       colorless


       white-phyrexian
       blue-phyrexian 
       black-phyrexian
       red-phyrexian  
       green-phyrexian


       (* 2 white-hybrid)
       (* 2 blue-hybrid)
       (* 2 black-hybrid)
       (* 2 red-hybrid)
       (* 2 green-hybrid)


       azorius 
       dimir   
       rakdos  
       gruul   
       selesnya
       orzhov  
       golgari 
       simic   
       izzet   
       boros   


       )))


;;----------------------------------------------;;


(defun mtg-compare-mana-costs (mana-cost-1 mana-cost-2)


  "Whether MANA-COST-1 and MANA-COST-2 are comparable, and how they compare.


Inputs:


• MANA-COST-1 — a ‘mana-cost-p’.
• MANA-COST-2 — a ‘mana-cost-p’


Output:


• a ‘symbolp’.


• nil — 


• \\='lt — 


• \\='eq — 


• \\='gt — 


=== Notes ===


This function is the canonical comparator between ‘mtg-mana-cost’s,
inducing a Complete Partial Order.


=== Links ===


• URL ‘https://en.m.wikipedia.org/wiki/Complete_partial_order’"


  (let ((COMPARED-COLORS t))


    (cond ((equals mana-cost-1 mana-cost-2) 'eq)


          ((and (< (mtg-mana-cost-generic mana-cost-1) (mtg-mana-cost-generic mana-cost-2))
                (progn
                  (when (eq t COMPARED-COLORS)
                    (setq COMPARED-COLORS (mtg-compare-colors (mtg-mana-cost-colored mana-cost-1) (mtg-mana-cost-colored mana-cost-2)))))  ;;TODO it's a histogram (an “integer-vector”), not a bool-vector.
                  (eq 'lt COMPARED-COLORS)))))
            'lt)


            (() 'gt)


            (otherwise nil))))


;;----------------------------------------------;;


;;----------------------------------------------;;


;;==============================================;;


DATA: CharTables


;; ‘char-table-p’s:
;;
;;(make-char-table SUBTYPE &optional INIT)
;;


;;----------------------------------------------;;


TODO for keys?


;;----------------------------------------------;;


;;==============================================;;








































































TYPES


;;----------------------------------------------;;


;;----------------------------------------------;;


(cl-deftype mtg-numeral `(or integer symbol)


  "MTG Numerals include power/toughness/loyalty.


Generalizes ‘integerp’s with simple arithmetic expressions on wildcards.
For example:


• ‹Tarmogoyf›'s ‘mtg-card-toughness’ is symbol ‘1+*’.
• ‹Nim Shrieker›'s ‘mtg-card-power’ is number ‘0’
(despite having a continuously-varying one, similar to ‹Tarmogoyf›, from its ‘mtg-card-rulestext’ “~ gets +1/+0 for each artifact you control.”)


Links: URL ‘’")


;;----------------------------------------------;;


;;(cl-deftype mtg-)


;;==============================================;;


(cl-defstruct (mtg-card (:constructor mtg-card--create)
                        (:copier      mtg-card--copy))


"
• ORIGINAL      — zero-or-one ‘mtg-original-printing-p’.
• PRINTINGS     — one-or-more ‘mtg-printing-p’s.
"


 ;; Mechanical & Primary:


  (name          nil :type (or null ))
  (cost          nil :type (or null ))
  (cardtypes     nil :type (or null ))
  (subtypes      nil :type (or null ))
  (supertypes    nil :type (or null ))
  (colors        nil :type (or null ))
  (rulestext     nil :type (or null ))
  (power         nil :type (or null mtg-numeral))
  (toughness     nil :type (or null mtg-numeral))
  (loyalty       nil :type (or null mtg-numeral))


  ;; Mechanical & Secondary (i.e. can be derived from the “Primary” above):


  (cmc           0)
  (coloridentity nil)


  ;; Non-Mechanical & Internal (i.e. present on the card itself):


  (original  nil :type (or null mtg-original-printing))
  (printings nil :type (list mtg-printing))


  ;; Non-Mechanical & External (i.e. from an external resource, like a website):


  (rulings       nil :type (or null ))
  (legality      nil :type (or null ))
)


;;----------------------------------------------;;


(defun make-mtg-card (&rest kwargs)


  "Construct & Validate an ‘mtg-card-p’."


  ;; Validate all constraints, to report all errors
  ;; (c.f. ‘cl-assert’s, where only the first failing assertion is reported):


(let ((POWER==TOUGHNESS (mtg-numerals-equal power toughness))


 (cl-assert POWER==TOUGHNESS "")






;;TODO not cl-assert define mtg--asserts?


;;----------------------------------------------;;


(defun copy-mtg-card (card)


  "Return a deep copy of CARD (an ‘mtg-card-p’)."


  (let ((NEW-CARD (mtg-card--copy card)))


    (progn


      (cl-callf copy-mtg-printings (mtg-card-printings NEW-CARD))
      (cl-callf copy-mtg-original-printing (mtg-card-original NEW-CARD))


      (cl-callf copy-mtg-mana-cost (mtg-card-cost NEW-CARD))
      (cl-callf copy-mtg-cardtypes (mtg-card-cardtypes NEW-CARD))
      (cl-callf copy-mtg-subtypes (mtg-card-subtypes NEW-CARD)
      (cl-callf copy-mtg-supertypes (mtg-card-supertypes NEW-CARD))


      (cl-callf copy-mtg- (mtg-card- NEW-CARD))
      (cl-callf copy-mtg- (mtg-card- NEW-CARD)


      NEW-CARD)))  ; the 
 
;;TODO: copy-mtg-printings copy-mtg-original-printing copy-mtg-mana-cost copy-mtg-cardtypes copy-mtg-subtypes copy-mtg-supertypes copy-mtg- 


;;


(defalias 'copy-mtg-cardtypes #'copy-sequence)


;; ^ Shallow-Copy works for a list-of-atoms.


(defalias 'copy-mtg-subtypes #'copy-sequence)


;; ^ Shallow-Copy works for a list-of-atoms.


(defalias 'copy-mtg-supertypes #'copy-sequence)


;; ^ Shallow-Copy works for a list-of-atoms.


;;==============================================;;


(cl-defstruct (mtg-printing (:constructor mtg-printing--create)
                        (:copier      mtg-printing--copy))


  (multiverseid    nil :type (or null string (integer 0 *)))
  (scryfallid      nil :type (or null string (integer 0 *)))  ;TODO is uuid?


  (uuid            nil :type (or null string))


;;----------------------------------------------;;


(cl-defstruct (mtg-original-printing (:constructor mtg-original-printing--create)
                        (:copier      mtg-original-printing--copy))


  "The original printing of an MTG Card.


Slots:


• DATE      — a ‘stringp’.


• TEXTBOX   — a ‘stringp’. Verbatim textbox. e.g. “Draw 3 cards or force opponent to draw 3 cards.”.


• TYPELINE  — a ‘stringp’. Verbatim typeline. e.g. “Summon Legend”; “Interrupt”.


"


  (name nil :type symbol)
  ;;
  (date     nil :type string)
  (textbox  nil :type string)
  (typeline nil :type string)


  )










;; each ‘mtg-*-name’ is symbolic and immutable, which induces the ‘symbol-name’ of a (global) constant.
;;
;; « (name :type (or null symbol) :read-only t) »
;;
;; 




(name   :type (or null symbol) :read-only t)
(title  :type (or null string))
(abbrev :type (or null symbol))


































(cl-defstruct (cl-structure-class
               (:conc-name cl--struct-class-)
               (:predicate cl--struct-class-p)
               (:constructor nil)
               (:constructor cl--struct-new-class
                (name docstring parents type named slots index-table
                      children-sym tag print))
               (:copier nil))
  "The type of CL structs descriptors."
  ;; The first few fields here are actually inherited from cl--class, but we
  ;; have to define this one before, to break the circularity, so we manually
  ;; list the fields here and later "backpatch" cl--class as the parent.
  ;; BEWARE: Obviously, it's indispensable to keep these two structs in sync!
  (name nil :type symbol)               ;The type name.
  (docstring nil :type string)
  (parents nil :type (list-of cl--class)) ;The included struct.
  (slots nil :type (vector cl-slot-descriptor))
  (index-table nil :type hash-table)
  (tag nil :type symbol) ;Placed in cl-tag-slot.  Holds the struct-class object.
  (type nil :type (memq (vector list)))
  (named nil :type bool)
  (print nil :type bool)
  (children-sym nil :type symbol) ;This sym's value holds the tags of children.
  )






(cl-defstruct (cl--class
               (:constructor nil)
               (:copier nil))
  "Type of descriptors for any kind of structure-like data."
  ;; Intended to be shared between defstruct and defclass.
  (name nil :type symbol)               ;The type name.
  (docstring nil :type string)
  ;; For structs there can only be one parent, but when EIEIO classes inherit
  ;; from cl--class, we'll need this to hold a list.
  (parents nil :type (list-of cl--class))
  (slots nil :type (vector cl-slot-descriptor))
  (index-table nil :type hash-table))










;; e.g. deep copy:


(cl-defstruct (cl-slot-descriptor
               (:copier cl--copy-slot-descriptor-1)
               …)
  …))


(defun cl--copy-slot-descriptor (slot)
  (let ((NEW (cl--copy-slot-descriptor-1 slot)))
    (cl-callf copy-alist (cl--slot-descriptor-props NEW))
    NEW))




















(cl-defstruct (mtg-mana-cost
               (:constructor mtg-mana-cost--create)
               (:copier      nil))


  (name nil)
  ;;
  (mana '() :type (list-of mtg-mana-symbol) :read-only nil)
  )


 :type (list-of mtg-mana-symbol) :read-only nil


x :type () :read-only nil
x


  (specializers nil :read-only t :type list)
  (qualifiers   nil :read-only t :type (list-of atom))
  (uses-cnm     nil :read-only t :type boolean)
  (function     nil :read-only t :type function))
































(defgeneric greet (greeter)
  "Hello.")


;; 


(cl-defstruct dog)


(defmethod greet ((_ dog))
  "Woof!")


;;


(cl-defstruct cow)


(defmethod greet ((_ cow))
  "Moo!")


;;


(cl-defstruct ape)


(defmethod greet ((_ ape)))




















(oref CARD cardtypes)


mtg-card
; a unique card. this type only has fields which are mechanically-relevant, i.e. how the game rules see a card in your deck (in particular, which are universal across printings and languages).
:original-edition ; NOTE, each card may have an edition it was originally printed in, despite not having a “current” or “latest” edition. e.g. ‹City in a Bottle› is a Black-Bordered Card.


:initarg  colors
:initform nil
:type symbol
:documentation "
an ‘mtg-colors-p’:


• nil — Colorless.
• t   — All Colors.


NOTE If you extend the variable ‘mtg-color-alist’, then « t » and  « 'wubrg » will differ. e.g. ‹Sliver Queen› and ‹Transguild Courier› have the exact sane colors in 2019, but wouldn't if MTG gains a sixth color (say, “Purple”). The function ‘mtg-colors-equal’ respects this.


Conceptually, this is a set of colors. Programmatically, they're stored as a single ‘symbolp’ (not a ‘listp’ thereof) for efficiency (symbols have a fast equality comparator in ‘eq’). This field is valid when constructed by ‘make-mtg-card’, which validates and normalizes a set of colors; if you construct or modify this field manually, you must normalize your value via ‘make-mtg-colors’ (or ensure normality via ‘mtg-colors-p’). For example, « (eq (make-mtg-colors '(u g)) (make-mtg-colors '(g u))) » is « t », while both « (eq 'ug 'gu) » and « (equal '(u g) '(g u)) » are « nil »."


mtg-printing
; Silver-Bordered Cards can talk about ‘mtg-printing’ fields (in their Rules Text), while Black-Bordered Cards can talk only about ‘mtg-card’ fields.
:card     mtg-card ; the card which was printed.
:edition
:border 


mtg-printed-card
:card     mtg-card
:printings (list mtg-printing) ; search card has one-or-more printings.


mtg-db
:cards (list mtg-printed-card)
:sets (list mtg-set)
:source string ; the URI (a filepath or URL) which fetched (read or downloaded).
:version string ; the version of the original datasource of this object. NOTE ‘:version’, not ‘:date’, represents the date at which this data's vendor built it.
:date string ; timestamp at which this object was parsed/loaded.






































INTERNATIONALIZATION


;;


(defconst mtg-english-declensions
  `(


elf elves
ox oxen
TODO
   )
  "")




(defvar mtg-declensions
  '((english . mtg-english-declensions)
    )
  "")


;;






























VARIABLES


mtg-base-card-types
mtg-spell-card-types
mtg-permanent-card-types


mtg-subtype-alist




















(defgroup


:link (emacs-commentary-link :tag "Commentary" 'mtg)
:link (url-link :tag "GitHub" "https://github.com/sboosali/mtg.el#readme")


;; ^ Links to the “;;; Commentary:” section of the “mtg.el” library.






 :safe        Set SYMBOL's `safe-local-variable' property to VALUE.
        See Info node `(elisp) File Local Variables'.
:local  If VALUE is t, mark SYMBOL as automatically buffer-local.
        If VALUE is `permanent', also set SYMBOL's `permanent-local'
        property to t.
























(defconst mtg-default-tilde-string "~"
  "Default ‘mtg-tilde-string’.")


defstring mtg-tilde-string mtg-default-tilde-string






























;;; TYPES


TYPES: FORMAT


(defconst mtg-default-formats
)


(defcustom mtg-formats mtg-default-formats


 :type `(set ,@mtg-default-formats)






TYPES: LANGUAGE


(defcustom mtg-preferred-language 'english


 :type `(radio ,@mtg-languages)




(defcustom mtg-preferred-format 'vintage


 :type `(radio ,@mtg-default-formats)


;;


"Greek" "Ελληνικά" 'el
"Vietnamese" "Tiếng Việt" 'vi


;;


[TODO Replace :abbr with :code]


• CODE — a `symbolp`; an *ISO-639 Language Code*.














































ACCESSORS


(define-inline mtg-get-condense-separator ()
  "Return ‘mtg-condense-separator’, a ‘stringp’.
Defaults to “ | ”."
  (let* ((STRING mtg-condense-separator))
    (or (stringp STRING) " | ")))


(define-inline mtg-get-condense-ellipsis ()
  "Return ‘mtg-condense-ellipsis’, a ‘stringp’.
Defaults to “…”."
  (let* ((STRING mtg-condense-ellipsis))
    (or (stringp STRING) "…")))
 
(define-inline mtg-get-condense-width ()
  "Return and validate ‘mtg-condense-width’, a ‘natnump’.
  (let* ((WIDTH mtg-condense-width))
    (or (natnump WIDTH) nil))


(define-inline mtg-condense-width ()
  "Return and validate ‘mtg-condense-width’, a ‘natnump’.
  `(or (natnump mtg-condense-width) nil))


































;;; HASH-TABLES…


;;==============================================;;


;;==============================================;;


(defvar mtg-card-table


  (make-hash-table :test 'eq :size 20000 :rehash-size 1.5)


  "Table of MTG Cards, by their (canonical ized) names.


Type: a ‘hash-table-p’


Accessors:


• ‘mtg-get-card’.
• ‘mtg-intern-card’.")


;;----------------------------------------------;;


(defun mtg-intern-card (card &optional table)


  "Intern CARD by its ‘mtg-card-name’ (into TABLE)


Inputs:


• CARD — an ‘mtg-card-p’.


• TABLE — an optional ‘hash-table-p’.
Defaults to ‘mtg-card-table’.


Output:


• a ‘symbolp’.
The Hash-Key under which CARD is in TABLE.


Effects


• Modifies TABLE."


  (cl-check-type card  'mtg-card)
  (cl-check-type table '(or null hash-table))


  (let* ((NAME-STRING (mtg-card-name card))
         (NAME-SYMBOL (mtg-intern-card-name NAME-STRING))


         (TABLE (or table mtg-card-table)))


    (puthash NAME-SYMBOL card TABLE)))


;;----------------------------------------------;;


(defun mtg-get-card (name &optional table)


  "Get an ‘mtg-card’ by its NAME (from TABLE).


Inputs:


• NAME — a ‘stringp’ or ‘symbolp’.
Corresponds to an ‘mtg-card-name’.


• TABLE — an optional ‘hash-table-p’.
Defaults to ‘mtg-card-table’.


Output:


• an ‘mtg-card-p’.


Notes:


• Many functions which accept card objects (i.e. ‘mtg-card-p’) also accept card names (i.e. ‘stringp’/‘symbolp’).
For example, a card argument can be pre-processed like so:


    (unless (mtg-card-p CARD)
      (setq CARD (mtg-get-card CARD)))
"


  (cl-check-type name  '(or string symbol))
  (cl-check-type table '(or null hash-table))


  (let* ((NAME (cl-typecase name
                 (string (mtg-intern-card-name name))
                 (symbol name)))
         (TABLE (or table mtg-card-table)))


    (gethash NAME TABLE)))


;;==============================================;;


(defvar mtg-edition-table


  (make-hash-table :test 'eq :size 500 :rehash-size 2.0)


  "Table of MTG Editions.


a ‘hash-table-p’.")


;;----------------------------------------------;;


defun mtg-intern-edition (edition)
(intern NAME mtg-edition-hash-table)


;;==============================================;;


(defvar mtg-keyword-table


  (make-hash-table :test 'eq :size 1000 :rehash-size 2.0)


  "Table of MTG Keywords.


a ‘hash-table-p’.")


;;----------------------------------------------;;


defun mtg-intern-keyword (keyword)
(intern NAME mtg-keyword-hash-table)


;;==============================================;;




;;==============================================;;










































































































INTERN & OBARRAY


;;==============================================;;


defvar mtg-card-obarray (make-obarray )


;;----------------------------------------------;;


defun mtg-intern-card (card)
(intern NAME mtg-card-obarray)


;; CARD?
(set-symbol-value NAME card mtg-card-obarray)?


(put NAME 'mtg-card card mtg-card-obarray)?


;;==============================================;;


defvar mtg-edition-obarray (make-obarray )


;;----------------------------------------------;;


defun mtg-intern-edition (edition)
(intern NAME mtg-edition-obarray)


;;==============================================;;


defvar mtg-keyword-obarray (make-obarray )


;;----------------------------------------------;;


defun mtg-intern-keyword (keyword)
(intern NAME mtg-keyword-obarray)


;;==============================================;;






















































































ELECTRIC


;;; Electricity


;; ‘mtg-electric-*’ are used by ‘mtg-mode’ and ‘mtg-query-mode’.


;;


(defvar mtg-electric-mode-map


  (let ((MAP (make-sparse-keymap)))


    (define-key MAP "\n"  #'mtg-electric-insert-newline)
    (define-key MAP "#"  #'mtg-electric-insert-hashtag)
    (cl-loop for (CHAR . _) in mtg-electric-pair-alist
      do (define-key MAP CHAR #'mtg-electric-insert-pair))


    MAP)


  "Keymap for ‘mtg-electric-mode’.")


;; TODO e.g. (define-key MAP "/" #'nxml-electric-slash)


;;


(define-minor-mode mtg-electric-mode


  "Toggles automatic text insertion on certain keys and under certain contexts.


“Electric Keys” (and their behavior) include:


• Braces — See ‘mtg-electric-insert-pair’ and ‘mtg-electric-pair-alist’. Insert a Closing Brace when the user types in an Opening Brace, preserving ‘point’. e.g. « { » becomes « {█} » (where “█” represents the ‘point’).
• Comments — See ‘mtg-electric-insert-hashtag’ and ‘mtg-comment-padding’. Insert spacing after the user types in a Comment Starter. e.g. « # » becomes « # █ ». 
• Return — See ‘mtg-electric-insert-newline’ and ‘mtg-electric-newline’. When the user press ‹RET›, maybe: insert a bullet point, when the user is within a bulleted list; insert a Comment Starter (see above), when the user is within a comment. e.g. « » becomes « • █ ».
• — See ‘mtg-electric-’.
• — See ‘mtg-electric-’.


‘mtg-electric-*’ are used by ‘mtg-mode’ and ‘mtg-query-mode’.
'mtg-electric-insert-hashtag
(comment-indent-new-line)


The command ‘mtg-electric-mode’ toggles electricity (i.e. the variable ‘mtg-electric-mode’) by default; with a positive Prefix Argument, turn electricity on; and with a negative Prefix Argument (or any non-positive-integer), turn electricity off.


Behaves like an ‘electric-pair-local-mode’ (but isn't implemented with ‘elec-pair.el’.)"


  :keymap mtg-electric-mode-map


  :lighter " Electric(MtG)")


  (if (bound-and-true-p mtg-electric-mode)


  (progn ;; Turning ‘mtg-electric-mode’ on...


    ())


  (progn ;; Turning ‘mtg-electric-mode’ off...


    ())))


 (when prolog-electric-dot-flag
    (setq-local electric-indent-chars
                (cons ?\. electric-indent-chars)))


;; electric-indent-chars


;;


(defgroup mtg-electric nil
  :group 'mtg)


;;


(defcustom mtg-electric-


  :type '()


  :safe #'p
  :group 'mtg-electric)


;;


(defun mtg--alist-from-chars-to-chars-or-strings-p (object)
  (cl-typep object 
            '(list (cons character (or character string))))


(defalias 'mtg/alist{char->char|string}? #'mtg--alist-from-characters-to-characters-or-strings-p)


;;TODO use cl-type as predicate: '(list (cons character (or character string)))


;;


(defun mtg--natnum-or-string-p (object)
  (or (natnump object)
      (stringp object)))


(defalias 'mtg/natnum|string? #'mtg--natnum-or-string-p)


;;


(defcustom mtg-comment-padding 1


  "‘comment-padding’ for ‘mtg-mode’


a ‘natnump’ or ‘stringp’.


By default: « 1 », which is equivalent to « “ ” ».")


  :type '(choice (string :tag "Padding") (integer :tag "How many spaces"))
  :safe #'mtg/natnum|string?


  :group 'mtg-electric)


;;


(defcustom mtg-electric-pair-alist


 `(


   ;; Brackets:


   (?{  . ?})
   (?\( . ?\)
   (?\[ . ?\])
   (?<  . ?>)


   ;; Quotations:


   (?“  . ?”)
   (?«  . ?»)
   (?„  . ?‟)
   (?「  . ?」)


   ;;(?  . ?)
)


  "“Electric Pairs” for ‘mtg-electric-mode’.


Associates a ‘characterp’ with either a ‘characterp’ or a ‘stringp’. Pressing a ‘car’ inserts its ‘cdr’ too."


  :type '(alist :key-type (string :tag "Electric Key") :value-type (choice (character) (string)))


  :set        #'mtg-custom-set
  :initialize #'custom-initialize-default


  :safe #'mtg/alist{char->char|string}?
  :group 'mtg-electric)


(defcustom mtg-electric-newline


  `()


  :type '()


  :safe #'listp
  :group 'mtg-electric)






define-derived-mode mtg-query-mode
"‘mtg-query-mode’ enables “Electricity” by default."
  (mtg-electric-mode +1)


;;


(defconst mtg--electric-commands
(list #'mtg-electric-insert-pair 
#'mtg-electric-insert-newline
#'mtg-electric-insert-hashtag)
  "")


;;


(defconst mtg--electric-keys
'(?\n ?#)
"")


;;


(defun mtg--electric-keys ()
  "Return all Electric Keys."
  (cl-loop for (CHAR . _) in mtg-electric-key-alist ;TODO mtg-electric-key-alist or mtg--electric-key-char-table?
    collect CHAR into KEYS
    finally return (append mtg--electric-keys KEYS)))


;;


(defun mtg--electric-command-p (symbol)
  "Whether SYMBOL is an Electric Insertion command."
  (memq symbol mtg--electric-commands))


;;


(defun mtg--electric-key-p (key)
  "Whether KEY is an Electric Key."
  (or (memq key mtg--electric-keys))
      (char-table-range key mtg--electric-pairs-char-table)))


;;  (assq key mtg-electric-pair-alist)


;;


(defun mtg-electric-insert-pair (&optional char)


  "‘insert’ CHAR, electrically if ‘mtg-electric-mode’ is on.


Inputs:


• CHAR — a ‘characterp’.
Defaults to the key currently being pressed.


Related:


• Looks CHAR up in ‘mtg--electric-pairs-char-table’."


  (let ((OPEN-CHAR (or char
 (cl-loop for EVENT across (this-command-keys-vector) with CHAR = (and (characterp EVENT) (>= EVENT 32) EVENT) if CHAR return CHAR)))) ; the first event that's a key.


  (if (not mtg-electric-mode)
      (insert OPEN-CHAR) ;TODO or (self-insert-command)


  (let ((CLOSE-CHARS (char-table-range mtg--electric-pairs-char-table OPEN-CHAR)))


  (if (not CLOSE-CHARS)
      (insert OPEN-CHAR) ;TODO or (self-insert-command)


   (insert OPEN-CHAR CLOSE-CHARS)
   (when-let* ((LENGTH (cl-typecase CLOSE-CHARS
(string (length CLOSE-CHARS))
(char   1))))
      (backward-char LENGTH))))))


;;


mtg-electric-insert-brace


  "‘insert’ “{”/“(”/“"”/“«”/“„”/etc,electrically if ‘mtg-electric-mode’ is on."


;;


mtg-electric-insert-newline


  "‘insert’ “\n”, electrically if ‘mtg-electric-mode’ is on."


;;


(defun mtg-electric-insert-hashtag ()


  "‘insert’ “#”, electrically if ‘mtg-electric-mode’ is on."


  (interactive "*")




  (save-excursion
    (if (bolp)
        (insert "# ")
     (insert "#"))))


;;


;;




;;




(defvar mtg--electric-pairs-char-table


  (mtg--rebuild-electric-pairs-char-table mtg-electric-pair-alist)


  "Character-Table for Electric Keys.
  
a ‘char-table-p’.


‘mtg--electric-pairs-char-table’ is the (optimized) internal variable of the (customizeable) external variable ‘mtg-electric-pair-alist’.")


;;


(defun mtg--reset-electric-pairs-char-table (&optional alist)


  "(Re-)Set ‘mtg--electric-pairs-char-table’.


Calls ‘mtg--rebuild-electric-pairs-char-table’ on ALIST (‘mtg-electric-pair-alist’ by default)."


  (let* ((CHAR-TABLE (mtg--rebuild-electric-pairs-char-table ALIST)))


  (setq mtg--electric-pairs-char-table CHAR-TABLE)))


;;


(defun mtg--rebuild-electric-pairs-char-table (&optional alist)


  "(Re-)Build ‘mtg--electric-pairs-char-table’ (a ‘char-table-p’) from ‘mtg-electric-pair-alist’ (or ALIST)."


  (let* ((ALIST (or alist mtg-electric-pair-alist))
         (CHAR-TABLE (make-char-table nil)))


    (cl-loop for (OPEN-CHAR . CLOSE-CHARS) in ALIST
      do (set-char-table-range CHAR-TABLE OPEN-CHAR CLOSE-CHARS))))


;;


(defun mtg--rebind-electric-pairs (&optional keymap alist)


  "(Re-)Bind Electric Keys in KEYMAP selon ALIST.


• KEYMAP defaults to variable ‘local-map’ (e.g. ‘mtg-mode-map’).


• ALIST defaults to ‘mtg-electric-pair-alist’."


  (let* ((KEYMAP (or keymap local-map))
         (ALIST  (or alist mtg-electric-pair-alist)))


    (cl-loop for (CHAR . _) in ALIST do (define-key KEYMAP CHAR  #'mtg-electric-insert-pair))))


;;


(defun mtg--unbind-electric-pairs (&optional keymap alist)


  "Unbind any Electric Keys in KEYMAP.


• KEYMAP defaults to variable ‘local-map’ (e.g. ‘mtg-mode-map’).


• ALIST defaults to ‘mtg-electric-pair-alist’.


NOTE “unbinding” means restoring ‘self-insert-command’, not nil."


  (let* ((KEYMAP (or keymap local-map))
         (ALIST  (or alist mtg-electric-pair-alist)))


    (cl-loop for (CHAR . _) in ALIST do (define-key KEYMAP CHAR #'self-insert-command))))


;;


;;


(defvar mtg-mode-map …)
(define-key MAP "\n"  #'mtg-electric-newline)
(define-key MAP [remap newline] #'mtg-newline)
(define-key MAP [remap newline-and-indent] #'mtg-newline-and-indent)


;; TODO implement:
(defalias 'mtg-electric-newline #'newline)




;; TODO move this variable to the define minor mode declaration:


(defcustom mtg-electric-mode nil


  "


a ‘booleanp’.


Non-nil means automatically closing requests when you insert an open."


  :type 'boolean
  :safe #'booleanp
  :group 'mtg)








(defun mtg-electric-newline (arg)


  "‘insert’ a newline for ‘mtg-mode’, “electrically”.


  (interactive "*P")  ; ‹*› means writable buffers only.
                      ; ‹P› means a raw prefix argument.


  ())






(defun mtg-newline ()


  "‘insert’ a newline for ‘mtg-mode’, “electrically”.


If ‘mtg-electric mode’.
In `mtg-electric-mode', if ending a line containing an mtg opening request,
automatically inserts the matching closing request after point."


  (interactive "*")  ; writable buffers only.


  (if electric-indent-mode
      (electric-indent-just-newline nil)
    (newline-and-indent)))










(define-key menu-map [ne]
      '(menu-item "Electric newline mode"
                  mtg-electric-mode
                  :help "Auto insert closing requests if necessary"
                  :button (:toggle . mtg-electric-mode)))


(defun mtg-electric-newline (arg)
  "Insert newline for mtg mode; special if mtg-electric mode.
In `mtg-electric-mode', if ending a line containing an mtg opening request,
automatically inserts the matching closing request after point."
  (interactive "P")






(defun mtg-electric-newline (arg)
  "Insert newline for mtg mode; special if mtg-electric mode.
In `mtg-electric-mode', if ending a line containing an mtg opening request,
automatically inserts the matching closing request after point."
  (interactive "P")
  (let ((completion (save-excursion
                      (beginning-of-line)
                      (and (null arg)
                           mtg-electric-mode
                           (<= (point) (- (point-max) 3))
                           (cdr (assoc (buffer-substring (point)
                                                         (+ 3 (point)))
                                       mtg-brace-table)))))
        (needs-nl (not (looking-at "[ \t]*$"))))
    (if (null completion)
        (newline (prefix-numeric-value arg))
      (save-excursion
        (insert "\n\n" completion)
        (if needs-nl (insert "\n")))
      (forward-char 1))))






















mtg-electric-mode




newline-and-bullet


e.g. entering:


    Choose one —█⏎


becomes:


    Choose one —
    • █


where « █ » represents the ‘point’
and « ⏎ » represents pressing ‘newline’ (a.k.a. RET).






mtg-electric-return








(defun markdown-enter-key ()


  "Handle RET depending on the context.
If the point is at a table, move to the next row.  Otherwise,
indent according to value of `markdown-indent-on-enter'.
When it is nil, simply call `newline'.  Otherwise, indent the next line
following RET using `markdown-indent-line'.  Furthermore, when it
is set to 'indent-and-new-item and the point is in a list item,
start a new item with the same indentation. If the point is in an
empty list item, remove it (so that pressing RET twice when in a
list simply adds a blank line)."


  (interactive "*")


  (cond


   ;; Table
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-next-row))


   ;; Indent non-table text
   (markdown-indent-on-enter
    (let (bounds)
      (if (and (memq markdown-indent-on-enter '(indent-and-new-item))
               (setq bounds (markdown-cur-list-item-bounds)))
          (let ((beg (cl-first bounds))
                (end (cl-second bounds))
                (length (cl-fourth bounds)))
            ;; Point is in a list item
            (if (= (- end beg) length)
                ;; Delete blank list
                (progn
                  (delete-region beg end)
                  (newline)
                  (markdown-indent-line))
              (call-interactively #'markdown-insert-list-item)))
        ;; Point is not in a list
        (newline)
        (markdown-indent-line))))


   ;; Insert a raw newline
   (t (newline))))








(defun markdown-insert-list-item (&optional arg)


  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.


  (interactive "*")


  ())


























FUNCTIONS




;;


(defconst mtg-english-title-syntax-table


  (let ((TABLE (copy-syntax-table text-mode-syntax-table))


    (dolist (CHAR '(?\' (?a . ?z) (?A . ?Z)))
      (modify-syntax-entry CHAR "w" TABLE))


    (dolist (CHAR '(?-))
      (modify-syntax-entry CHAR "_" TABLE))


     TABLE)


  "Syntax-Table for “titles” like MTG Card Names. 


A ‘syntax-table-p’.")


;;


(defias 'mtg-titlecase-normalize-string #'mtg-normalize-string)


;;


(defcustom mtg-titlecase-english-uncapitalized-word-blacklist


  '("a" "ago" "an" "and" "as" "at" "but" "by" "for"


   ;; prepositions:


   "from" "in" "into" "it" "next" "nor" "of" "off" "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"


  ;; fragments:


  "n" "t" "es" "s"


  )


  "Words always lowercased by ‘mtg-titlecase’.


A ‘listp’ of ‘stringp’s;
a set of case-insensitive word-syntax strings.


Includes:


• articles (e.g. “the”, “a”, “an”)
• coordinate conjunctions (e.g. “and”, “or”, “nor”)
• prepositions (e.g. “”)
• fragments


 (Note: NIVA prefers to capitalize prepositions of five characters or more (“after”, “among”, “between”).)
"


  :type '(repeat (string))
  :safe #'listp


  :group 'mtg)


;;


(defcustom mtg-titlecase-english-capitalized-word-length-whitelist '()


  "Words always capitalized by ‘mtg-titlecase’.


A ‘listp’ of ‘stringp’s;
a set of case-insensitive word-syntax strings.


Most words are capitalized by ‘mtg-titlecase’ already: the ‘mtg-titlecase-english-capitalized-word-length-whitelist’ whitelist trumps the ‘mtg-titlecase-english-uncapitalized-word-blacklist’ blacklist.


‘mtg-titlecase-english-capitalized-word-length-threshold’ is an “open”/predicate version of this."


  :type '(repeat (string))
  :safe #'listp


  :group 'mtg)


;;


(defcustom mtg-titlecase-english-capitalized-word-length-threshold 5


  "Length at-or-above which ‘mtg-titlecase’ always capitalizes words.


A ‘natnump’. 
This natural is compared against the ‘length’ of a word.
Defaults to 5, because: “NIVA prefers to capitalize prepositions of five characters or more.”


To disable such whitelisting, set to nil. To disable blacklisting, set to 0 (or 1).


Affects:


• prepositions (e.g. “after”, “among”, “between”)


‘mtg-titlecase-english-capitalized-word-length-whitelist’ is an “closed”/constant version of this."


  :type 'integer
  :safe #'natnump


  :group 'mtg)


;;


(defcustom mtg-titlecase-english-contraction-word-suffixes 


  '("n" "t" "es" "s")


  "




  :type '(repeat (string))
  :safe #'listp


  :group 'mtg)


;;


(defcustom mtg-titlecase-english-prepositions


aboard
about
above
absent (law)
across
cross (archaic)
after
against
'gainst, gainst (poetic or archaic); again, gain (archaic)
along
'long (abbreviation), alongst (archaic)
alongside
amid
amidst, mid, midst (poetic or archaic);
among
amongst (in US English poetic or archaic)
'mong, mong, 'mongst (abbreviations)
apropos (rare for apropos of)
apud (formal)
around
'round, round (abbreviations)
as
astride
at
@ (abbreviation)
atop, ontop
bar
before
afore, tofore (dialectal or archaic)
B4 (abbreviation)
behind
ahind (dialectal or archaic)
below
ablow, allow (Scotland)
beneath
'neath, neath (poetic)
beside
besides
between
atween (dialectal or archaic)
beyond
ayond (dialectal or archaic)
but
by
chez (rare)
circa
c., ca. (abbreviations)
come
dehors (law)
despite
spite (abbreviation)
down
during
except
for
4 (abbreviation)
from
in
inside
into
less
like
minus
near
nearer (comparative), nearest (superlative)
anear (archaic)
notwithstanding (also postpositional)
of
o' (poetic or eye-dialect)
off
on
onto
opposite
out
outen (archaic or dialectal)
outside
over
o'er (poetic)
pace (formal)
past
per
plus
post (often hyphenated)
pre (often hyphenated)
pro (often hyphenated)
qua (formal)
re (often used with colon)
sans (formal)
save
sauf (archaic)
short
since
sithence (archaic)
than
through
thru (abbreviation)
throughout
thruout (abbreviation)
till
to
2 (abbreviation)
toward, towards
under
underneath
unlike
until
'til, til (abbreviations)
unto (obsolete, poetic)
up
upon
'pon, pon (abbreviations)
upside
versus
vs., v. (abbreviations)
via
vice (formal)
vis-à-vis (formal)
with
w/, wi', c̄ (abbreviations)
within
w/i (abbreviation)
without
w/o (abbreviation)
worth


  "English preposition words.


A ‘listp’ of ‘stringp’s."


  :type '(repeat (string))
  :safe #'listp


  :group 'mtg)


;;


(defconst mtg-titlecase-english-multiword-prepositions        


according to
across from
adjacent to
ahead of
along with
apart from
as for
as of
as per
as regards
aside from
back to
because of
close to
counter to
down on
due to
except for
far from
inside of
instead of
left of
near to
next to
opposite of
opposite to
other than
out from
out of
outside of
owing to
prior to
pursuant to
rather than
regardless of
right of
subsequent to
such as
thanks to
up to
based on


as far as 
as opposed to
as soon as
as well as


at the behest of
by means of
by virtue of
for the sake of
for lack of
for want of
in accordance with
in addition to
in case of
in front of
in lieu of
in place of
in point of
in spite of
on account of
on behalf of
on top of
with regard to (sometimes written as "w/r/t")
with respect to
with a view to


"")


;;


(defun mtg-titlecase (beg end &key language)


  "Title-Case the current region, or current line, w.r.t. the orthography of LANGUAGE.


‘mtg-titlecase’ is a “DWIM” (Do-What-I-Mean) ‘commandp’.


=== Input/Output ===


Inputs:


• BEG — an ‘integer-or-marker-p’.
Defaults to ‘region-beginning’ or ‘line-beginning-position’.


• END — an ‘integer-or-marker-p’.
Defaults to ‘region-end’ or ‘line-end-position’.


• LANGUAGE — a ‘symbolp’.
Defaults to symbol ‘english’.
[TODO support all ‘mtg-languages’]


=== English ===


Effects, for the English LANGUAGE:


• Always capitalize the first word and the last word (this rule trumps all rules below).


• Capitalize all nouns (e.g. “fish”), pronouns (e.g. “it”, “she”), adjectives (e.g. “fishy”), verbs (e.g. “betrays”, “hungers”), adverbs (e.g. “”), subordinate conjunctions (e.g. “as”, “because”, “although”, “if”), and long prepositions (e.g. “after”, “among”, “between”; see ‘mtg-titlecase-english-capitalized-word-length-threshold’).


• Lowercase all articles (e.g. “the”, “a”, “an”), coordinate conjunctions (e.g. “and”, “or”, “nor”), short prepositions (e.g. “into”), and fragments (e.g. “s” in “it's”). See ‘mtg-titlecase-english-uncapitalized-word-blacklist’.


• Lowercase infinite-“to”.


TODO (e.g. “that”, “who”).


where:


• Capitalization — is performed by function ‘capitalize-word’.
• Lowercasing — is performed by function ‘downcase-word’.


=== Examples ===


‘mtg-titlecase-string’ examples:


(mtg- \"the abyss\") 
;; ↪ “The Abyss”


(mtg- \"THE ABYSS\") 
;; ↪ “The Abyss”


(mtg- \"magus of the abyss\")
;; ↪ “Magus of the Abyss”


(mtg- \"it that betrays\") 
;; ↪ “It That Betrays”


(mtg- \"he who hungers\") 
;; ↪ “He Who Hungers”


(mtg- \"it that rides as one\") 
;; ↪ “It That Rides as One”


(mtg- \"ayula, queen among bears\") 
;; ↪ “Ayula, Queen Among Bears”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


(mtg- \"\") 
;; ↪ “”


=== Usage ===


Used:


• for properly title-casing ‘mtg-card-name’s.


Usage:


• M-x mtg-titlecase


• M-: (call-interactively #'mtg-titlecase)


• M-: (mtg-titlecase (region-beginning) (end-of-line))
  ;; ^ Current (highlighted) region.


• M-: (mtg-titlecase (mark) (point))
  ;; ^ Current (implicit) region.


• M-: (mtg-titlecase (beginning-of-line) (region-end))
  ;; ^ Current line.


• M-: (mtg-titlecase (point-min) (point-max))
  ;; ^ Whole buffer.


=== Related ===


• ‘mtg-titlecase’ wraps ‘mtg-titlecase-string’.
• ‘mtg-titlecase-string’ is text-based where ‘mtg-titlecase’/‘mtg-titlecase-region’ are buffer-based. 


=== Links ===


URL ‘https://karl-voit.at/2015/05/25/elisp-title-capitalization/’"


  (interactive "*")


  (let ((BEG (or beg
                 (if (use-region-p) (region-beginning) (line-beginning-position))))


        (END (or end
                 (if (use-region-p) (region-end) (line-end-position))))


        (LANGUAGE (or language)))


    (cl-check-type BEG integer-or-marker)
    (cl-check-type END integer-or-marker)


    (let ((BEG (min BEG END))
          (END (max END BEG))


      (mtg-titlecase-region BEG END :language LANGUAGE))))


;;


(defun mtg-titlecase-string (text &key language)


  "Title-Case TEXT in LANGUAGE.


Input(s):


• TEXT — a ‘stringp’.


Output:


• a ‘stringp’.


(See ‘mtg-titlecase’ for documentation.)"


  (with-temp-buffer
    (insert text)
    (mtg-titlecase-region (point-min) (point-max) :language language)
    (buffer-string)))


;;


(cl-defun mtg-titlecase-region (beg end &key language)


  "Title-Case text between END and BEG, w.r.t. the conventions of LANGUAGE.


(See ‘mtg-titlecase’ for documentation.)"


  (let* ((LOWERCASE-WORDS mtg-titlecase-english-uncapitalized-word-blacklist)
         )


    (save-excursion


      (goto-char BEG)


      (let ((WORD (thing-at-point 'word)))
        (when (mtg-titlecase-english-should-capitalize-first-word-p WORD)
          (capitalize-word +1))


      ;; ^ Always capitalize the first word,
      ;;   except prefixes like “d'”, or “il-” / “en-”.


      (while (< (point) END)


        (skip-syntax-forward "^w" END)


        ;; ^ “^w” means a character without word-syntax.


;;^TODO replace ‘skip-syntax-forward’ with explicit regexp for whitespace & separators (i.e. “-”, “'”, “.”, “//”, etc).


        (let ((WORD (thing-at-point 'word)))


          (if (stringp WORD)


              (if (mtg-titlecase-english-should-capitalize-word-p WORD)
                  (capitalize-word +1)
                (downcase-word +1)))))


      (skip-syntax-backward "w" BEG)


;;^TODO ‘backward-word’ doesn't work in ‘superword-mode’?


      (let ((WORD (thing-at-point 'word)))


        (when (and (>= (point) BEG)
                   (mtg-titlecase-english-should-capitalize-last-word-p WORD))
          (capitalize-word +1)))


      ;; ^ Always capitalize the last word,
      ;;   except suffixes like “'s”.


       t))))


;;


(cl-defun mtg-titlecase-english-should-capitalize-word-p (word &key (normalize t))


  "Whether WORD should be capitalized, i.e. not downcased.


Input(s):


• WORD — a ‘stringp’.


• NORMALIZE — an optional ‘booleanp’.
Defaults to t.
Whether to normalize WORD before checking it (if non-nil), via ‘mtg-titlecase-normalize-string’;
or whether to pass it through “raw” to each check (if nil).


Output:


• a ‘booleanp’.
(NOTE: may return some non-nil ‘objectp’ rather than t.)"


  (let* ((WORD (if normalize (mtg-titlecase-normalize-string word) word)))


    (or (when mtg-titlecase-english-capitalized-word-length-threshold
          (>= (length WORD) mtg-titlecase-english-capitalized-word-length-threshold))


        (member WORD mtg-titlecase-english-capitalized-word-whitelist)


        (not (member WORD mtg-titlecase-english-uncapitalized-word-blacklist)))))


;; e.g. “D'Avenant Healer”
;; e.g. “Man-o'-War”
;; e.g. “Nevinyrral's Disk”
;; e.g. “Evil Eye of Orms-by-Gore”
;; e.g. “Varchild's War-Riders”


;;


TODO (mtg-titlecase-english-should-capitalize-first-word-p)


;; TODO (not (member (or (thing-at-point 'word t) "s") '("o" "d" "il" "en"))))


;; e.g. “D'Avenant Healer” vs “”.
;; e.g. “”


;;


TODO (mtg-titlecase-english-should-capitalize-last-word-p)


;; TODO (not (member (or (thing-at-point 'word t) "s") '("n" "t" "es" "s"))))


;; e.g. “”
;; e.g. “”


;;
;;


(defalias 'mtg-custom-initialize #'custom-initialize-default)


;; ^ Usage:
;;
;; (defcustom …
;;   :set        #'mtg-custom-set
;;   :initialize #'mtg-custom-initialize
;;   …)
;;
;; When ‘defcustom:set’ is explicitly specified, ‘defcustom:initialize’ gets implicitly set as ‘defcustom:set’ too.
;; By explicitly specifying ‘defcustom:initialize’ as the default value of ‘defcustom:initialize’ (i.e. when unspecified), we can override ‘defcustom:set’ without overriding ‘defcustom:initialize’.
;; 


;;


(defun mtg-custom-set (variable value)


  "Set VARIABLE to VALUE, then maybe call a handler for VARIABLE.


Usage:


    (defcustom …
      :set #'mtg-custom-set)


Handles some customization variables in customization group ‘mtg’ specially.
For example, whenever certain customizable external variables (e.g. of type alist) are changed, update the (corresponding) optimized internal variables (e.g. of type ‘hash-table-p’ or ‘hash-table-p’)."


  (set variable value)


  (cl-case variable


    ('mtg-electric-pair-alist (mtg--reset-electric-pairs-char-table value))


  ;;('mtg- (mtg-))


))


;;


(defun mtg-condense-text (text &optional width)


  "Condense TEXT, truncating to WIDTH.


Condense while preserving as much information as possible.


Inputs:


• TEXT  — a `stringp'.
• WIDTH — an optional `integerp' 
  Defaults to nil (a.k.a. ∞, i.e. no maximum width).


Transformations include:


• Replace substrings with abbreviations (via ‘mtg-abbreviate-text’). e.g. (mtg-abbreviate-text "converted mana cost") evaluates to "cmc".


• Replace substrings with symbols(via ‘mtg-symbolize-text’).


• condense to a `string-width' of WIDTH, with “…” (i.e. ellipses).


Configuration:


• ‘mtg-condense-separator’ — 


Related:


• Calls ‘mtg-symbolize-text’.
• Calls ‘mtg-abbreviate-text’.
• Calls ‘truncate-string-to-width’."


  (let* ((WIDTH (mtg-get-condense-width))


         (ELLIPSIS (mtg-get-condense-ellipsis))


         (SEPARATOR (mtg-get-condense-separator)) ;TODO make sure accessors work with dynamicscope


         (SEPARATOR* (propertize SEPARATOR 'face 'mtg-table-text-separator))


         (TEXT-ONELINE (string-join (split-string (string-trim text) "[\f\n\r\v]+" :omit-nulls) SEPARATOR))


         (TEXT-CONDENSED (if WIDTH
              (let ((PADDING  nil)
                    (COLUMN-END WIDTH)
                    (COLUMN-BEG 0)
                    )
                (truncate-string-to-width TEXT-ONELINE COLUMN-END COLUMN-BEG PADDING ELLIPSIS))
            TEXT-ONELINE))
         )


    TEXT-CONDENSED)


;;


(defun mtg-condense-card (card &optional width)


  "Return a copy of CARD, condensing textual slots (via ‘mtg-condense-text’).


Inputs:


• CARD  — an `mtg-card-p'.
• WIDTH — an optional `integerp' 
  Defaults to nil (i.e. no maximum width).


"


  ())






;;






















;;


(define-inline mtg-resolve-colors (colors)


  "Resolve COLORS against ‘mtg-colors’.


=== Examples ===


(mtg-resolve-colors nil)
nil


(mtg-resolve-colors t)
'wubrg


(mtg-resolve-colors 'wubrg)
'wubrg
"


(inline-letevals (colors)


  (inline-quote (if (eq t colors)
      (make-mtg-colors mtg-colors)
    colors))))


;;


(define-inline mtg-colors-equal (colors1 colors2)


  "Whether COLORS1 and COLORS2 are equivalent ‘mtg-color’s.


Laws:


"


  (inline-letevals (colors1 colors2)


  (inline-quote (or (eq colors1 colors2)
      (let ((COLORS1 ,(mtg-resolve-colors colors1))
            (COLORS2 ,(mtg-resolve-colors colors2)))
        (eq COLORS1 COLORS2))))))


;; ^ the first check has no false positives, but may have false negatives (e.g. ‘t’ and ‘wubrg’ are conditionally equivalent, but not symbolically equal).
;; the first check is done for efficiency (so that comparing “normal” colors is not slowed down by comparing “abnormal” colors like ‘t’). Note that « (or t …) » is « t ».
;; the second check is done for correctness.
;;


;;


(define-inline mtg-numerals-equal (numerals1 numerals2)


  "Whether NUMERALS1 and NUMERALS2 are equivalent ‘mtg-numeral’s.


=== Type ===


Inputs:


• NUMERALS1 — an ‘mtg-numeral-p’.
• NUMERALS2 — an ‘mtg-numeral-p’.


Output:


• a ‘booleanp’.


=== Examples ===


M-: (mtg-numerals-equal "1/2" "2/4")
 → nil


M-: (mtg-numerals-equal "*/4" "0/4")
 → nil


=== Laws ===


"


  (inline-letevals (numerals1 numerals2)


  (inline-quote (or (eq numerals1 numerals2)
      (let ((NUMERALS1 ,(identity numerals1))
            (NUMERALS2 ,(identity numerals2)))
        (eq NUMERALS1 NUMERALS2))))))


;; ^ 


;;


mtg-colors-p


;;




(defun mtg-spell-card-type-p (card-type)
  "Whether CARD-TYPE is a “spell”.
e.g. symbol ‘sorcery’."
  (memq card-type mtg-spell-card-types))


;;


(defun mtg-permanent-card-type-p (card-type)
  "Whether CARD-TYPE is a “permanent”.
e.g. symbol ‘creature’."
  (memq card-type mtg-permanent-card-types))


;;


(defun mtg-base-card-type-p (card-type)
  "Whether CARD-TYPE is a “base type”.
e.g. not symbol ‘tribal’."
  (memq card-type mtg-base-card-types))


;;


































;;; Abbreviations…


;;==============================================;;


(cl-deftype mtg-card-predicate ()


  `(or function
       (list-of (or mtg-card string))
       symbol)


  "Type representing a set of ‘mtg-card’s.


‘mtg-card-predicate’ values can be:


• Card Names — a ‘listp’ of ‘stringp’s.
A “closed set” of cards, explicitly 


• Card Predicate — a ‘functionp’ from ‘mtg-card-p’ to ‘booleanp’.
An “open set” of cards, induced 


")


;;==============================================;;


(defcustom mtg-card-nicknames-alist


  `(("Academy" . "Tolarian Academy")
    ("Ancestral" . "Ancestral Recall")
    ("AK" . "Accumulated Knowledge")


    ("REB" . "Red Elemental Blast")
    ("Bob" . "Dark Confidant")
    ("Bolt" . "Lightning Bolt")
    ("CoB" . "City of Brass")
    ("CoP" . "Circle of Protection")
    ("Drain" . "Mana Drain")
    ("DRS" . "Deathrite Shaman")
    ("Dude Ranch" . "Kjeldoran Outpost")
    ("E-wit" . "Eternal Witness")
    ("Edict" . ("Diabolic Edict" "Chainer's Edict" "Cruel Edict"))
    ("Finkel" . "Shadowmage Infiltrator")
    ("FoW" . "Force of Will")
    ("Force" . "Force of Will")
    ("Goyf" . "Tarmogoyf")
    ("Hymn" . "Hymn to Tourach")
    ("Keg" . "Powder Keg")
    ("Birds" . "Birds of Paradise")
    ("Necro" . "Necropotence")
    ("Oath" . "Oath of Druids")
    ("Path" . "Path to Exile")
    ("Primetime" . "Primeval Titan")
    ("Ritual" . "Dark Ritual")
    ("SFM" . "Stoneforge Mystic")
    ("Stick" . "Isochron Scepter")
    ("Swords" . "Swords to Plowshares")
    ("Tim" . "Prodigal Sorcerer")
    ("‘tog" . "Psychatog")
    ("Walk" . "Time Walk")
    ("Welder" . "Goblin Welder")
    ("WoG" . "Wrath of God")
    ("Bargain" . "Yawgmoth's Bargain")
    ("Will" . "Yawgmoth's Will")
    ("YawgWill" . "Yawgmoth's Will")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("" . "")
    ("Trop" . "Tropical Island")
    ("Vulc" . "Volcanic Island")
    ("Misty" . "Misty Rainforest")


;;  ("" . '(""))
   )


  "Knicknames for sets-of-cards (as a whole).


URL `https://mtg.gamepedia.com/List_of_Magic_slang'")


  :type '(alist :key-type   (string :tag "Nickname")
                :value-type (or string (repeat string) :tag "Name(s)"))


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defcustom mtg-card-group-nicknames-alist


  `(("Urzatron" .  '("Urza's Power Plant" "Urza's Mine" "Urza's Tower"))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))
    ("" . '("" ""))


;;  ("" . '("" ""))
   )


  "Knicknames for groups-of-cards (as a whole).


e.g. « (\"Urzatron\" . '(\"Urza's Power Plant\" \"Urza's Mine\" \"Urza's Tower\")) » means that “Urzatron” is the knickname for the three-card group of “Urza's Power Plant” & “Urza's Mine” & “Urza's Tower”.




URL `https://mtg.gamepedia.com/List_of_Magic_slang'")


  :type '(alist :key-type   (string :tag "Nickname")
                :value-type (repeat string :tag "Names"))


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defcustom mtg-card-kind-nicknames-alist


  `(("Fetch" . "")


  "Knicknames for kinds of cards.


Such kinds are defined either explicitly, as a set (a ‘listp’), or implicitly, as a predicate (a ‘functionp’).


(“Kinds” meaning unofficial groupings, which aren't mutually exclusive (c.f. ‘mtg-edition’s, which are for the original printing of a card, but aren't for reprintings of a card).)


e.g. 


=== Type ===


The ‘cdr’ of each ‘consp’ is a valid ‘mtg-card-predicate’ (which see), i.e. either:


• Card Names — a ‘listp’ of ‘stringp’s.
A “closed set” of cards, explicitly 


• Card Predicate — a ‘functionp’ from ‘mtg-card-p’ to ‘booleanp’.
An “open set” of cards, induced 


• Variable — a ‘symbolp’ whose ‘symbol-value’ is itself an ‘mtg-card-predicate’ (recursive).


=== Links ===


URL `https://mtg.gamepedia.com/List_of_Magic_slang'")


  :type '(alist :key-type   (string :tag "Nickname")
                :value-type (or (repeat string :tag "Card Names") (function :tag "Card Predicate")))


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defconst mtg-unicode-alist


  `(((rx bos "--" eos) . "—") 
    ((rx bos "*"  eos) . "•")


    ((rx bow "Ae") . "Æ")


    ((rx "’") . "'")
    ((rx "“") . "\"")
    ((rx "”") . "\"")


;;    ((rx "") . "")
   )


  ".


---


e.g. Aether Vial:


- “Æther Vial” from the Kaladesh Masterpieces.
- “Aether Vial” from Darksteel (the original printing).


---


e.g. a Charm:


    Choose one —
    • 
    •


    Choose one --
    * 
    *


---


e.g. Boomerang:


    Return target permanent to its owner’s hand.


    Return target permanent to its owner's hand.


---


e.g. Llanowar Mentor:


    Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”


    Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has "{T}: Add {G}."


---"


  :type '(alist :key-type   (regexp)
                :value-type (string))


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


;;==============================================;;


;;TODO M-% mtg-abbrev-alist mtg-abbreviation-alist


(defcustom mtg-abbreviation-alist


    `(("cmc"    . "converted mana cost")


      ("etb"    . "enters the battlefield")
      ("ltb"    . "leaves the battlefield")


      ("ltoet"  . "less than or equal to")
      ("gtoet"  . "greater than or equal to")


      ("eot"   . "end of turn")


      ("atboyu" . "at the beginning of your upkeep")
      )


  "MTG Abbreviations.


Each ‘consp’ in ‘mtg-abbreviation-alist’ is equivalent to that entry being both in ‘mtg-expansion-alist’ and (modulo swapping) in ‘mtg-contraction-alist’.
For example, registering this “abbreviation”:


    (add-to-list 'mtg-abbreviation-alist '("cmc" . "converted mana cost"))


is equivalent to registering this “expansion” plus this “contraction”:


    (add-to-list 'mtg-expansion-alist '("cmc" . "converted mana cost"))
    (add-to-list 'mtg-contraction-alist '("converted mana cost" . "cmc"))


URL `https://mtg.gamepedia.com/List_of_Magic_slang'"


  :type '(alist :key-type   (string :tag "Abbreviation")
                :value-type (string :tag "Expansion"))


  :set        #'mtg-custom-set
  :initialize #'mtg-custom-initialize


  ;;TODO :modify mtg-mode--custom-set to update mtg-mode-abbrev-table


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defcustom mtg-contraction-alist


  `(
    )


  "MTG Contraction-only Abbreviations."


  :type '(alist :key-type   (string)
                :value-type (string :tag "Abbreviation"))


  :set        #'mtg-custom-set
  :initialize #'mtg-custom-initialize


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defcustom mtg-expansion-alist


  `(("ueot"   . "until end of turn")
    )


  "MTG Expansion-only Abbreviations."


  :type '(alist :key-type   (string :tag "Abbreviation")
                :value-type (string :tag "Expansion"))


  :set        #'mtg-custom-set
  :initialize #'mtg-custom-initialize


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defun mtg-expansion-alist ()
  "Accessor for ‘mtg-abbreviation-alist’ & ‘mtg-expansion-alist’."
  (append mtg-expansion-alist mtg-abbreviation-alist))
 
;;----------------------------------------------;;


(defun mtg-contraction-alist ()
  "Accessor for ‘mtg-abbreviation-alist’ & ‘mtg-contraction-alist’."
  (append mtg-contraction-alist mtg-abbreviation-alist))


;;----------------------------------------------;;


(defun mtg-mode-abbrev-table-list (&optional abbreviations)


  "Return an Abbrev-List for ‘mtg-mode-abbrev-table’."


  (let ((ALIST (or abbreviations (mtg-expansion-alist))))


    (cl-loop for (ABBREVIATION . EXPANSION) in ALIST
               collect `(,ABBREVIATION ,EXPANSION nil :system t)))


;;----------------------------------------------;;


(define-abbrev-table 'mtg-mode-abbrev-table


    (append (mtg-mode-abbrev-table-list)
            `(
            ;; ("" "" nil :system t)
              ))


  "The ‘abbrev-table-p’ for ‘mtg-mode’.


Initialized from function ‘mtg-mode-abbrev-table-list’."


;; :parents (list)
  :regexp nil)


;;----------------------------------------------;;


;; TODO (mtg-contraction-alist)?


(defun mtg-replacement-alist ()


  "Return an Association-‘listp’ of replacements (‘stringp’s)."


  (append mtg-unicode-alist
          mtg-abbreviation-alist))


;;----------------------------------------------;;






;;==============================================;;


(defun mtg-replace (&optional replacement-alist string-or-buffer)


  "Replace selon (each ‘consp’ of) REPLACEMENT-ALIST in STRING-OR-BUFFER.


=== Type ===


Inputs:


• REPLACEMENT-ALIST — an Association-‘listp’ from ‘stringp’s to ‘stringp’s.
Defaults to function ‘mtg-replacement-alist’.


• STRING-OR-BUFFER — a ‘string-or-buffer-p’.
Defaults to function ‘current-buffer’.


Output:


• a ‘stringp’ (if STRING-OR-BUFFER is a ‘stringp’) or nil (if STRING-OR-BUFFER is a ‘bufferp’ or nil).


Effects:


• Modifies either the STRING-OR-BUFFER (if STRING-OR-BUFFER is a ‘bufferp’) or ‘current-buffer’ (if STRING-OR-BUFFER is nil).


=== Examples ===


M-: (mtg-replace (mtg-replacement-alist) \"\")
 → \"\"


=== Laws ===


• (mtg-replace nil) ≈ ‘ignore’


• (mtg-replace nil "…") ≈ ‘identity’
"


  (with-temp-buffer
    (insert text)


  (save-excursion


    (save-restriction


 ;;     (or beg (setq beg (point-min)))
 ;;     (if end (narrow-to-region (point-min) end))


    (save-match-data


      (cl-loop for (STRING . ENCODED-STRING) in my/html-substitutions


        with REGEXP = (rx-to-string `(: ,STRING))


        do (goto-char (point-min))
           (while (search-forward REGEXP nil t)
               (replace-match ENCODED-STRING t t nil))))


    (buffer-substring (point-min) (point-max))))


;;----------------------------------------------;;


(cl-defun mtg-abbreviate-text (text &key abbrevs)


  "Replace ABBREVS in TEXT.


Inputs:


• TEXT — a ‘stringp’.
[TODO a ‘stringp’ or ‘bufferp’?]


Inputs (Options):


• ABBREVS — an association-`listp' between ‘stringp’s.
Defaults to ‘mtg-abbreviations-alist’.
Each ‘car’ is the replacement;
each ‘cdr’ which matches a TEXT substring will be replaced. The replacements should be shorter; i.e. the ‘length’ (or ‘string-width’) of each ‘car’ is shorter than its ‘cdr’ (e.g. ‘mtg-table/clean-text’ assumes this holds).


Output:


• a ‘stringp’.
[TODO a ‘stringp’ or nil?]
• Replacements are not recursive."


  (when-let* ((ABBREVIATIONS (copy-sequence (or abbrevs mtg-abbreviations-alist))))


    (save-match-data
(cl-typecase text


 (buffer (with-current-buffer text
(format-replace-strings mtg-abbreviation-alist)))


 (string (with-temp-buffer
(insert text)
(format-replace-strings mtg-abbreviation-alist)
(buffer-substring)))


  (t        (format-replace-strings mtg-abbreviation-alist))))))


;;----------------------------------------------;;


(cl-defun mtg-abbreviate-card (card &key abbrevs)


  "Replace ABBREVS in (a copy of) CARD.


Inputs:


• CARD — an ‘mtg-card-p’.


Output:


• a new ‘mtg-card-p’.
• Updates ‘mtg-card-rulestext’:


    • Replace abbreviations via ‘mtg-abbreviate-text’.
    • Replace any namesakes (i.e. the ‘mtg-card-name’) with “~” (i.e. ‘mtg-tilde-string’)."


  ())


;;----------------------------------------------;;


;;==============================================;;


















  (let* ((STYLED-REGEXP (mtg--english-card-name-styled-regexp)))


    (save-match-data


     (while (re-search-forward STYLED-REGEXP nil t)
      (replace-match ))


(add-text-properties BEG END '() OBJECT)


)))












;;----------------------------------------------;;


(defconst mtg-default-abbreviations


  `()


  "Default ‘mtg-abbreviations’.")






Injectivity:


• For convenience (when entering text), each abbreviation (i.e. ‘car’) should be unique.
• For clarity (when displaying text), each abbreviateé (i.e. ‘cdr’) should be unique.


")


;;----------------------------------------------;;


(cl-defun mtg-abbreviation (text &key foldcase)


  "Return an abbreviation of TEXT.


Inputs:


• TEXT — a ‘stringp’.
TEXT is looked up in ‘mtg-abbreviation’.
This lookup respects FOLDCASE.


Inputs (Options):


• FOLDCASE — a ‘booleanp’:


    • nil (the default) means “case-sensitive”.
    • non-nil means “case-insensitive”.


Output:


• a ‘stringp’ or nil.
• nil means “can't be abbreviated”.


Examples:


M-: (mtg-abbreviation \"until end of turn\")
  ↪ "ueot"


;;[TODO respect case]
M-: (mtg-abbreviation \"Until end of turn\")
  ↪ "Ueot"


M-: (mtg-abbreviation \"until end of phase\")
  ↪ nil"


(when-let* ((DATA (assoc-string text mtg-abbreviation foldcase))


(ABBREVIATION (cl-typecase DATA
(string DATA)
(list   (car DATA)))))


  ABBREVIATION))




;; Abbreviations:
;;
;; • Context-Free Abbreviations (e.g. “etb” as “enters the battlefield”).
;;
;; • Context-Sensitive Abbreviations (e.g. “~” as the current card's name).
;;














mtg-table/clean-text abbrevs


?¦ ; “BROKEN BAR” ; represent newlines


(ABBREVIATED-TEXT (mtg-abbreviate text :abbrevs abbrevs))


;; ^ Abbreviate before truncating.










mtg-symbolize-text replace some substrings (mostly words) with symbolic Unicode characters.


mtg-iconize-text


;;==============================================;;






;;==============================================;;






















































MTG MESSAGES
 
;; (message (substitute-command-keys "Press « \\[mtg-COMMAND] » to ACTION."))


;;==============================================;;


;; ElDoc?


  (message (substitute-command-keys "Press « \\[mtg-COMMAND] » to ACTION."))


;;==============================================;;
































































MTG-VIEW-CARD-MODE


;;==============================================;;


;;;###autoload
(define-derived-mode mtg-view-card-mode special-mode "MTG View Card"


  "Major mode for viewing help text and navigating references in it.
Entry to this mode runs the normal hook `help-mode-hook'.


Commands:
\\{mtg-view-card-mode-map}"


  (progn


    (setq-local revert-buffer-function #'mtg-revert-view-card-mode-buffer)


    ()))


;;----------------------------------------------;;


;; like ‘help-mode’, but: with images; with hyperlinks between cards (which, like ‘facep’s, may or may not be ‘boundp’ variables); 


mtg-view-card-mode ; help-mode?


;;----------------------------------------------;;


;;==============================================;;


(cl-defun mtg-view-card (card-or-name &key )


  "View CARD-OR-NAME in ‘mtg-view-card-mode’.


=== Inputs ===


• CARD-OR-NAME — an ‘mtg-card-p’ or a ‘stringp’.


=== Features ===


Hyperlinks:


• Multifaced Cards — Link the other Card Faces of the (Multifaced) Card. Includes: split cards (from Invasion), flip cards (from Kamigawa), double-faced cards (from Innistrad), triple-faced cards (from Return To Innistrad), Aftermath cards, etc. e.g. ⟨Fire // Ice⟩, ⟨⟩, ⟨⟩, ⟨⟩, ⟨⟩. Links are labeled appropriately. e.g. (with Emacs' ‘button’ conventions) “[Right Half  (…)]” / “[Left Half  (…)]”, “[Rotate (to …)]”, “[Transform (into …)]”, “[Meld (into …)]”, etc.


• References — Link any non-Namesake References (Namesake References themselves may be abbreviated as “~”, or just styled).
e.g. ⟨Arachnus Spinner⟩ references  ⟨Arachnus Web⟩.


• TODO Back-References — e.g. ⟨Arachnus Web⟩ is referenced by ⟨Arachnus Spinner⟩.


• “Related” — Link to a (manually-curated & personally-customizable) list of cards that are related and/or similar. e.g. ⟨Spell Swindle⟩ and ⟨Mana Drain⟩ are both counterspells that “add X=c.m.c. mana”. See ‘mtg-related-cards-alist’. 𝗣𝗹𝗲𝗮𝘀𝗲 𝗰𝗼𝗻𝘁𝗿𝗶𝗯𝘂𝘁𝗲 𝘆𝗼𝘂𝗿 𝗼𝘄𝗻 𝗿𝗲𝗹𝗮𝘁𝗶𝗼𝗻𝘀! (edit ‘mtg-contrib.el’ at URL ‘https://github.com/sboosali/mtg.el/blob/master/lisp/mtg-contrib.el’"


  (interactive (list (mtg-read-card :by 'name)))


  ())


(mtg--button )


;; e.g. Arachnus Spinner (card):
;;
;; the “Arachnus Web” in “… named Arachnus Web …” gets ‘propertize’d (‘buttonize’d).
;;
;; 


;; e.g. Urborg Panther (card):
;;
;; “{B}, Sacrifice Urborg Panther: Destroy target creature blocking Urborg Panther. Sacrifice a creature named Feral Shadow, a creature named Breathstealer, and Urborg Panther: Search your library for a card named Spirit of the Night and put that card onto the battlefield. Then shuffle your library.”
;;


;;----------------------------------------------;;


;;==============================================;;


(defconst mtg-card-button 'mtg-card


  "Button-Type for linking to an ‘mtg-card’.")


;;----------------------------------------------;;


(define-button-type mtg-card-button


  'action      #'mtg-button-action
  'follow-link t)


;;----------------------------------------------;;


(defun mtg-button-action (button)


  "The symbol ‘action’ for ‘mtg-card-button’.


Call BUTTON's property ‘help-function’."


  (help-do-xref nil
                (button-get button 'help-function)
                (button-get button 'help-args)))


;;----------------------------------------------;;


(defun help-do-xref (_pos function args)
  "Call the help cross-reference function FUNCTION with args ARGS.
Things are set up properly so that the resulting help-buffer has
a proper [back] button."
  ;; There is a reference at point.  Follow it.
  (let ((help-xref-following t))
    (apply function (if (eq function 'info)
                        (append args (list (generate-new-buffer-name "*info*"))) args))))


;;==============================================;;


(defun mtg-revert-view-card-mode-buffer (&optional buffer)


  "‘revert-buffer’ for ‘mtg-view-card-mode’."


  (let* (( ()))
    (revert-buffer buffer)))












;;==============================================;;














;;==============================================;;












































































MODE




(defconst mtg-comment-char ?#
  "The primary Comment Starter for ‘mtg-mode’, a ‘characterp’.")


(defconst mtg-comment-start


  (rx-to-string `(,mtg-comment-char))


(defconst mtg-comment-start-skip


  (rx-to-string `(1+ (any ,mtg-comment-char space))


  "‘comment-start-skip’ for ‘mtg-mode’.


Skips extra whitespace and/or ‘mtg-comment-char’s (doesn't match the empty string).")








 :abbrev-table mtg-mode-abbrev-table
 (setq-local local-abbrev-table mtg-mode-abbrev-table)




  (setq-local comment-start-skip mtg-comment-start-skip)


  (setq-local comment-padding mtg-comment-padding)


  (setq-local comment-multi-line t)


  ;; ^ t means: Multiline Comments are continued across lines (i.e. without the Comment Ender at the end of each line).


  (setq-local comment-use-syntax t)


  ;; ^ t means: syntax-tables, without regexps, can determine comments.






  (setq-local add-log-current-defun-function #'mtg-current-card-name)






TODO Don't specify an overall prompt string for the main map of a major or minor mode, because that would cause the command loop to present a keyboard menu every time.


















;;


(defun mtg-beginning-of-card ()


  "Move ‘point’ to the beginning of the current Card


(“current” meaning “currently being defined”)."


  (interactive)


  (let* ()
    ()))


;;






;;


(defun mtg-current-card-name ()


  "Return the name of the Card at ‘point’, or nil.


Is ‘add-log-current-defun-function’ for ‘mtg-mode’."


  (save-excursion
    ()))


;;


(defun mtg-current-card-face-name ()


  "Return the name of the Card Face at ‘point’, or nil."


  (save-excursion
    (let ((POINT (point)))
      (re-search-backward mtg-declared-card-name-regexp))))




















      ;; If we are now precisely at the beginning of a defun, make sure
      ;; beginning-of-defun finds that one rather than the previous one.


      (or (eobp) (forward-char 1))
      (beginning-of-defun)


      ;; Make sure we are really inside the defun found, not after it.


      (when (and (looking-at "\\s(")
                 (progn (end-of-defun)
                        (< location (point)))
                 (progn (forward-sexp -1)
                        (>= location (point))))


        (if (looking-at "\\s(")
            (forward-char 1))


        ;; Skip the defining construct name, typically "defun" or
        ;; "defvar".
        (forward-sexp 1)
        ;; The second element is usually a symbol being defined.  If it
        ;; is not, use the first symbol in it.


        (skip-chars-forward " \t\n'(")
        (buffer-substring-no-properties (point)
                                        (progn (forward-sexp 1)
                                               (point)))))))






(backward )


;; ^ Skip comments and blank lines.
;;
;; “the line is blank (i.e., empty or containing only whitespace characters)”
;;
;;


(save-excursion (beginning-of-line) (not (looking-at "\\s-*$"))






(defconst mtg--blank-line-or-comment-rx
  (rx bol (0+ (or blank (syntax comment-start)) eol)
  "Matches a comment and/or blank line.")


(defun mtg--blank-line-or-comment-p


  ( mtg--blank-line-or-comment-rx))


;; ^ Skip comments and/or blank lines.
;;
;; “the line is blank if empty or containing only whitespace characters)”
;;
;;


greek
cyrillic


chinese
japanese
korean


arabic
hebrew
ethiopic ; Geʽez, for Ethiopian/Eritrean languages.


indian


lao
tibetan
thai
vietnamese














(defcustom mtg-language-alist mtg-builtin-language-alist




Programmatic Customization…


e.g. to change the flag:


    (setf (mtg-language-flag (assq 'french mtg-language-alist)) ?🇨🇦)


    (setf (mtg-language-flag (assq 'spanish mtg-language-alist)) ?🇦🇷)
























































TAGS






















































































;;; RX-MTG
;;; MTG-RX


;;==============================================;;


defconst mtg-rx-constituents 


    (boz . symbol-start)
    (eoz . symbol-end)


    ;; ^ “z” for Symbol, because “s” is taken by Buffer.


    (word   . (mtg-rx-word   1 nil))
    (symbol . (mtg-rx-symbol 1 nil))
    (line   . (mtg-rx-line   1 nil))


    (w . word)
    (s . symbol)
    (l . line)


    ;; ^ boundary wrappers.


words word-start word-end


symbols symbol-start symbol-end


parens (: "(" _ ")")


braces (: "{" _ "}")


brackets (: "[" _ "]")


quotes (or ("\"" _ "\"") ("“" _ "”") …)


;; 


mtg-stem
mtg-pluralize
mtg-capitalize


;; e.g. ‘mtg-stem’:
;; 
;; (mtg-stem "elf") ≈ (: word-start (or "elf" "elves" "Elf" "Elves") word-end)


;;==============================================;;


;; defcustom mtg-noun-plurals-alist


  `(
   )


;;----------------------------------------------;;


defcustom mtg-noun-plural-rule-alist


  `(
    ("ss$" . "sses")
    ("zz$" . "zzes")  ; e.g. "buzzes".
    ("sh$" . "shes")
    ("tch$" . "tches")
    ("eaf$" . "eaves")
    ("ief$" . "ieves")  ; e.g. "theives".
    ("roof$" . "roofs")
    ("oof$" . "ooves")
    ("ife$" . "ives")
    ("lf$" . "lves")  ; e.g. "elf" → "elves"
    ("[aeiou]y$" . "\\&s")
    ("ndum$" . "nda")  ; e.g. "addendum".
    ("um$" . "a")  ; e.g. "media", "criteria", "symposia",
                  ;; "crania", curriculum", "data".
    ("^die$" . "dice")
    ("dogma$" . "dogmas") ;; exception to -ma rule.
    ("lemma$" . "lemmas") ;; exception to -ma rule.
    ("schema$" . "schemas") ;; exception to -ma rule.
    ("ia$" . "ium")  ; e.g. "bacteria".
    ("ma$" . "mata")  ; e.g. "stigma".
    ("na$" . "nae")  ; e.g. "antenna".
    ("ta$" . "tum")  ; e.g. "strata".
    ("Atlas$" . "Atlantes") ;; Case-sensitive
    ("atlas$" . "atlases")
    ("Harry$" . "Harrys") ;; Case-sensitive
    ("aircraft$" . "aircraft")
    ("alga$" . "algae")
    ("alumna$" . "alumnae")
    ("alumnus$" . "alumni")
    ("ameoba$" . "ameobae")
    ("automaton$" . "automata")
    ("bacillus$" . "bacilli")
    ("banjo$" . "banjos")
    ("beau$" . "beaux")
    ("cactus$" . "cacti") ;; Or "cactuses".
    ("cannon$" . "cannon") ;; Or "cannons".
    ("canto$" . "cantos")
    ("cargo$" . "cargos")
    ("cattle$" . "cattle")
    ("child$" . "children")
    ("cod$" . "cod")
    ("corpus$" . "corpora")
    ("dwarf$" . "dwarves")
    ("cs$" . "csen")  ; e.g. "emacsen".
    ("foot$" . "feet")
    ("formula$" . "formulae")
    ("graffito$" . "graffiti")
    ("rion$" . "ria")  ; e.g. "criteria".
    ("deer$" . "deer")
    ("focus$" . "foci")
    ("genus$" . "genera")
    ("goose$" . "geese")
    ("hedron$" . "hedra")  ; e.g. "polyhedron".
    ("hippopotamus$" . "hippopotami")
;;    ("index$" . "indices") ;; "indexes" is also acceptable.
    ("insigne$" . "insignia")
    ("life$" . "lives")
    ("louse$" . "lice")


    ("mackerel$" . "mackerel")
    ("man$" . "men")
    ("matrix$" . "matrices")
    ("moose$" . "moose")
    ("motto$" . "mottos")
    ("mouse$" . "mice")
    ("nucleus$" . "nuclei")
    ("octopus$" . "octopi") ;; Or "octopuses".
    ("offspring" . "offspring")
    ("opus$" . "opera")
    ("\\box$" . "oxen")
    ("panino$" . "panini")
    ("paparazzo$" . "paparazzi")
    ("phalanx$" . "phalanges")
    ("phenomenon$" . "phenomena")
    ("people$" . "people")
    ("perch$" . "perch") ;; Can certain uses of "perch" be plural?
    ("piano$" . "pianos")
    ("police$" . "police")
    ("portico$" . "porticos")
    ("quarto$" . "quartos")
    ("radius$" . "radii")
    ("rhinoceros$" . "rhinoceri") ;; Or "rhinoceroses".
;;    ("series$" . "series") ;; Already has an "s".
    ("sheep$" . "sheep")
;;    ("species$" . "species") ;; Already has an "s".
    ("solo$" . "solos")
    ("syllabus$" . "syllabi")
    ("terminus$" . "termini")
    ("ulus$" . "uli")  ; e.g. "stimuli".
    ("trout$" . "trout")
    ("tooth$" . "teeth")
    ("uterus$" . "uteri") ;; Or "uteruses".
    ("virtuoso" . "virtuosi")
    ("viscus$" . "viscera")
;;    ("woman$" . "women") ;; See "man$".
;;    ("e$" . "es") ;; Fall-through to "[^s]$".
    ("is$" . "es")  ; e.g. "axes", "crises", "testes".
    ("us$" . "uses")  ; e.g. "campuses", "platypuses", "prospectuses".
    ("io$" . "ios")
    ("oo$" . "oos")
    ("o$" . "oes")
    ("y$" . "ies")
    ("[ei]x$" . "ices")  ; e.g. "vertices".
    ("x$" . "xes")


    ("[^s]$" . "\\&s") ;; Add an `s' if not an `s'.
   )


  "Associative list with first element a regular expression
 for the suffix of nouns, and the second element is
 the replacement to make the word plural.


Matches are made in order of appearance.


Sorted by order of plural \"operation\", secondarily by case order,
then by alphabetical order.


Links:


• URL ‘http://en.wikipedia.org/wiki/English_plural’")


;;----------------------------------------------;;


;;==============================================;;


;;----------------------------------------------;;


(defun mtg-stem-regexp (text)


  "Return a regexp matching declensions of stem TEXT.


See function ‘mtg-from-stem’."


  (mtg--regexp-opt (mtg-from-stem text) 'words))


;;----------------------------------------------;;


(define-inline mtg-from-stem (text)


  "Return all declensions/capitalizations of stem TEXT.


=== Inputs/Output ===


• TEXT — a ‘stringp’.


• Return a ‘stringp’."


  (declare (side-effect-free t))


  (inline-letevals ((TEXT text))
    (inline-quote
      ( ,TEXT))))


(let* ((STEMS '("elf"))


 (PLURALIZED-WORDS
  (cl-loop for SINGULARS = STEMS
           for WORD in SINGULARS
   append (mtg-pluralize WORD) into PLURALS
   finally return (append STEMS PLURALS )))


 (CAPITALIZED-WORDS
  (cl-loop for UNCAPITALIZED = PLURALIZED-WORDS
           for WORD in UNCAPITALIZED
   collect (mtg-capitalize WORD) into CAPITALIZED
   finally return (append UNCAPITALIZED CAPITALIZED)))


 (WORDS (cl-remove-duplicates CAPITALIZED-WORDS)))


  ( WORDS)))


;;----------------------------------------------;;


(define-inline mtg-decline-noun (text)


  "Return all (English) declensions of the noun/stem TEXT.


See:


• variable ‘mtg-noun-declensions-alist’.
• variable ‘mtg-noun-plurals-alist’."


  ())


;;----------------------------------------------;;


(define-inline mtg-pluralize-noun (text)


  "Return the (English) plural of TEXT.


Examples:


• M-: (mtg-pluralize-noun \"elf\")
 ↪ \"elves\"


See:


• variable ‘mtg-noun-plurals-alist’."


  (
(or (alist-get mtg-noun-plurals-alist)


"es"
"s"
))


;; TODO wget https://www.emacswiki.org/emacs/download/plural.el


;;==============================================;;


;;----------------------------------------------;;


(defun mtg-rx-word (form)


  "Return a regexp form which surrounds FORM with word boundaries.


For example, « (mtg-rx-word \\='(_ (or \"T\" \"t\") \"he\")) », desugared from « (mtg-rx (word (or \"T\" \"t\") \"he\")) », should be equivalent to « \\='(word-start (or \"T\" \"t\") \"he\" word-end) ».


FORM has form like « (word FORM¹ FORM² …) »."


  (rx-check form)


  (let ((FORM `(word-start ,@(cdr form) word-end)))


    (rx-and `(and ,@FORM))))


;;----------------------------------------------;;


(defun mtg-rx-symbol (form)


  "Return a regexp form which surrounds FORM with symbol boundaries.


For example, « (mtg-rx-symbol \\='(_ ?{ (1+ digit) ?})) », desugared from « (mtg-rx (symbol ?{ (1+ digit) ?})) », should be equivalent to « \\='(symbol-start ?{ (1+ digit) ?} symbol-end) ».


FORM has form like « (symbol FORM¹ FORM² …) »."


  (rx-check form)


  (let ((FORM `(symbol-start ,@(cdr form) symbol-end)))


    (rx-and `(and ,@FORM))))


;;----------------------------------------------;;


(defun mtg-rx-line (form)


  "Return a regexp form which surrounds FORM with line boundaries.


For example, « (mtg-rx-line \\='(_ \"Choose one \" (or ?— \"--\"))) », desugared from « (mtg-rx (line \"Choose one \" (or ?— \"--\"))) », should be equivalent to « \\='(line-start \"Choose one \" (or ?— \"--\") line-end) ».


FORM has form like « (line FORM¹ FORM² …) »."


  (rx-check form)


  (let ((FORM `(line-start ,@(cdr form) line-end)))


    (rx-and `(and ,@FORM))))


;;----------------------------------------------;;


;;==============================================;;






















(defconst mtg-mana-cost-regexp)


(defconst mtg-declared-card-name-regexp


  (mtg-rx-to-string `(: bol (group ,mtg-card-name-regexp) (or ,mtg-newline ,mtg-mana-cost-regexp)))


  "Matches a Card Name.


Matches some words which:


• start the line.


• are mostly ‘capitalize’d;
the first and last words being always capitalized.
(See ‘mtg-card-name-regexp’.)


• are ended by an ‘mtg-newline’ or ‘mtg-symbol’;
i.e. don't seem to be part of Rules Text.")


;;(defconst mtg-referenced-card-name-regexp (or "~"))










(defconst mtg-card-face-divider-regexp
  (rx symbol-start "//" symbol-end))


(defconst mtg-card-face-name-regexp
  (rx-to-string `(: (1+ (or (1+ blank) ,mtg-card-face-divider-regexp) eol))))


;; ^ a Card Name starts a Card Block (and a Card Face Block).
;; 
;; the prior line is either:
;; • a blank line for single-faced cards (matching (rx (1+ blank))).
;; • a separator, for the other faces of a multi-faced card (matching (rx "//")).
;;
;;














































SYNTAX














(defconst mtg-atom-regexp


  (rx-to-string `())


  "Regexp matching an atomic word/symbol.


e.g.:


• « the », « The »
• « {T} », « {2/b} »
• « 1/2 », « 3 »
• « ~ »
• « Yawgmoth's », « il-Kor »
• « , », « ; », « . », « : », « ( », « ) » 
• « • », « — »
")


















;; English / Spanish (Español) / French (Français) / Portuguese (Português) / Italian (Italiano) / Russian (Русский) (/ Greek (Ελληνικά))…
‘’


;; Deutsch:


;; German (Deutsch) (/ Dutch (Nederlands))…
„‟
‚‘


;; CJK:


「」 ; 
『』


「 ; LEFT CORNER BRACKET (full-width)
」; RIGHT CORNER BRACKET (full-width)










mtg-sentence-end-regexp
(or eol ". " ";")




(defun mtg-forward-sentence (&optional arg)


  "Move forward to next end of sentence.  With argument, repeat.
With negative argument, move backward repeatedly to start of sentence.
The variable `sentence-end' is a regular expression that matches ends of
sentences.  Also, every paragraph boundary terminates sentences as well."


  (interactive "^p")


  (or arg (setq arg 1))
  (let ((opoint (point))
        (sentence-end (sentence-end)))
    (while (< arg 0)
      (let ((pos (point))
            par-beg par-text-beg)
        (save-excursion
          (start-of-paragraph-text)
          ;; Start of real text in the paragraph.
          ;; We move back to here if we don't see a sentence-end.
          (setq par-text-beg (point))
          ;; Start of the first line of the paragraph.
          ;; We use this as the search limit
          ;; to allow sentence-end to match if it is anchored at
          ;; BOL and the paragraph starts indented.
          (beginning-of-line)
          (setq par-beg (point)))
        (if (and (re-search-backward sentence-end par-beg t)
                 (or (< (match-end 0) pos)
                     (re-search-backward sentence-end par-beg t)))
            (goto-char (match-end 0))
          (goto-char par-text-beg)))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
        (if (re-search-forward sentence-end par-end t)
            (skip-chars-backward " \t\n")
          (goto-char par-end)))
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))




























(mtg-symbol-cost . ,(rx)))


;;






;;




















(defun mtg-propertize ()


  ())














FONT LOCK




(defalias 'mtg-mark-block #'mark-paragraph)


(defalias 'mtg-font-lock-mark-block #'mtg-mark-block)




(setq font-lock-defaults
        `(,mtg-font-lock/keywords
          nil nil nil nil
          (font-lock-mark-block-function . mtg-font-lock-mark-block)
          (font-lock-syntactic-face-function 
           . mtg-font-lock-syntactic-face)))






         ;; Highlight occurrences of either ‘foo’ or ‘bar’, using
          ;; foo-bar-face, even if they have already been highlighted.
          ;; foo-bar-face should be a variable whose value is a face.
          ("foo\\|bar" 0 foo-bar-face t)
          
          ;; Highlight the first subexpression within each occurrence
          ;; that the function fubar-match finds,
          ;; using the face which is the value of fubar-face.
          (fubar-match 1 fubar-face)






(setq-local sentence-end-double-space nil) ; Single-Space is enough to end a Sentence.




;; “Lines that start a new paragraph and are contained in it must match only ‘paragraph-start’, not ‘paragraph-separate’.”










;; Card Parser…
;;
;; Propertize a text block (representing an MTG Card), one line at a time. This parser is simple, it uses the ‘mtg-mode-syntax-table’ and the many ‘mtg-mode-*-regexp’s but does also keep some state.






(defun mtg-mark-defun ()


  "‘mark-defun’ for ‘mtg-mode’.


Marks by ‘mtg-mark-card’ within MTG Cards,
plus any trailing commentary (i.e. comments below the card)."


  (interactive)


  (mtg-mark-card))








(defun mtg-mark-card ()


  "‘mark’ the current MTG Card."


  (interactive)


  (let* ((comments-as-whitespace t))


    (mark-block)))


































;;


(defconst mtg-mode-syntax-propertize


  (syntax-propertize-rules
    ("\\<dnl\\>" (0 "<")))


  "‘syntax-propertize-function’ for ‘mtg-mode’.")


 (setq-local syntax-propertize-function mtg-mode-syntax-propertize)








  (syntax-propertize-rules
   ("#" (0 (when (m4--quoted-p (match-beginning 0))
             (string-to-syntax "."))))))


(defun m4--quoted-p (pos)
  "Return non-nil if POS is inside a quoted string."
  (let ((quoted nil))
    (dolist (o (nth 9 (save-excursion (syntax-ppss pos))))
      (if (eq (char-after o) ?\`) (setq quoted t)))
    quoted))


(defconst m4-syntax-propertize
  (syntax-propertize-rules
   ("#" (0 (when (m4--quoted-p (match-beginning 0))
             (string-to-syntax "."))))))


(add-hook 'm4-mode-hook
          (lambda () (set (make-local-variable 'syntax-propertize-function)
                          m4-syntax-propertize)))






    (modify-syntax-entry ?#  "<\n" TABLE)
    (modify-syntax-entry ?\n ">#"  TABLE)




    (modify-syntax-entry ?!  "." TABLE)
    (modify-syntax-entry ?@  "." TABLE)
    ;; (modify-syntax-entry ?\# "." TABLE)
    (modify-syntax-entry ?$  "." TABLE)
    (modify-syntax-entry ?%  "." TABLE)
    (modify-syntax-entry ?^  "." TABLE)
    (modify-syntax-entry ?&  "." TABLE)
    ;; (modify-syntax-entry ?*  "." TABLE)


    (modify-syntax-entry ?=  "." TABLE) ; the equal sign is the definition operator.
    (modify-syntax-entry ?+  "." TABLE)
    (modify-syntax-entry ?<  "." TABLE)
    (modify-syntax-entry ?>  "." TABLE)
    ;; (modify-syntax-entry ?/  "." TABLE)
    (modify-syntax-entry ?:  "." TABLE)
    (modify-syntax-entry ?\? "." TABLE)
    (modify-syntax-entry ?\\ "." TABLE)
    (modify-syntax-entry ?|  "." TABLE)












(defun m4--quoted-p (pos)
  "Return non-nil if POS is inside a quoted string."
  (let ((quoted nil))
    (dolist (o (nth 9 (save-excursion (syntax-ppss pos))))
      (if (eq (char-after o) ?\`) (setq quoted t)))
    quoted))
























;;; CAP




deflist mtg-completion-at-point-functions


  "‘completion-at-point-functions’ for ‘mtg-mode’."




The functions on this hook should generally return quickly, since they may be called very often (e.g., from post-command-hook).










(defun mtg-complete-at-point (p)


  "Complete the phrase at P.


P — an ‘integerp’ or ‘markerp’.
Defaults to the current point ‘point’.


  (interactive "*d")  ; ‹*› means writable buffers only.
                      ; ‹d› means ‘point’ as an ‘integerp’.


  ())










(completing-read
           prompt
           (lambda (string predicate action)


             (cl-case action
(metadata `(metadata
                   (annotation-function . mule--ucs-names-annotation)
                   (category . unicode-name)))
(otherwise  (complete-with-action action (ucs-names) string predicate)))))




(let ((char (gethash name ucs-names)))
 (when char (format " (%c)" char))))


















































;;; READERS:


(let* ((THING   'symbol)
       (DEFAULT (thing-at-point THING))
       (NAME    "Name")
       (PROMPT  (if DEFAULT 
                   (format "%s (default %s): " NAME DEFAULT)
                 (format "%s: " NAME))))


  (completing-read PROMPT))


























































OBARRAYS


;;


(defconst mtg--obarray


  "“Namespace” for (known) MTG s.


an ‘obarrayp’ of ‘mtg--p’s.")


;;


(defconst mtg-card-obarray


  "“Namespace” for (known) MTG Cards.


an ‘obarrayp’ of ‘mtg-card-p’s.")


;;


(defconst mtg-edition-obarray


  "“Namespace” for (known) MTG Editions.


an ‘obarrayp’ of ‘mtg-edition-p’s.")
;;


(defconst mtg--obarray


  "“Namespace” for (known) MTG s.


an ‘obarrayp’ of ‘mtg--p’s.")


;;


(defconst mtg--obarray


  "“Namespace” for (known) MTG s.


an ‘obarrayp’ of ‘mtg--p’s.")


;;


(defconst mtg--obarray


  "“Namespace” for (known) MTG s.


an ‘obarrayp’ of ‘mtg--p’s.")


;;


(defconst mtg--obarray


  "“Namespace” for (known) MTG s.


an ‘obarrayp’ of ‘mtg--p’s.")


;;


(defconst mtg--obarray


  "“Namespace” for (known) MTG s.


an ‘obarrayp’ of ‘mtg--p’s.")




;;
























COMPLETION








Context-Sensitive Completion:


• “ █” — completes an ‘mtg-’.


• “named █” — completes an ‘mtg-card-name’.


• “ █” — completes an ‘mtg-’.


• “ █” — completes an ‘mtg-’.


• “ █” — completes an ‘mtg-’.












(defun mtg-complete-at-point (&optional point)


  "‘completion-at-point’ for ‘mtg-mode’.


Completes:


• Keywords — anywhere in Rules Text (including at the start of a line, and in the middle of the word)
• Ability Words — in Rules Text (only at the start of a line).
• Supertypes/Cardtypes — at the start of the Typeline.
• Subtypes — at the end of a Typeline (i.e. after the “—”).
• Abbreviations — 


Notes:


• Spacing — Keywords and ability words may have spaces (e.g. “First strike” or “Spell mastery”). Thus, the completion can span multiple words; in particular, completions are not necessarily syntactic symbols (a Syntactic Symbol means that all its characters have Syntax Code “_” or Syntax Code “w”).


See:


• URL ‘https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html’
• Info Node ‘(elisp) Completion in Buffers’."














form:


          (start end collection . props)


start and end delimit the text to complete (which should enclose point).


props is a property list for additional information; any of the properties in completion-extra-properties are recognized (see Completion Variables), as well as the following additional ones:


:predicate
The value should be a predicate that completion candidates need to satisfy. 


:exclusive
If the value is no, then if the completion table fails to match the text at point, completion-at-point moves on to the next function in completion-at-point-functions instead of reporting a completion failure.








;;


;;


defalist mtg-annotation-alist


`(
(card    . ?🀆)  ; Options: 🂠 (PLAYING CARD BACK), 🀆 (MAHJONG TILE WHITE DRAGON).
(set     . ?)  ; 
(keyword . ?⚿)  ; Options: 🔑 (KEY), 🗝 (OLD KEY), ⚿ (SQUARED KEY).


)


;;


(defun mtg-complete-at-point/annotate (candidate)


  "Annotate CANDIDATE.


Appends a different Unicode character for different types (of MTG Object). For example, the “PLAYING CARD BACK” or “MAHJONG TILE WHITE DRAGON” Unicode characters for cards (configured via ‘mtg-annotation-alist’), vs “” for sets a.k.a. editions (vs keywords &c). e.g. (mtg-annotate \"Time Spiral\" 'card) evals to \"Time Spiral 🀆\", while (mtg-annotate \"Time Spiral\" 'edition) evals to \"Time Spiral \".


"


  ())


;;


(defconst mtg-complete-at-point/annotation-function #'mtg-complete-at-point/annotate


  "The ‘annotation-function’ metadata for ‘mtg-complete-at-point’.")


- `annotation-function': function to add annotations in *Completions*.
   Takes one argument (STRING), which is a possible completion and
   returns a string to append to STRING.


;;






(mtg-complete-at-point/-max-display 20 "Display more modules.")
(mtg-complete-at-point/-preferred '() "Display these modules first (at the top of the drop-down).")


)


(defcustom mtg-complete-at-point/-preferred
  '()
  "Override ordering of module results by specifying preferred modules."
  :group 'haskell
  :type '(repeat string))


(defcustom mtg-complete-at-point/-max-display
  10
  "Maximum items to display in minibuffer."
  :group 'haskell
  :type 'number)






(defun mtg-complete-at-point/display-sort (candidates)


  "Sort CANDIDATES."


   (delete-dups candidates)


  ;; Sort with preferences:


   (cl-callf sort candidates 
                (lambda (a b)


                  (let ((a-mem? (member a mtg-complete-at-point/-preferred))
                        (b-mem? (member b mtg-complete-at-point/-preferred)))


                    (cond
                     ((and a-mem? (not b-mem?))
                      t)


                     ((and b-mem? (not a-mem?))
                      nil)


                     (t
                      (string< a b)))))))








;;


(defconst mtg-complete-at-point/display-sort-function #'mtg-complete-at-point/display-sort


  "The ‘display-sort-function’ for ‘mtg-complete-at-point’.")


- `display-sort-function': function to sort entries in *Completions*.
   Takes one argument (COMPLETIONS) and should return a new list
   of completions.  Can operate destructively.


;;


(defun mtg-complete-at-point/cycle-sort (candidates)


  "Sort CANDIDATES."


  ())


;;


(defconst mtg-complete-at-point/cycle-sort-function #'mtg-complete-at-point/cycle-sort


  "The ‘cycle-sort-function’ metadata for ‘mtg-complete-at-point’.")


- `cycle-sort-function': function to sort entries when cycling.
   Works like `display-sort-function'.


;;


(defconst mtg-complete-at-point/category


  '()


  "The ‘category’ metadata for ‘mtg-complete-at-point’.")


- `category': the kind of objects returned by `all-completions'.
   Used by `completion-category-overrides'.


;;


(defun mtg-complete-at-point-metadata 


  "Return the Completion-Metadata for ‘mtg-complete-at-point’."


  `(annotation-function ,#'mtg-complete-at-point/annotation-function
    display-sort-function ,#'mtg-complete-at-point/display-sort-function
    cycle-sort-function ,#'mtg-complete-at-point/cycle-sort-function
    category ,#'mtg-complete-at-point/category))


;;










(defun mtg-steganograph (text object) 


  "Smuggle an (MTG-)OBJECT into TEXT (as a ‘propertize’d reference)."


  (let* ((SYMBOL (if (symbolp object) object (mtg-intern-object object))))


 
   (propertize text 'mtg-object SYMBOL)))


;;


(defun mtg-intern-object (object) 


  "Intern OBJECT.


Input: an ‘objectp’ (declared by ‘mtg.el’).


Output: a ‘symbolp’."


  (when-let* ((SYMBOL (TODO object)))


    SYMBOL))


;;
















;;


(defun mtg-annotate-card-name (text &optional card) 


  "Annotate TEXT."


  ( text))


;;


(defun mtg-annotate-edition-name (text &optional edition) 


  "Annotate TEXT."


  ( text))


;;


(defun mtg-annotate-keyword (text &optional keyword) 


  "Annotate TEXT.


Annotations:


• Reminder Text — The ‘mtg-keyword-remindertext’ of the KEYWORD.".


  (let* ((REMINDER-TEXT (with-slots (remindertext) KEYWORD remindertext)))


    (propertize (format "(%s)" REMINDER-TEXT) 'face 'mtg-reminder-text)))


;;


(defun mtg-annotate-ability-word (text &optional ability-word) 


  "Annotate TEXT.


Annotations:


• Ellipsis Text — The ‘mtg-ability-word-pattern’ of the ABILITY-WORD.".


  (let* ((PATTERN-TEXT (mtg-ability-word-pattern ability-word)))


    (concat (propertize text 'face 'mtg-ability-word)
            " "
            (propertize (format "— %s…" PATTERN-TEXT) 'face 'mtg-rules-text))))


;;


(defun mtg-annotate-card-type (text &optional card-type) 


  "Annotate TEXT."


    (concat (propertize text 'face 'mtg-card-type)


            (when-let* ((ICON-STRING (mtg-card-type-get-icon-string card-type)))
              (concat " " ICON-STRING))))


;;


(defun mtg-annotate-creature-type (text &optional creature-type) 


  "Annotate TEXT."


    (concat (propertize text 'face 'mtg-creature-type)


            (when-let* ((ICON-CHAR   (mtg-creature-type-get-icon-string creature-type))
(ICON-STRING (string ICON-CHAR)))


              (concat " " ICON-STRING))))




;;


(defun mtg-annotate-printed-card-name (text &optional printing) 


  "Annotate TEXT.


Annotations:


• Language — The ‘mtg-language-abbr’ and/or ‘mtg-language-flag’ of the language in which PRINTING was printed.
"


  ( text))


;;


(defun mtg-annotate-text (text &optional ) 


  "Annotate TEXT."


  ( text))


;;


(defun mtg-annotate-text (text &optional ) 


  "Annotate TEXT."


  ( text))


;;        • Card Names  — are annotated with a summary of the card (color, type, cost).
;;        • Set Codes   — are annotated with their full names (e.g. « AN “Arabian Nights” »).
;;




Function to add annotations in the *Completions* buffer.
The function takes a completion and should either return nil, or a string that
will be displayed next to the completion.  The function can access the
completion table and predicates via `minibuffer-completion-table' and related
variables.")




;; ‘format’…
;;
;; >The characters in string, other than the format specifications, are copied directly into the output, including their text properties, if any. Any text properties of the format specifications are copied to the produced string representations of the argument objects.
;;








;;


mtg-super-type-get-icon-string
mtg-super-type-get-icon-char


;;


(defun mtg-card-type-get-icon-char (card-type)
  ())


;; M-: (mtg-card-type-get-icon-char 'artifact)
;;  ↪ ?🏺  ;; AMPHORA


;;


mtg-creature-type-get-icon-string
mtg-creature-type-get-icon-char


;;


(defun mtg-card-type-get-icon-string (card-type)
  (let ((CHAR (mtg-card-type-get-icon-char card-type)))
  (propertize (string CHAR) 'face 'mtg-icon-chars)))


;; M-: (mtg-card-type-get-icon-string 'artifact)
;;  ↪ #("🏺" …)


;;














;;


(defconst mtg-mode-card-types-regexp


  (mtgrx mtg-line mtg-blank 
         (group-n 1 (1+ (: mtg-capitalized-word blank)))
         mtg-blank mtg-dash)


  "")


;;


(defun mtg-looking-back-card-types (&optional limit point)


  "


Inputs:


• LIMIT — an ‘integerp’.


• POINT — a ‘number-or-marker-p’.


Output:


• a ‘stringp’ ‘listp’.


Matching:


• is case-insensitive.
e.g. either “Creature —” or “creature —”.


• respects ASCII.
e.g. either “Creature —” or “Creature --”.


• is prioritized.
e.g. an “Artifact Creature” may have the same subtypes as a (non-Artifact) “Creature”."


 (let* ((POINT (or point (point)))
        (LIMIT (or limit (point-min))))


  (save-excursion
    (goto POINT)


  (save-match-data


 (cond ((looking-back mtg-mode-card-types-regexp (max LIMIT (line-beginning-position)))


          (let* ((MATCH (match-string 1 nil)) 
                (TYPES (cl-loop for WORD in (split-string MATCH) collect (mtg-intern WORD)))
                 )
            (mtg-subtypes-by-card-types TYPES)))


        (otherwise nil))))))


;; ^
;; 
;; e.g. (‘looking-back’ "Creature —" (‘line-beginning-position’))
;;


;;


(defun mtg-subtypes-by-card-types (card-types)


  "Returns a union of the MTG Subtypes of CARD-TYPES.


Inputs:


• CARD-TYPE — a ‘symbolp’, or a ‘listp’ thereof.


Output:


• a ‘stringp’ ‘listp’.


See also function ‘mtg-subtypes-by-card-type’."


 (when-let* ((CARD-TYPES (if (listp card-types) card-types (list card-types))))


  (cl-loop for CARD-TYPE in CARD-TYPES
    append (mtg-subtypes-by-card-type CARD-TYPE)))))


;;


(defun mtg-subtypes-by-card-type (card-type)


  "Returns (custom and official) MTG Subtypes of CARD-TYPE.


Inputs:


• CARD-TYPE — a ‘symbolp’.


Output:


• a ‘stringp’ ‘listp’.


See:


• function ‘mtg-creature-subtypes’.
• function ‘mtg-spell-subtypes’.
• function ‘mtg-artifact-subtypes’.
• function ‘mtg-enchantment-subtypes’.
• function ‘mtg-land-subtypes’.
• function ‘mtg-planeswalker-subtypes’.
• ‘mtg-subtype-alist’."


  (or (and (eq 'creature card-type) (mtg-creature-subtypes))
      (and (or (eq 'instant card-type) (eq 'sorcery card-type)) (mtg-spell-subtypes))
      (and (eq 'artifact card-type) (mtg-artifact-subtypes))
      (and (eq 'enchantment card-type) (mtg-enchantment-subtypes))
      (and (eq 'land card-type) (mtg-land-subtypes))
      (and (eq 'planeswalker card-type) (mtg-planeswalker-subtypes))
      (assq card-type (mtg-subtype-alist))))


;;


(defun mtg-normalize-card-types (card-types)


  "Validate & Normalize CARD-TYPES.


Inputs:


• CARD-TYPES — a ‘symbolp’ ‘listp’. 


Output:


• a ‘symbolp’ ‘listp’ (a “Set”):


    • Ensure any constraints between card-types are satisfied (e.g. no “Creature Instant”).


    • ‘filter’ away unknowns (w.r.t. function ‘mtg-card-types’).


    • output is ‘sort’ed & ‘uniq’ue.


Examples:


M-: (mtg-normalize-card-types '(artifact creature enchantment))
  ↪ '(artifact enchantment creature)


M-: (mtg-normalize-card-types '(creature land))
  ↪ '(land creature)
  ; e.g. ‹Dryad Arbor›


M-: (mtg-normalize-card-types '(sorcery land))
  ↪ nil


M-: (mtg-normalize-card-types '(tribal sorcery))
  ↪ '(tribal sorcery)


M-: (mtg-normalize-card-types '(instant sorcery))
  ↪ nil


M-: (mtg-normalize-card-types '(planeswalker jaywalker planeswalker))
  ↪ '(planeswalker)


M-: (mtg-normalize-card-types '())
  ↪ nil"


 (when-let* ((CARD-TYPES (delq nil (delete-dups (copy-sequence card-types)))))


  (when (mtg-validate-card-types CARD-TYPES)


(cond ((memq 'creature CARD-TYPES) '(creature)) 
((memq 'tribal CARD-TYPES) '(creature))
(t CARD-TYPES)))))


;;


(defun mtg-validate-card-types (card-types)


  "Validate CARD-TYPES.


Inputs:


• CARD-TYPES — a ‘symbolp’ ‘listp’. 


Output:


• a ‘booleanp’."


  (cl-check-type card-types 'list)


  (and card-types


 (or (not (cl-some #'mtg-spell-card-type-p card-types)) (not (cl-some #'mtg-permanent-card-type-p card-types)))


 (or (not (cl-some #'mtg-permanent-card-type-p card-types)) (not (cl-some #'mtg-spell-card-type-p card-types)))


(or (not (memq 'tribal card-types)) (cl-some #'mtg-base-card-type-p card-types))))


;; ^ « (or (not x) y) » means “x implies y”.


;;


(defun mtg-normalize-card-types-for-subtypes (card-types)


  "Returns the CARD-TYPES.


Inputs:


• CARD-TYPES — a ‘symbolp’ ‘listp’. 


Output:


• a ‘symbolp’ ‘listp’:


    • symbol ‘creature’ dominates permanent card-types.
    • symbol ‘tribal’ extends spell card-types with symbol ‘creature’.
    • spell card-types are consolidated (as symbol ‘sorcery’).
    • ‘filter’s away unknowns (see function ‘mtg-card-types’).
    • output is ‘sort’ed & ‘uniq’ue (i.e. like a Set).


Examples:


M-: (mtg-normalize-card-types-for-subtypes '(instant))
  ↪ '(sorcery)


M-: (mtg-normalize-card-types-for-subtypes '(tribal sorcery))
  ↪ '(sorcery creature)


M-: (mtg-normalize-card-types-for-subtypes '(artifact enchantment creature))
  ↪ '(creature)


M-: (mtg-normalize-card-types-for-subtypes '(creature land))
  ↪ '(land creature)
  ; e.g. ‹Dryad Arbor›


M-: (mtg-normalize-card-types-for-subtypes '(artifact enchantment))
  ↪ '(artifact enchantment)


M-: (mtg-normalize-card-types-for-subtypes '(legendary planeswalker planeswalker planeswalker))
  ↪ '(planeswalker)"


 (let* ((CARD-TYPES (mtg-normalize-card-types card-types)))


  (cond ((memq 'creature CARD-TYPES) '(creature)) 
((memq 'tribal CARD-TYPES) '(creature))
(t CARD-TYPES))))


;;








(cl-defun mtg-creature-subtypes (&key scan)


  "Returns (custom and official) MTG Creature Subtypes.


Inputs:


• SCAN — a ‘booleanp’.


Output:


• a ‘stringp’ ‘listp’:


    • “Statically” — Merges the variable ‘mtg-creature-subtypes’ with the (buffer-local) variable ‘mtg-extra-creature-subtypes’.


    • “Dynamically” — unless SCAN is nil, this function also scans the ‘current-buffer’ for implicit creature types (i.e. words that are at the end of a “Creature —” Typeline). c.f. ‘abbrev-expand’ vs ‘dabbrev-expand’."






PARSING
















































FUNCTIONS




(defun mtg--quoted-p (pos)


  "Return non-nil if POS is inside a quoted string."


  (let ((quoted nil))
    (dolist (o (nth 9 (save-excursion (syntax-ppss pos))))
      (if (eq (char-after o) ?\`) (setq quoted t)))
    quoted))






(defsubst mtg-card/is-known-spell-p (card)


    (or (memq 'instant (mtg-card-cardtypes card))
(memq 'sorcery (mtg-card-cardtypes card)))


















;;


(defun mtg-get-reminder-text (keyword)


  "Return the Reminder Text of KEYWORD (and MTG Keyword Ability/Action).


Inputs:


• KEYWORD — a ‘symbolp’.
  (For convenience, KEYWORD can be a ‘stringp’, which is converted via ‘mtg-intern’.)


Example:


• M-: (mtg-get-reminder-text 'first-strike)
   ↪ \"(This creature deals combat damage before creatures without first strike.)\"
"


  (when-let* ((KEYWORD (cond ((symbolp keyword) keyword) ((stringp keyword) (mtg-intern keyword))))
  (SYMBOL  (mtg-intern KEYWORD 'mtg-keywords)))
         (OBJECT  (intern-soft SYMBOL)))


    (mtg-keyword-remindertext OBJECT)))


;;


(defun mtg-eldoc/reminder-text (keyword)


  ())










































































CONSTANTS


color-alias-list
u . blue


type-alias-list
i . instant


S . (or instant sorcery)
P . (or creature land artifact enchantment planeswalker)


;; e.g.
;;
;;*si ;; search for instants or sorceries
;;
;;*S ≈ *si ;; "Spell" 
;;*P ≈ *claep ;; "Permanent"
;; ;; ^ capitalized (1) to disambiguate and (2) because they're not cardtypes themselves.
;;












(defcustom mtg-fill-column 50


  "‘fill-column’ for ‘mtg-mode’.


a positive ‘integerp’, or nil.


Caveats:


• Variable-Pitch Font — ‘mtg-mode’ 
• Card Namesakes — “~” has a shorter ‘string-width’ than the card's name.


URL ‘’"


  :type '(choice (const :tag "No Fill-Column" nil) integer))


  :group 'mtg)


;; TODO emacs string-width variable-pitch font






(defconst mtg-known-hand-keywords


  '()


  "MTG Keywords active for cards in your hand/graveyard/exile zone (e.g. ‹Flash›).")


;; Changeling (This card is every creature type.)


(defconst mtg-known-stack-keywords '()


  "MTG Keywords active for spells on the stack (e.g. if ‹Uncounterable› were a keyword).")


(defconst mtg-known-battlefield-keywords


  '()


  "MTG Keywords active for permanents on the battlefield (e.g. ‹Indestructible›).")
















































TYPES


(deftype fontsize () `(or (integer 1 *) (float 0 1)))


(deftype mtg- ()  `(or …))


(deftype mtg-cost () `(list ))


(deftype mtg-mana-cost () `(list ))


(deftype mtg-cmc () `(integer 0 *))




























STRUCTS


make-mtg-edition


 (BLOCK (mtg-edition/intern-block .block))


 (CODE (mtg-edition/intern-code .code))


mtg-edition/intern-block


mtg-edition/intern-code








(mtg-keyword
name
reminder
id)


"MTG Keywords.


Fields (a.k.a. “Slots”):


• NAME     — Name of the keyword. This is how it appears in Rules Text (possibly capitalized). NAME is the identifier by which the Game Rules refer to this keyword (c.f. ID, the unique identifier by which ‘mtg.el’ refers to this.)


• REMINDER — Reminder Text for the keyword. Used to: ① automatically insert reminder text when writing custom cards (see ‘mtg-toggle-reminder-text-for-keyword-at-point’); ② display REMINDER when ‘point’ is at NAME (see ‘mtg-eldoc/reminder-text’). Can be a string, which is equivalent to a constant function. Can be a function, which outputs contextual Reminder Text given the card. For example, function can insert the card's converted mana cost (into the reminder text) by looking at the mana cost, e.g. to provide a context-sensitive Reminder Text for the ‹Transmute› keyword; or, it can decide between singular or plural by looking at the sentence which contains the keyword, e.g. ‹Fabricate›; or, it can just ignore its input, and return a constant string. For future compatibility, the function should be variadic (i.e. defined with ‘&rest’).


• ID       — A unique identifier for the keyword. It should be derived from NAME, e.g. by ‘mtg-intern’. It should be idiomatic elisp when interpreted as a (possibly-qualified) variable name.


Types:


• NAME     :: a ‘stringp’.
• REMINDER :: nil, or a ‘stringp’, or a ‘functionp’ from ‘mtg-card-p’ to ‘stringp’.
• ID       :: a ‘symbolp’.


(defconst mtg-keyword/devotion


)








(mtg-ability-word
name
pattern
id)


"MTG Ability Words. 


Fields (a.k.a. “Slots”):


• NAME     — Name of the ability word. This is how it appears in Rules Text (possibly capitalized).


• TEXT — Pattern Text for the ability word. Used to: ① automatically highlight the (inferred) pattern text (see ‘mtg-font-lock/pattern-text’); ② generate a skeleton which expands NAME into TEXT (see ‘mtg-skeleton/from-keyword’). Can be a string, which must have one-or-more ellipses (i.e. “…” in Unicode, or “...” in ASCII). The ellispes are placeholders, which represent the rest of the rules text in a paragraph (i.e. the non-pattern text). Can be a function which outputs contextual Pattern Text, given the card. For example, the function can return look at the card type to provide different text on spells versus on permanents, e.g. like ‹Landfall›.


• ID       — A unique identifier for the ability word. It should be derived from NAME, e.g. by ‘mtg-intern’. It should be idiomatic elisp when interpreted as a (possibly-qualified) variable name.


Types:


• NAME     :: a ‘stringp’.
• TEXT     :: nil, or a ‘stringp’, or a ‘functionp’ from ‘mtg-card-p’ to ‘stringp’.
• ID       :: a ‘symbolp’.








(defconst mtg-keyword/landfall


  (make-mtg-ability-word :name "landfall"
   :pattern #'mtg-keyword/landfall/pattern-text/get-by-cardtype))


(defun mtg-keyword/landfall/pattern-text/get-by-cardtype (card)


  (let* ((CARD-TYPE (mtg-card-types card))
(SPELL-TYPE? (memq CARD-TYPE mtg-cardtype/spell-types))


  (if SPELL-TYPE?  mtg-keyword/landfall/pattern-text/for-spells
mtg-keyword/landfall/pattern-text/for-permanents)))


(defconst mtg-keyword/landfall/pattern-text/for-permanents
  "Whenever a land enters the battlefield under your control, …")


  (defconst mtg-keyword/landfall/pattern-text/for-spells
  "If you had a land enter the battlefield under your control this turn, … instead")


































FUNCTIONS


(defvar mtg- ()


  `(or …))




an ‘obarrayp’






















































METRICS


;;==============================================;;


;; ‘cl-loop’ reducers:
;;
;; • thereis, always
;; • count
;; • minimize, maximize
;; • sum, product
;;


;;----------------------------------------------;;


(defun mtg-card-textual-metrics (&optional cards)


  " for .


• CARDS — an optional ‘sequencep’ of ‘mtg-card-p’s.
Defaults to ‘mtg-card-list’."


  (let* ((CARDS (or cards mtg-card-list)))


    (cl-loop for CARD being each element of CARDS


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


    ;;  _ (mtg-card- CARD) into _


      finally return _)))


;;----------------------------------------------;;


(defun mtg-card-numeric-metrics (&optional cards)


  " for .


• CARDS — an optional ‘sequencep’ of ‘mtg-card-p’s.
Defaults to ‘mtg-card-list’."


  (let* ((CARDS (or cards mtg-card-list)))


    (cl-loop for CARD being each element of CARDS


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      sum (mtg-card- CARD) into _
      product (mtg-card- CARD) into _


      minimize (mtg-card- CARD) into _
      maximize (mtg-card- CARD) into _


    ;;  _ (mtg-card- CARD) into _


      finally return _)))


;;----------------------------------------------;;


(defun mtg-card-cmc-summarize (&optional cards)


  "Return a summary of the .


• CARDS — an optional ‘sequencep’ of ‘mtg-card-p’s.
Defaults to ‘mtg-card-list’."


  (let* ((CARDS (or cards mtg-card-list)))


    (cl-loop for CARD being each element of CARDS


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


      sum (mtg-card- CARD) into _
      product (mtg-card- CARD) into _


      minimize (mtg-card- CARD) into _
      maximize (mtg-card- CARD) into _


    ;;  _ (mtg-card- CARD) into _


      finally return _)))




;;----------------------------------------------;;








;;==============================================;;


;;----------------------------------------------;;


;; e.g. “has this card been printed at common?” (/ “is it in the Pauper format?”):
;;
;; • (cl-loop for PRINTING being each element of mtg-card-printing-list with RARITY = (mtg-card-printing-rarity PRINTING) thereis (eq 'common RARITY))
;;
;; • (cl-loop for PRINTING being each element of mtg-card-printing-list with RARITY = (mtg-card-printing-rarity PRINTING) with EDITION = (mtg-card-printing-edition PRINTING) thereis (when (eq 'common RARITY) PRINTING))
;;


;;----------------------------------------------;;


(defun mtg-card-printing-summarize (&optional card-printings)


  "Return a summary of CARD-PRINTINGS.


• CARD-PRINTINGS — an optional ‘sequencep’ of ‘mtg-card-p’s.
Defaults to ‘mtg- card-list’."


  (let* ((CARD-PRINTINGS (or card-printings mtg-card-printing-list)))


    (cl-loop for PRINTING being each element of CARD-PRINTINGS


      _ (mtg-card- CARD) into UNIQUES
      _ (mtg-card- CARD) into _


      _ (mtg-card- CARD) into _
      _ (mtg-card- CARD) into _


    ;;  _ (mtg-card- CARD) into _


      finally return (list :names (cl-sort (cl-remove-duplicates UNIQUES))))))














;;----------------------------------------------;;


;;==============================================;;




















































QUERY


;;==============================================;;


;;==============================================;;


(defun mtg-string-distance (text card)


  "Return the Levenshtein Distance from TEXT to CARD's ‘mtg-card-name’."


  (let* ((TEXT (string-collate? text))
         (NAME (mtg-card-name card))
         )


    (string-distance TEXT NAME)))


;; ^ ‘string-distance’:
;;
;; • Returns the Levenshtein Distance between ‘stringp’s.
;; • See URL ‘https://en.m.wikipedia.org/wiki/Levenshtein_distance’.
;;
;; 


;;==============================================;;


defun mtg-query-match (query card)


;;----------------------------------------------;;


defun mtg-query-eval (query &optional constituents)


;;----------------------------------------------;;


(defconst mtg-care-query-builtin-constituents


`(


;; Accessors (Mechanical):


NAME ,#'mtg-card-name
COST mtg-card-cost
CMC mtg-card-cmc
COLORS mtg-card-colors
COLORIDENTITY mtg-card-color identity
SUPERTYPES mtg-card-supertypes
CARD-TYPES mtg-card-types
SUBTYPES   mtg-card-subtypes
RULES mtg-card-rulestext
POWER mtg-card-power
TOUGHNESS mtg-card-toughness
LOYALTY mtg-card-loyalty
SIDES mtg-card-sides


;; Accessors (Aesthetic):


PRINTINGS mtg-card-printings
RARITY mtg-card-rarity
SET mtg-card-edition
FLAVOR mtg-card-flavortext
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-


;; Computations, may be slow (c.f. Accessors, which are quick)...


KEYWORDS (or mtg-card-keywords (mtg-parse-card-keywords mtg-card-rulestext))
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-
 mtg-card-


;; Unary/Binary/.../Multinary Functions...
;;
;;


has-keyword #'mtg-card-has-keyword


has-type #'mtg-card-has-any-type
has-supertype #'mtg-card-has-supertype
has-card-type #'mtg-card-has-card-type
has-subtype #'mtg-card-has-subtype
...
has- #'mtg-card-has-


)


  ".")


;;----------------------------------------------;;


(defconst mtg-card-query-constituents


`(


TYPES  (append SUPERTYPES CARD-TYPES SUBTYPES)


;; Unary/Variadic Functions...
;;
;; e.g. enchant creature, enchant permanent.


ENCHANT (lambda (object) (and (memq 'aura SUBTYPES) (has RULESTEXT (rx-to-string `(bol (or "e" "E") ,(format "nchant %s" object) eol)))


;; Predicates:


LEGENDARY? (memq 'legendary SUPERTYPES)
SNOW?      (memq 'snow      SUPERTYPES)


LAND?        (memq 'land         CARD-TYPES)
ENCHANTMENT? (memq 'enchantment CARD-TYPES)
...
?      (memq '    CARD-TYPES)


;; Numeral Predicates include Vehicles (i.e. non-creature cards).


POWER?     (neq nil POWER)
TOUGHNESS? (neq nil TOUGHNESS)
LOYALTY?   (neq nil LOYALTY)


COLORLESS? (eq nil COLORS) 


WHITE?     (memq 'white COLORS)
BLUE?      (memq 'blue  COLORS)
BLACK?     (memq 'black COLORS)
RED?       (memq 'red   COLORS)
GREEN?     (memq 'green COLORS)


SIMIC? (and BLUE? GREEN?)
...
? (and ? ?)


FLYING?         (memq 'flying         KEYWORDS)
DOUBLE-STRIKE?  (memq 'double-strike  KEYWORDS)
INDESTRUCTIBLE? (memq 'indestructible KEYWORDS)
...
? (memq ' KEYWORDS)


COMMON?   (eq 'common   RARITY)
UNCOMMON? (eq 'uncommon RARITY)
RARE?     (eq 'rare     RARITY)
MYTHIC?   (eq 'mythic   RARITY)


;; BasicLandType-Subtype Predicates...


PLAINS?   (memq 'plains   SUBTYPES)
ISLAND?   (memq 'island   SUBTYPES)
SWAMP?    (memq 'swamp    SUBTYPES)
MOUNTAIN? (memq 'mountain SUBTYPES)
FOREST?   (memq 'forest   SUBTYPES)


;; Land-Subtype Predicates...


LOCUS?    (memq ' SUBTYPES)
...
? (memq ' SUBTYPES)


;; Enchantment-Subtype Predicates...


AURA?      (memq 'aura SUBTYPES)
SHRINE?    (memq ' SUBTYPES)
CURSE?     (memq ' SUBTYPES)
CARTOUCHE? (memq ' SUBTYPES)
SAGA?      (memq ' SUBTYPES)
...
? (memq ' SUBTYPES)


;; Artifact-Subtype Predicates...


EQUIPMENT? (memq 'equipment SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
...
? (memq ' SUBTYPES)


;; Spell-Subtype Predicates...


ARCANE? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
? (memq ' SUBTYPES)
...
? (memq ' SUBTYPES)


;; Creature-Subtype Predicates include Tribal (i.e. non-creature) cards.


ELF? (memq 'elf SUBTYPES)
...
? (memq ' SUBTYPES)


;; Measurements:


TYPES#      (length TYPES)
SUPERTYPES# (length SUPERTYPES)
CARD-TYPES# (length CARD-TYPES)
SUBTYPES#   (length SUBTYPES)


PRINTINGS#  (length PRINTINGS)


;; Abbreviations:


CC COLORS
CI COLORIDENTITY


TT TYPES
TP SUPERTYPES
TC CARD-TYPES
TB SUBTYPES


 )


  "


Mnemonics:


• “?” Suffix — means a ‘booleanp’.


• “#” Suffix — means an ‘integerp’.


• “!” Suffix — means a ‘booleanp’. impli s "exactness". e.g. symbol ‘BLUE?’ means “the card is blue”, symbol ‘BLUE!’ means “the card is blue and no other colors” (a.k.a. “the card's colors are exactly blue”).


Notes:


• ‘cdr’s can reference ‘mtg-card-query-builtin-constituents’, as well as other ‘car’s in ‘mtg-card-query-constituents’ itself.")


;;----------------------------------------------;;


(defcustom mtg-card-query-user-constituents '()


  "Extends ‘mtg-card-query-constituents’.")


;;----------------------------------------------;;


(defun mtg-card-query-constituents (&optional constituents)


  (append mtg-care-query-builtin-constituents
          constituents
          mtg-care-query-user-constituents
          mtg-care-query-constituents))


;;----------------------------------------------;;


;;==============================================;;


;;


;;


(defun mtg-query-cards (query &optional cards)


  "Query CARDS for ‘mtg-card-p’s matching QUERY.


• QUERY — an ‘mtg-card-query-p’s.


• CARDS — an optional ‘sequencep’ of ‘mtg-card-p’s. 
Defaults to the variable ‘mtg-cards’."


  (let* ((CARDS (or cards mtg-cards)))


    (cl-loop for CARD being the elements of CARDS
      if (mtg-match-card mtg-query-card CARD)
      collect CARD)))


;;


;;==============================================;;


;;




;;


(defvar mtg-match-card-query--card
  "‘mtg-match-card-query’ internal.")


;;


(defun mtg-match-card-query (query card)


  (let ((mtg-match-card-query--card card))


    (mtg-match-card-query-- query)))


;; ^ TODO ‘let’ or ‘setq’.


;;


(defun mtg-match-card-query-- (query)


  (let ()


    (pcase QUERY


      (`(has-type ,TYPES)
        (mtg-query/has-type TYPES mtg-match-card-query--card))


      (`(and ,@QUERIES)
        (cl-loop for QUERY in QUERIES
          always (mtg-match-card-query-- QUERY)))


      (`(or ,@QUERIES)
        (cl-loop for QUERY in QUERIES
          thereis (mtg-match-card-query-- QUERY)))


      (_ nil))))


;;


always condition
This clause stops the loop when the specified condition is nil. Unlike while, it stops the loop using return nil so that the finally clauses are not executed. If all the conditions were non-nil, the loop returns t:
          (if (cl-loop for size in size-list always (> size 10))
              (only-big-sizes)
            (some-small-sizes))


never condition
This clause is like always, except that the loop returns t if any conditions were false, or nil otherwise. 


thereis condition
This clause stops the loop when the specified form is non-nil; in this case, it returns that non-nil value. If all the values were nil, the loop returns nil.


;;==============================================;;






















































COMMANDS






(defun mtg-toggle-dwim (&optional point)


  "Toggle some text or glyphs at ‘point’ (“Do-What-I-Mean”).


Dispatches to:


• ‘mtg-toggle-reminder-text-for-keyword-at-point’ — Toggle on/off if ‘point’ is at a keyword (‘mtg-keyword-name’); toggle off if ‘point’ is inside ‘mtg-keyword-reminder’.


• ‘mtg-toggle-namesake’ — Toggle between “~” and the card's name, when ‘point’ is at a tilde (i.e. “~”) or the card name of the current card (i.e. ‘mtg-card-name-looking-backward’). For spell types, cycle between “~”, “this spell” and the card's name. 


• ‘mtg-cycle-symbol-display’ — How MTG Symbols are displayed: Cycle between ASCII (e.g. “{T}”), Unicode (e.g. “Ⓣ”), and Images (e.g. URL ‘’).


"


  (interactive (list
   (point)))


 (let* ((POINT (or point (point))))


   (mtg-toggle-reminder-text-for-keyword-at-point )))


































MENU
















MODE: IMENU


• keywords — mtg-keyword-name
• cards    — mtg-card-name








(defconst autoconf-definition-regexp


  "A\\(?:H_TEMPLATE\\|C_\\(?:SUBST\\|DEFINE\\(?:_UNQUOTED\\)?\\)\\)(\\[*\\(\\(?:\\sw\\|\\s_\\)+\\)\\]*"


  "")


(defvar autoconf-imenu-generic-expression


  `((nil ,autoconf-definition-regexp 1))


  "‘imenu-generic-expression’ for ‘mtg-mode’.")




















MODE: ELDOC




















MODE: ABBREV


(define-abbrev-table 'mtg-mode-abbrev-table


  `(("end"    "end"    ,#'mtg-indent-line :system t)
   )


  "Abbreviations for ‘mtg-mode’.


an ‘abbrev-table-p’.")
































MODE: SKELETON


(defmacro mtg-skeleton/from-keyword (keyword)


  ())






































CONSTANTS


;;


(defconst mtg-default-creature-type-race


  [
  ]


  "MTG Creature Types which are “races” (e.g. Goblin).


a ‘symbolp’ ‘vectorp’.


URL ‘’")


;;


(defconst mtg-default-creature-type-class


  [
  ]


  "MTG Creature Types which are “classes” (e.g. Wizard).


a ‘symbolp’ ‘vectorp’.


URL ‘’")


;;


(defconst mtg-default-creature-type-state


  [zombie]


  "MTG Creature Types which are “states” (e.g. Zombie).


a ‘symbolp’ ‘vectorp’.


For example, Zombies (represented by symbol ‘zombie’) can behave as either an MTG Race (c.f. ‘mtg-default-creature-type-race’) or an MTG Class (c.f. ‘mtg-default-creature-type-class’). Conversely, if a Creature Type is neither, it should not be explicitly registered (under any ‘mtg-default-creature-type-*’).


URL ‘https://mtg.gamepedia.com/Zombie’")


;;


(defvar mtg-creature-type-race-class-table


  (let* ((TABLE (make-hash-table :test #'eq :size (+ (length mtg-default-creature-type-race) (length mtg-default-creature-type-class) (length mtg-default-creature-type-state)))))


    (dolist (CREATURE-TYPE mtg-default-creature-type-race)
      (puthash CREATURE-TYPE 'race TABLE))


    (dolist (CREATURE-TYPE mtg-default-creature-type-class)
      (puthash CREATURE-TYPE 'class TABLE))


    (dolist (CREATURE-TYPE mtg-default-creature-type-state)
      (puthash CREATURE-TYPE 'state TABLE))


    TABLE)


  "MTG Creature Types which are “classes” (e.g. Wizard).


a ‘hash-table-p’ between ‘symbolp’s:


• each key — can be any MTG Creature Type (c.f. ‘mtg-creature-types’).
• each value — can be one of:


    • symbol ‘race’
    • symbol ‘class’
    • symbol ‘state’
    • nil


URL ‘’")


;;


;;


(defun mtg-creature-type-which-kind (creature-type)


  "Returns the “kind” of CREATURE-TYPE.


Inputs:


• CREATURE-TYPE — a ‘symbolp’, or a ‘stringp’ (which gets ‘intern’ed).


Output:


• a ‘symbolp’. By default, one of:


    • symbol ‘race’
    • symbol ‘class’
    • symbol ‘state’
    • nil


Examples:


• M-: (mtg-creature-type-which-kind 'wizard)
  → 'class
• M-: (mtg-creature-type-which-kind \"wizard\")
  → 'class
• M-: (mtg-creature-type-which-kind \"Wizard\")
  → 'class


URL ‘’"


  (let* ((CREATURE-TYPE (mtg-intern creature-type)))


    (gethash CREATURE-TYPE mtg-creature-type-race-class-table)))


;;






























PRETTY




























PARSING RULES TEXT


;; Activated Abilities (except Loyalty Abilities) are written as “<Cost>: <Effect>. [Activation_Instructions.]”




























README












































MAKEFILE




















TESTS
































BENCHMARKS














































TYPES / STRUCTS




;;----------------------------------------------;;
;;; Types --------------------------------------;;
;;----------------------------------------------;;


(cl-defstruct (mtg-card (:constructor mtg-card-create)
                        (:copier      nil))


  "`mtg-card' represents a (unique) “Magic: The Gathering” card.


Field Docs:


• NAME          — Card Name.
                  Normalized (via ‘mtg-normalize-card-name’) and Cached (via `intern').
• COST          — Mana Cost.
• TYPES         — Card Type(s).
• SUBTYPES      — Subtype(s). e.g. Creature Type(s), ‹Aura›, ‹Equipment›, etc.
• SUPERTYPES    — Supertypes(s). e.g. ‹Legendary›, ‹Snow›, etc.
• COLORS        — .
• RULES         — .
• POWER         — .
• TOUGHNESS     — .
• LOYALTY       — .
• CMC           — .
• COLORIDENTITY — .
• RULINGS       — .
• LEGALITY      — .
• SCRYFALL      — Scryfall Metadata, see URL `https://scryfall.com/docs/api/cards'.


Field Types:


• NAME          ∷ a `symbolp'.
• COST          ∷ a `listp' of `symbolp's.
• TYPES         ∷ a `listp' of `symbolp's.
• SUBTYPES      ∷ a `listp' of `symbolp's.
• SUPERTYPES    ∷ a `listp' of `symbolp's.
• COLORS        ∷ a `listp' of `symbolp's.
• RULES         ∷ a `stringp'.
• POWER         ∷ an `integerp', or `stringp'.
• TOUGHNESS     ∷ an `integerp', or `stringp'.
• LOYALTY       ∷ an `integerp', or `stringp'.
• CMC           ∷ a `natnump' (i.e. non-negative `integerp').
• COLORIDENTITY ∷ a `listp' of `stringp's and/or `symbolp's.
• RULINGS       ∷ a `stringp'.
• LEGALITY      ∷ a `stringp'.
• SCRYFALL      ∷ a `stringp'.


Related:


• `make-mtg-card' — Smart Constructor which further documents the type(s) of each field.


• URL `https://mtgjson.com/files/all-cards/' — Documents the JSON Schema for an MTG Card in MTGJSON."


  ;; Mechanical & Primary:


  (name          nil)
  (cost          nil)
  (types         nil)
  (subtypes      nil)
  (supertypes    nil)
  (colors        nil)
  (rules         nil)
  (power         nil)
  (toughness     nil)
  (loyalty       nil)


  ;; Mechanical & Secondary (i.e. can be derived from the “Primary” above):


  (cmc           0)
  (coloridentity nil)


  ;; Non-Mechanical & External (i.e. from an external resource, like a website):


  (rulings       nil)
  (legality      nil)
  (date          nil)
  (scryfall      nil)
  (uuid          nil))


;; M-: (mtg-card-create :name "" :cost "" :types "" :supertypes "" :subtypes "" :colors "" :rules "" :power "" :toughness "" :loyalty "" :cmc 1 :coloridentity "" :image "" :flavor "" :frame "" :layout "" :rarity "" :typeline "" :language "" :artist "" :rulings "" :legality "" :scryfall "")
;;  ⇒


;;TODO color cmc supertypes subtypes layout watermark collector language


;; TODO legality  'legal 'banned' 'restricted 'illegal
;; TODO color     'white 'blue 'black 'red 'green
;; TODO language  'en ...


;;----------------------------------------------;;


(cl-defun make-mtg-card (&key name
                              cost
                              types
                              subtypes
                              supertypes
                              colors
                              rules
                              power
                              toughness
                              loyalty
                              cmc
                              coloridentity
                              image
                              flavor
                              frame
                              layout
                              rarity
                              edition
                              typeline
                              language
                              artist
                              date-printed
                              is-reprint
                              rulings
                              legalities
                              date
                              date-released
                              scryfall
                              uuid)


  "Make an `mtg-card', with validation & defaulting.


Inputs:


• NAME          — a `stringp' or `symbolp'.
• COST          — a `stringp', or a `listp' of `stringp's.
• TYPES         — a `listp' of `stringp's.
• SUBTYPES      — a `listp' of `stringp's.
• SUPERTYPES    — a `listp' of `stringp's.
• COLORS        — a `listp' of `stringp's.
• RULES         — a `stringp'.
• POWER         — an `integerp', or `stringp'.
• TOUGHNESS     — an `integerp', or `stringp'.
• LOYALTY       — an `integerp', or `stringp'.
• CMC           — a `natnump' (i.e. non-negative `integerp').
• COLORIDENTITY — a `listp' of `stringp's and/or `symbolp's.
• FLAVOR        — a `stringp'.
• FRAME         — a `stringp' or `symbolp'.
• LAYOUT        — a `stringp' or `symbolp'.
• RARITY        — a `stringp' or `symbolp'.
• EDITION       — a `stringp' or `symbolp'.
• TYPELINE      — a `stringp' or `symbolp'.
• LANGUAGE      — a `stringp' or `symbolp'.
• IMAGE         — an ‘imagep’, or a `symbolp' (an Image Symbol, from ‘defimage’),
                  or a `stringp' (a URI, e.g. a file-path or website-adderess, with Image Content-Type).
• ARTIST        — a `stringp' or `symbolp'.
• RULINGS       — a `stringp'.
• LEGALITIES    — a p`listp' (a Property-List).
• SCRYFALL      — a `stringp'.


Output:


• an `mtg-card-p'.


Example:


• M-: (make-mtg-card)
    ⇒ (mtg-card-create)
    ⇒ #s(mtg-card nil nil nil nil nil nil nil nil nil nil 0 ...)


Links:


• URL `'


Related:


• wraps `mtg-card-create'
• calls `make-mtg-printing'"


  (let* ((NAME          (cl-typecase name
                          (symbol name)
                          (string (intern name))))
         (COST          cost)
         (TYPES         types)
         (SUBTYPES      subtypes)
         (SUPERTYPES    supertypes)
         (COLORS        colors)
         (RULES         rules)
         (POWER         power)
         (TOUGHNESS     toughness)
         (LOYALTY       loyalty)
         (CMC           cmc)
         (COLORIDENTITY coloridentity)
         (IMAGE         image)
         (FLAVOR        flavor)
         (FRAME         frame)
         (LAYOUT        layout)
         (BORDER        border)
         (RARITY        rarity)
         (EDITION       edition)
         (TYPELINE      typeline)
         (LANGUAGE      language)
         (ARTIST        artist)
         (DATE          date)
         (IDENTIFIERS   identifiers)
         (RULINGS       rulings)
         (LEGALITY      legality)
         (SCRYFALL      scryfall)


         (PRINTING (mtg-printing-create :image         IMAGE
                                        :flavor        FLAVOR
                                        :frame         FRAME
                                        :layout        LAYOUT
                                        :border        BORDER
                                        :rarity        RARITY
                                        :edition       EDITION
                                        :typeline      TYPELINE
                                        :language      LANGUAGE
                                        :artist        ARTIST))


         (CARD     (mtg-card-create :name          NAME
                                    :cost          COST
                                    :types         TYPES
                                    :subtypes      SUBTYPES
                                    :supertypes    SUPERTYPES
                                    :colors        COLORS
                                    :rules         RULES
                                    :power         POWER
                                    :toughness     TOUGHNESS
                                    :loyalty       LOYALTY
                                    :cmc           CMC
                                    :coloridentity COLORIDENTITY
                                    :printings     (if PRINTING (list PRINTING) nil)


                                    :date          DATE
                                    :identifiers   IDENTIFIERS
                                    :rulings       RULINGS
                                    :legality      LEGALITY
                                    :scryfall      SCRYFALL)))


    CARD))


;;==============================================;;


(cl-defstruct (mtg-printing
                (:constructor mtg-printing-create)
                (:copier      nil))


  "`mtg-printing' represents a single printing of a “Magic: The Gathering” card.


Field Docs:


• IMAGE         — .
• FLAVOR        — .
• FRAME         — .
• LAYOUT        — .
• RARITY        — .
• EDITION       — .
• TYPELINE      — .
• LANGUAGE      — .
• ARTIST        — the name of the artist who illustrated IMAGE.


• CARD          — the card for which this is a printing of.


Field Types:


• IMAGE         ∷ a `symbolp' (an Image Symbol, from ‘defimage’),
                  or a `stringp' (a URI, e.g. a file-path or website-adderess, with Image Content-Type).
• FLAVOR        ∷ a `stringp'.
• FRAME         ∷ a `stringp' or `symbolp'.
• LAYOUT        ∷ a `stringp' or `symbolp'.
• RARITY        ∷ a `stringp' or `symbolp'.
• EDITION       ∷ a `stringp' or `symbolp'.
• TYPELINE      ∷ a `stringp' or `symbolp'.
• LANGUAGE      ∷ a `stringp' or `symbolp'.
• ARTIST        ∷ a `stringp' or `symbolp'.


• CARD          — ∷ a `symbolp' or an `mtg-card-p'.


Related:


• `make-mtg-printing' — Smart Constructor which further documents the type(s) of each field.


• URL `https://mtgjson.com/files/all-cards/' — Documents the JSON Schema for an MTG Card Printing in MTGJSON."


  ;; Non-Mechanical & Internal (i.e. present on the card itself):


  (image         nil)
  (flavor        nil)
  (frame         nil)
  (layout        nil)
  (border        nil)
  (rarity        nil)
  (edition       nil)
  (typeline      nil)
  (language      nil)
  (artist        nil)


  (card          nil))


;; M-: (mtg--create :abbreviation ' :name "")
;;  ⇒ #s(mtg- )
;;----------------------------------------------;;


(cl-defun make-mtg-card (&key image flavor frame layout rarity edition typeline language artist date)


  "Make an `mtg-printing', with validation & defaulting.


Inputs:


• IMAGE         — a `symbolp' (an Image Symbol, from ‘defimage’),
                  or a `stringp' (a URI, e.g. a file-path or website-adderess, with Image Content-Type).
• FLAVOR        — a `stringp'.
• FRAME         — a `stringp' or `symbolp'.
• LAYOUT        — a `stringp' or `symbolp'.
• RARITY        — a `stringp' or `symbolp'.
• EDITION       — a `stringp' or `symbolp'.
• TYPELINE      — a `stringp' or `symbolp'.
• LANGUAGE      — a `stringp' or `symbolp'.
• ARTIST        — a `stringp' or `symbolp'.
• RULINGS       — a `stringp'.
• LEGALITY      — a `stringp'.
• SCRYFALL      — a `stringp'.


Output:


• an `mtg-printing-p'.


Example:


• M-: (make-mtg-printing)
    ⇒ (mtg-printing-create)
    ⇒ #s(mtg-printing nil nil nil nil nil nil nil nil nil nil 0 ...)


Links:


• URL `'


Related:


• wraps `mtg-printing-create'"


  )


;;----------------------------------------------;;


(cl-defun make-mtg-card-printing (&key name
                              cost
                              types
                              subtypes
                              supertypes
                              colors
                              rules
                              power
                              toughness
                              loyalty
                              cmc
                              coloridentity
                              image
                              flavor
                              frame
                              layout
                              rarity
                              edition
                              typeline
                              language
                              artist
                              date-printed
                              is-reprint
                              rulings
                              legalities
                              date
                              date-released
                              scryfall
                              uuid)


  "Make an `mtg-printing-p' for an `mtg-card-p', with validation & defaulting.


Inputs:


• NAME          — a `stringp' or `symbolp'.
• COST          — a `stringp', or a `listp' of `stringp's.
• TYPES         — a `listp' of `stringp's.
• SUBTYPES      — a `listp' of `stringp's.
• SUPERTYPES    — a `listp' of `stringp's.
• COLORS        — a `listp' of `stringp's.
• RULES         — a `stringp'.
• POWER         — an `integerp', or `stringp'.
• TOUGHNESS     — an `integerp', or `stringp'.
• LOYALTY       — an `integerp', or `stringp'.
• CMC           — a `natnump' (i.e. non-negative `integerp').
• COLORIDENTITY — a `listp' of `stringp's and/or `symbolp's.
• FLAVOR        — a `stringp'.
• FRAME         — a `stringp' or `symbolp'.
• LAYOUT        — a `stringp' or `symbolp'.
• RARITY        — a `stringp' or `symbolp'.
• EDITION       — a `stringp' or `symbolp'.
• TYPELINE      — a `stringp' or `symbolp'.
• LANGUAGE      — a `stringp' or `symbolp'.
• IMAGE         — a `symbolp' (an Image Symbol, from ‘defimage’),
                  or a `stringp' (a URI, e.g. a file-path or website-adderess, with Image Content-Type).
• ARTIST        — a `stringp' or `symbolp'.
• RULINGS       — a `stringp'.
• LEGALITIES    — a p`listp' (a Property-List).
• SCRYFALL      — a `stringp'.


Output:


• an `mtg-card-p'.


Example:


• M-: (make-mtg-card)
    ⇒ (mtg-card-create)
    ⇒ #s(mtg-card nil nil nil nil nil nil nil nil nil nil 0 ...)


Links:


• URL `'


Related:


• wraps `mtg-card-create'
• calls `make-mtg-printing'"


  (let* ((NAME          (cl-typecase name
                          (symbol name)
                          (string (intern name))))
         (COST          cost)
         (TYPES         types)
         (SUBTYPES      subtypes)
         (SUPERTYPES    supertypes)
         (COLORS        colors)
         (RULES         rules)
         (POWER         power)
         (TOUGHNESS     toughness)
         (LOYALTY       loyalty)
         (CMC           cmc)
         (COLORIDENTITY coloridentity)
         (IMAGE         image)
         (FLAVOR        flavor)
         (FRAME         frame)
         (LAYOUT        layout)
         (BORDER        border)
         (RARITY        rarity)
         (EDITION       edition)
         (TYPELINE      typeline)
         (LANGUAGE      language)
         (ARTIST        artist)
         (DATE          date)
         (IDENTIFIERS   identifiers)
         (RULINGS       rulings)
         (LEGALITY      legality)
         (SCRYFALL      scryfall)


         (PRINTING (mtg-printing-create :image         IMAGE
                                        :flavor        FLAVOR
                                        :frame         FRAME
                                        :layout        LAYOUT
                                        :border        BORDER
                                        :rarity        RARITY
                                        :edition       EDITION
                                        :typeline      TYPELINE
                                        :language      LANGUAGE
                                        :artist        ARTIST))


         (CARD     (mtg-card-create :name          NAME
                                    :cost          COST
                                    :types         TYPES
                                    :subtypes      SUBTYPES
                                    :supertypes    SUPERTYPES
                                    :colors        COLORS
                                    :rules         RULES
                                    :power         POWER
                                    :toughness     TOUGHNESS
                                    :loyalty       LOYALTY
                                    :cmc           CMC
                                    :coloridentity COLORIDENTITY
                                    :printings     (if PRINTING (list PRINTING) nil)


                                    :date          DATE
                                    :identifiers   IDENTIFIERS
                                    :rulings       RULINGS
                                    :legality      LEGALITY
                                    :scryfall      SCRYFALL)))


    CARD))


;;==============================================;;


(cl-defstruct (mtg-translations
               (:constructor mtg--create)
               (:copier      nil))


  abbreviation (name ""))


;; M-: (mtg--create :abbreviation ' :name "")
;;  ⇒ #s(mtg- )


;;==============================================;;


(cl-defstruct (mtg-edition
               (:constructor mtg-edition-create)
               (:copier      nil))


  abbreviation name (type 'expansion) (image nil))


;; M-: (mtg-edition-create :abbreviation 'abu :name "Alpha Beta Unlimited")
;;  ⇒ #s(mtg-edition abu "Alpha Beta Unlimited" nil)


;;==============================================;;


(cl-defstruct (mtg-block
               (:constructor mtg-block-create)
               (:copier      nil))


  abbreviation (name "") (editions '()))


;; M-: (mtg-block-create :abbreviation 'ia :name "Ice Age Block" :editions '())
;;  ⇒ #s(mtg-block ia "Ice Age Block" ())


;;==============================================;;


(cl-defstruct (mtg-ruling 
               (:constructor mtg--create)
               (:copier      nil))


  text (date nil))


;; M-: (mtg--create :abbreviation ' :name "")
;;  ⇒ #s(mtg- )


;;==============================================;;


(cl-defstruct (mtg-symbol
               (:constructor mtg-symbol-create)
               (:copier      nil))


  symbol abbreviation (image nil) (char nil))


;; M-: (mtg-symbol-create :symbol 'tap :abbreviation 'T :image 'mtg-tap-symbol-svg-image :char 'mtg-tap-symbol-char)
;;  ⇒ #s(mtg-symbol tap T mtg-tap-symbol-svg-image mtg-tap-symbol-char)


;;==============================================;;


(cl-defstruct (mtg-rarity
               (:constructor mtg-rarity-create)
               (:copier      nil))


  rarity abbreviation color)


;; M-: (mtg-rarity-create :rarity 'rare :abbreviation 'r)
;;  ⇒ #s(mtg-rarity rare r nil)












































;; IMPORTS...


;; `helm':


(progn
  (declare-function helm              "helm")
  (declare-function helm-marked-candidates "helm")
  (declare-function helm-build-sync-source "helm"))


;; `projectile':


(declare-function projectile-project-root "projectile")


;; Configs...


(require 'mtg)




;;; Makefile:


EmacsLint := $(Emacs) -Q --batch -f elisp-lint-files-batch


DISTFILE := $(PKG)-$(VERSION).tgz


dist:
        rm -f $(DISTFILE)
        git archive --format=tgz "--prefix=$(PKG)/" "--output=$(outdir)/$(DISTFILE)" "v$(VERSION)"


checklint: build
        $(EmacsLint) $(EL)


.PHONY: checklint


checkconf:
        $(Emacs) -q --load .emacs


.PHONY: checkconf


# checkdocs:
# .PHONY: checkdocs


















































HELM (MTG-QUERY)






;;; Helm MTG — Query Cards...


;; Namespace: ‘mtg-helm/query-card/*’
;;


;; Features: 
;;
;; • Highlighted Matches — highlight which part(s) of each result was matched by the query. e.g. if you search for « @flying » (i.e. “flying in the rules text”), then “flying” is (1) highlighted in each result, including multiple instances of the word (thus, the user can easily distinguish, for example, “flying” as a keyword on its own line, versus “flying” in the middle of a sentence; and (2) is always present, despite truncation (thus, even if flying comes at the end of several paragraphs rules text, the text before the search term is truncated (which isn't the default), so that the text of the search term is preserved.
;;


;; Constants (MTG Query):


(defconst mtg-helm/query-card/buffer-name-format


  "*Helm MTG * Query Cards * %s *"


  "Format for `mtg-helm/query-card/buffer-name'.")


;;




;;


(defconst mtg-card-default-providers-list


  '("Wizards")


  "Default “Providers” of MTG Cards.


“Wizards” is the only official publisher.


Extended by `mtg-card-extra-providers-list '.")


;; Macros (MTG Query):


;; Variables (MTG Query):


(defvar-local mtg-current-query nil)


;;


(defvar mtg-helm/query-card/history '())


;; Helm Commands (MTG Query):


;;;###autoload
(defun mtg-helm/query-cards ()


  (helm :sources '(mtg-helm/query-card/default-source)


  :buffer (mtg-helm/query-card/buffer-name)


  :prompt 'mtg-helm/query-card/prompt


  :history 'mtg-helm/query-card/history-list


  :mode-line mtg-helm/query-card/mode-line


  :group 'helm-mtg-query))


;;


    :multiline t


    :multimatch nil


;;


(defsubst mtg-helm/query-card/buffer-name ()


  (let* ((PROVIDERS-LIST  (or nil mtg-card-providers-list-default))
         (PROVIDERS-STRING (string-join PROVIDERS-LIST "/")))


    (mtg-helm/query-card/buffer-name-format PROVIDERS-STRING)))


;;


;;


(defun mtg-helm/query-card/modeline-function ()


  "Display (current) printings count & unqiues count in Modeline."


  ())


;;


;; Helm Sources (MTG Query):


(defvar mtg-helm/query-card/default-source


  (helm-build-sync-source :candidates '()


  :init #'mtg-helm/query-card/initialize
 
  :action 'mtg-helm/query-card/actions


  :persistent-action #'mtg-helm/query-card/persistent-action


  :match #'mtg-helm/query-cards/matcher


  :fuzzy-match nil


  :allow-dups nil


  :requires-pattern mtg-helm/query-card/requires-pattern


  :persistent-help "TODO"
)


  "Source of officially-printed MTG Cards, for `mtg-helm/query-cards'.")


;;


(defun mtg-helm/query-cards/matcher (candidate)


  (unless mtg-current-query
    (setq-local mtg-current-query (mtg-parse-query helm-pattern)))


  (let* ((QUERY mtg-current-query))


    ()))


;; Helm Actions (MTG Query):


(defvar mtg-helm/query-card/default-action-help "")


;; (defvaralias 'mtg-helm/query-card/default-action-help ')


(defalias 'mtg-helm/query-card/default-action #')


;;


(defcustom mtg-helm/query-card/actions


  (helm-make-actions mtg-helm/query-card/default-action-help #'mtg-helm/query-card/default-action


    "Copy name(s), newline-separated" #'mtg-helm/query-card/copy-names-action
    "Insert name(s), one-per-line" #'mtg-helm/query-card/insert-names-action


    "Copy whole card(s), newline-separated" #'mtg-helm/query-card/copy-cards-action
    "Insert whole card(s), one-per-line" #'mtg-helm/query-card/insert-cards-action


    "View card image(s), i.e. with artwork" #'mtg-helm/query-card/view-images-action


  ;; non-custom Card Provider only:


    "Browse non-custom card(s), with Scryfall" #'mtg-helm/query-card/browse-scryfall-action
    "Browse non-custom card(s), with Gatherer" #'mtg-helm/query-card/browse-gatherer-action


  ;; multiple Candidates only:


    "Define cards as a new Named Card List, e.g. so you can write a « ~stuff » query." #'mtg-helm/query-card/new-named-card-list-action


    )


  "Action List for `helm-mtg/card-names'."


  :type '(alist :key-type string :value-type function)


  :group 'helm-mtg-query)


;;


defun mtg-helm/query-card/default-persistent-action


;;


defun mtg-helm/query-card/persistent-action


;; Customization Groups (MTG Query):


(defgroup helm-mtg-query nil


  :prefix "helm-mtg/")
  :group 'helm-mtg)


;; Customizable Variables (MTG Query):


(defconst helm-mtg/card-names/prompt 


  "MTG Query: "


  "`helm-requires-pattern' for `mtg-helm/query-cards'."


  :type (string)
  :safe #'stringp


  :group 'helm-mtg-query)


;;


(defcustom mtg-helm/query-card/requires-pattern 2


  "`helm-requires-pattern' for `mtg-helm/query-cards'."


  :type (integer)
  :safe #'atom


  :group 'helm-mtg-query)


;;


(defcustom mtg-helm/query-card/history-list


  'mtg-helm/query-card/history


  "List Variable with Minibuffer History for `mtg-helm/query-card'."


  :type (variable :tag "List Variable")
  :safe #'symbolp


  :group 'helm-mtg-query)


;;


(defcustom mtg-helm/query-card/mode-line


  "① Press ‹F1› to... ② Press ‹C-j› to copy the selected/marked names. ③ Press ‹TAB› to list all actions."


  "`helm-mode-line' for `mtg-helm/query-cards'.")


;;


(defcustom mtg-helm/query-card/mtgjson-uri


  "mtg.json"


  "URI from which to read or fetch the MTGJSON data that populate `mtg-helm/query-card' candidates.


Valid Types:


• a `stringp'
• a `symbol', whose `symbol-value' is a Valid Type."


  :type (choice (file     :tag "File")
                (string   :tag "URL")
                (variable :tag "URI Variable"))
  :safe #'stringp


  :group 'helm-mtg-query)


;;




































;;; MTG Helm — Card Names...


;; Commands (Card Names):
















































REGEXP


URL ‘https://scryfall.com/docs/regular-expressions’


(1) The "Thing"ling cycle
(2) Cards that mention orcs, but not other words like sORCery or ORChard
(3) Creatures that have a tap ability with no other payment




(1)


Scryfall Regexp: « name:/^[^\s]+ling$/ t:shapeshifter »


MTGE Regexp: « (name (rx (char alnum) "ling" eow)) (type (has 'shapeshifter)) »


MTGE Wildcard: « ling$ &shapeshifter »


(2)


Scryfall:


« o:/\b(orc|orcs)\b/ or name:/\b(orc|orcs)\b/ or ft:/\b(orc|orcs)\b/ »


MTGE:


« 'orc' »


(3)














































ERRORS


;;; Error Types:


(progn


  (define-error 'mtg-error "Unknown MTG error")


  ;;


  (defconst mtg-error 'mtg-error


    "Parent Error for all MTG Error Types.


=== MTG Error Types ===


an MTG Error Type is an Error Type whose Parent Error Types include (transitively) symbol ‘mtg-error’. the standard MTG Error Types include:


• symbol ‘mtg-error’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’
• symbol ‘mtg-error-’


new MTG Error Types can be registered via `define-mtg-error' (or ‘define-error’)."))


;;


(defun mtg-error (&optional type data)


  "‘signal’ an MTG Error.


=== Signature ===


Inputs:


• TYPE — a `symbolp', an MTG Error Type (see variable ‘mtg-error’).
• DATA — an `objectp'. TYPE-specific, ‘format’table data (also see function ‘signal’).


Output: ‘signal’ doesn't return.


=== Examples ===


M-: (mtg-error)


M-: (mtg-error ')


M-: (mtg-error ')"


  (signal ))


;;


(defmacro define-mtg-error (error-name message &rest parent-error-symbols)


  "‘define-error’ for variable ‘mtg-error’s.


=== Examples ===


M-: (pp-macroexpand (define-mtg-error mtg-data \"No data\"))


 ;; (define-error 'mtg-data \"[MTG] No data\" mtg-error)


"


  (let* ((MESSAGE (concat "[MTG] " message))


  (CHILD-ERROR-SYMBOL (quote error-name))


  (PARENT-ERROR-SYMBOLS (cons mtg-error parent-error-symbols)))


    `(define-error ,CHILD-ERROR-SYMBOL ,MESSAGE ,PARENT-ERROR-SYMBOLS)))


;;


(define-mtg-error 'mtg-data "Data is unavailable or invalid. See function ‘mtg-fetch’ (via « M-x describe-function RET mtg-fetch RET »).")


;;












defstruct mtg-multiverse


"An MTG Multiverse is one-or-more MTG Sets and/or (standalone) MTG Cards.


=== Examples ===


Multiverses include:


• ✓ Wizards Multiverse — Official sets/cards from  WOTC (Wizards of the Coast®).
• ❌ Reddit Multiverse — [TODO] Thousands of custom cards from /r/custommagic, with hundreds of unique mechanics.
• ✓ Example Multiverse — Tutorial for writing your own custom set with ‘mtg.el’.


Within this package, all non-custom cards (i.e. those officially printed/spoiled by WOTC, at the time which this version of the package was released) are grouped under the “Wizards Multiverse”.


The “Example Multiverse” is a tutorial for ‘mtg.el’ (updated and tested against each release of the package). It groups a few sets/cards and a keyword, as a simple example of developing your own custom format (it's located at « ../examples/mtg-example-multiverse.el »."


The “Reddit Multiverse” has thousands of custom cards from /r/custommagic, hundreds of custom key words, and a few mature custom sets (e.g. Cyberpunk, Lorado). These are designed / developed / illustrated by many different people and/or collaboratively (sometimes their templating is edited for clarity or validity by myself, Sam Boosalis a.k.a. /u/spriosboosalis). Some cards are popular ones (e.g. by upvotes, by references); some are controversial ones (e.g. with many comments); and some are particularly interesting (to myself).


=== Laws ===


each Multiverse can be considered independent from each other Multiverse: each must be internally consistent, but can be externally inconsistent. For example, while my own custom Multiverse respects the namespace of the Wizards Multiverse, Wizards may later print a card with the same name as a card of mine; ‘mtg.el’ still works (e.g. ‘helm-mtg’ has a Helm Source for each registered ‘helm-mtg-multiverses’).


these “Multiverse Laws” include:


• Multiverse Namespace — each unique Card must have a unique Card Name. i.e. no other Card in a Multiverse can have an identical Card Name with another Card while differing with in any Rules Fields; e.g. different printings of a card can have different flavor text, but not different rules text."








;;


(defmacro let-mtg-card value &rest body)
  ""
  (let-struct 'mtg-card value @body))




uuid
e.g. "7eb0f276-5e32-5a1e-acfd-9b0ddc19b845"
A universal unique id (v5) generated by MTGJSON. Each entry is unique.


text
e.g. "Reach (This creature can block creatures with flying.)\nDeathtouch (Any amount of damage this deals to a creature is enough to destroy it.)"
Rules text of the card.


names
e.g. ["Nicol Bolas, the Ravager","Nicol Bolas, the Arisen"]
Names of each face on the card. Meld cards are listed in the order of CardA, Meld, CardB.


printings
e.g. ["M19","PM19"]
List of sets the card was printed in, in uppercase.


side
e.g. "a" 
Identifier of the side. Used on cards with multiple faces, such as flip, split, transform cards. Can be a, b, or c.








(defconst mtg-default-card-layouts
  '(normal split aftermath adventure flip transform meld leveler saga class mutate augment host)  
  “")


(deflist mtg-card-layouts


mtg-default-card-layouts














;;


(defalias 'mtg--deep-dot-search #let-alist--deep-dot-search)


;;


(defun mtg--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (mtg--remove-dot symbol))
         (name (symbol-name clean)))
    (if (string-match "\\`\\." name)
        clean
      (mtg--list-to-sexp
       (mapcar #'intern (nreverse (split-string name "\\.")))
       variable))))


(defun mtg--list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `cdr' `assq' on VAR."
  `(cdr (assq ',(car list)
              ,(if (cdr list) (mtg--list-to-sexp (cdr list) var)
                 var))))




(let-mtg-card )




































;;; SEQ.EL


;; ‘seq.el’ integration for ‘mtg’ types.








;;; Basic seq functions that have to be implemented by new sequence types
(defgeneric seq-elt (sequence n)
  "Return Nth element of SEQUENCE."
  (elt sequence n))


;; Default gv setters for `seq-elt'.
;; It can be a good idea for new sequence implementations to provide a
;; "gv-setter" for `seq-elt'.
(defmethod (setf seq-elt) (store (sequence array) n)
  (aset sequence n store))


(defmethod (setf seq-elt) (store (sequence cons) n)
  (setcar (nthcdr n sequence) store))




























































;;; MAP.EL


;; ‘map.el’ integration for ‘mtg’ types.


;; (cl-defstruct mtg-card


(defconst mtg-card-keys


  (cl-loop for (NAME . _) in (cl-struct-slot-info 'mtg-card) collect NAME)


  "")




— Function: cl-struct-sequence-type struct-type
This function returns the underlying data structure for struct-type, which is a symbol. It returns record, vector or list, or nil if struct-type is not actually a structure.


— Function: cl-struct-slot-info struct-type
This function returns a list of slot descriptors for structure struct-type. Each entry in the list is (name . opts), where name is the name of the slot and opts is the list of slot options given to defstruct. Dummy entries represent the slots used for the struct name and that are skipped to implement :initial-offset.


— Function: cl-struct-slot-offset struct-type slot-name
Return the offset of slot slot-name in struct-type. The returned zero-based slot index is relative to the start of the structure data type and is adjusted for any structure name and :initial-offset slots. Signal error if struct struct-type does not contain slot-name.


— Function: cl-struct-slot-value struct-type slot-name inst
Return the value of slot slot-name in inst of struct-type. struct and slot-name are symbols. inst is a structure instance. This routine is also a setf place. Can signal the same errors as cl-struct-slot-offset.


;; (defmethod seq- ((seq mtg-card)) 
;; (defmethod map- ((map mtg-card))












(defgeneric map-into (map type)
  "Convert the map MAP into a map of type TYPE.")


(defmethod map-into (map (_type (eql list))) (map-pairs map))


(defmethod map-into (map (_type (eql alist))) (map-pairs map))


(defmethod map-into (map (_type (eql plist)))
  (let ((plist '()))
    (map-do (lambda (k v) (setq plist `(,k ,v ,@plist))) map)
    plist))


(defmethod map-into (map (_ (eql card)))
  (map-pairs map))


;;


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map-insert (map key value))


(defmethod map-put! (map key value))




(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map-empty-p ((map mtg-card))
  nil)


(defmethod map-contains-key ((map mtg-card) key)
  (cl-case key ))


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())


(defmethod map- ((map mtg-card))
  ())












































;;; JSON:


;;==============================================;;


(defconst mtg-json--object-type 'hash-table
  "‘json-object-type’ for ‘mtg-json’.")


;;


(defconst mtg-json--array-type 'vector
  "‘json-array-type’ for ‘mtg-json’.")


;;


(defconst mtg-json--key-type 'string
  "‘json-key-type’ for ‘mtg-json’.")


;;


(defconst mtg-json--false :json-false
  "‘json-false’ for ‘mtg-json’.")


;;


(defconst mtg-json--null nil
  "‘json-null’ for ‘mtg-json’.")


;;


(defconst mtg- '
  "‘’ for ‘mtg-json’.")


;;==============================================;;




























;;==============================================;;


(cl-destructuring-bind


    (&key method params &allow-other-keys)


    (json-read "{\"method\":\"echo\", \"params\":[true,false], \"id\":123456789}")


  (pcase method
    ("echo" (message "%s" params))))


;;==============================================;;


URL ‘https://mtgjson.com/files/card-types/’




(cl-defun mtg-load-cards (&key )


  "Return an `mtg-cards' struct."


  TODO)




























;;==============================================;;


URL ‘https://mtgjson.com/structures/set/’




keyruneCode :: string
e.g. “KTK”
The matching keyrune code for Keyrune image icons.
URL ‘https://andrewgioia.github.io/Keyrune/’










releaseDate :: string
e.g. “2019-01-25”
The release date of the set, in ISO-8601 format (i.e. YYYY-MM-DD).


URL ‘https://www.iso.org/iso-8601-date-and-time-format.html’


TODO: "emacs" "ISO-8601"




;;==============================================;;






















(cl-defun mtg-intern (string)


  "‘intern’ STRING, idiomatically.


Inputs:


• STRING — a ‘stringp’.


Output:


• a ‘symbolp’.
• Casing — Casing is normalized (to ‘downcase’).
• Separators — Separator characters (including: whitespace, underscore, forward/backward slashes, etc) are replaced with hyphens. Hyphens are preserved.
• Alphabet — The alphabet is simplified to Latin without accents (e.g. “Lim-Dûl” becomes symbol ‘lim-dul’).
• Punctuation — Other characters (non-separator punctuation) are dropped.
(e.g. “Yawgmoth's” becomes symbol ‘yawgmoths’). Non-graphical characters are dropped too. 
• English — Currently, only English-language phrases are considered (i.e. for the official/canonical names of cards/sets). Eventually, I might try to guess the language from non-Latin scripts, and munge that language's casing/punctuating/quoting/etc conventions.
• Non-Injectivity — While it's possible that unique card names can be collapsed onto the same symbol (e.g. ). Luckily, most such card names are either “degenerate”, in that they'd be too similar to already existing ones, or “unconventional”, in that they don't follow the standard naming conventions / writing style. However, if necessary, you can circumvent the reference system induced by ‘mtg-intern’ by using ‘mtg-symbol-value’ (which wraps ‘symbol-value’).


Examples:


• M-: (mtg-intern \"Merfolk Looter\")
  → 'merfolk-looter
• M-: (mtg-intern \"Lim-Dûl the Necromancer\")
  → 'lim-dul-the-necromancer
• M-: (mtg-intern \"Yawgmoth's Will\")
  → 'yawgmoths-will
• M-: (mtg-intern \"from_the_vault\")
  → 'from-the-vault"


  (let* ((STRING (replace TODO (downcase string))))


    (intern STRING)))


;; M-: (mtg-intern "from_the_vault")
;; 'from-the-vault


;;


(cl-defun mtg-symbol-value (symbol)


  "Return SYMBOL's ‘symbol-value’,b


Inputs:


• STRING — a ‘symbolp’.


Output:


• an ‘objectp’. (Should be an ‘atomp’, or an ‘mtg-*-p’ type)."


  ())


;;


;;


(cl-defun mtg-get (symbol-or-string &key type)


  "Return the MTG Object references by SYMBOL-OR-STRING.


Inputs:


• SYMBOL-OR-STRING — a ‘symbolp’ or ‘stringp’.
‘stringp’s are pre-processed via ‘mtg-intern’.
‘symbolp’s are checked for boundedness (via ‘boundp’) and for “shadowedness” (via « (‘get’ 'mtg SYMBOL-OR-STRING) »).


Inputs (optional):


• TYPE — a ‘symbolp’ (often, a ‘typep’). These MTG-specific types/namespaces are recognized:


    • symbol ‘card’ — an ‘mtg-card-p’.


    • symbol ‘set’ — an ‘mtg-set-p’.


    • symbol ‘’ — an ‘mtg--p’.


Output:


• an ‘objectp’. (Should be an ‘atomp’, or an ‘mtg-*-p’ type)."


  ())


;;


;;
;;
















;;----------------------------------------------;;


(defun mtg-json/parse-rulings (list)


  "Parse an MTGJSON “rulings” LIST.


Input:


• LIST — a ‘listp’.


URL `https://mtgjson.com/structures/rulings/'"


  (cl-check-type list #'sequencep "a Json Array")


  (cl-loop for ITEM being each element of list
           collect (let-alist ITEM
                     (mtg-ruling :ruling .text :date .date))))


;;----------------------------------------------;;


















(cl-defun mtg-json/mtgjson/parse-multiverse (object &key)


  "Parse an MTG Multiverse from OBJECT, w.r.t. URL ‘https://mtgjson.com/files/all-sets/’."


Inputs:


• OBJECT — a ‘mapp’.


Output:


• an `mtg-multiverse-p'.


Calls:


• `mtg-json/mtgjson/parse-edition'."


  (let* (())


    ()))


;;


;;


(cl-defun mtg-json/mtgjson/parse-edition (object &key)


  "Parse an MTG Edition from OBJECT, w.r.t. URL ‘https://mtgjson.com/structures/set/’."


Inputs:


• OBJECT — a ‘mapp’.


Output:


• an `mtg-edition-p'.


Calls:


• `mtg-json/mtgjson/parse-card'."


  (let* ((ALIST (map-into 'list object))


  (let-alist ALIST


  (let* ((CARDS


  (cl-loop for CARD in (seq-into 'list .cards)


(TRANSLATED-SET-NAMES (mtg-json/mtgjson/parse-edition-name-translations .translations)  ; Translated set name by


  (SIZE  (or .baseSetSize (length CARDS)))


  (KIND (mtg-intern .type)  ; the kind of edition. ; e.g.; archenemy, box, core, commander, draft_innovation, duel_deck, expansion, from_the_vault, funny, masters, masterpiece, memorabilia, spellbook, planechase, premium_deck, promo, starter, token, treasure_chest or vanguard.


  (VERSION (concat .meta.version  ; SemVer specification of the MTGJSON build.
  "-"
.meta.date)  ; ISO-8601 date of the MTGJSON build.
  )))


(make-mtg-edition 


 :name  .name
 :code  .code
 :kind  KIND
 :block .block
 :date  .releaseDate  ; e.g. "2019-01-25" ; Release date in ISO-8601 format for the set.
 :size  SIZE


 :translations TRANSLATED-SET-NAMES ; Translated set name by language.


 :version VERSION


 :cards CARDS))))




;;----------------------------------------------;;


(cl-defun mtg-json/mtgjson/parse-card (object &key edition)


  "Parse an MTG Card from OBJECT.


Inputs:


• OBJECT  — a ‘mapp’.
  w.r.t. URL ‘https://mtgjson.com/structures/card/’.
• EDITION — a ‘stringp’ or ‘symbolp’, or nil.
  the MTG Edition which OBJECT was within, if any.


Output:


• an `mtg-card-p', with an `mtg-card-printing-p'."


  (let* (


(CMC .convertedManaCost)


(TRANSLATIONS (... .foreignData))


(FRAME (make-mtg-card-frame :version .frameVersion
:effect  .frameEffect))


(PRINTING (make-mtg-card-printing 


:edition EDITION-NAME
:rarity .rarity


:border .borderColor
:frame FRAME


:artist .artist


:translations TRANSLATIONS))


(CARD


(make-mtg-card


:colors   .colors
:colorids .colorIdentity


;;


:cmc CMC


;;


:legalities LEGALITIES


:rulings RULINGS


:printings (list PRINTING))))


  CARD)))


;;


;;


;;


(cl-defun mtg-json/mtgjson/parse-token (object &key)


  "Parse an MTG Token from OBJECT, w.r.t. URL ‘https://mtgjson.com/structures/token/’ ."


Inputs:


• OBJECT — a ‘mapp’.


Output:


• an `mtg-edition-p'.


Calls:


• `mtg-json/mtgjson/parse-card'."


  (let* ((ALIST (map-into 'list object))


  (let-alist ALIST


)))


::


;;==============================================;;












;;==============================================;;










;;==============================================;;


(cl-defstruct (mtg-rarity
               (:constructor mtg-rarity--make)
               (:copier      mtg-rarity--copy))


  "MTG Rarity (for ‘mtg-card-printing’s)."


  (name nil :type symbol)
  ;; ^ this type's “key” slot.


  (abbr        nil :type symbol)
  (description ""  :type string)
  (color nil :type (or color image))
  )


;; ‘rarity’
;; “Can be: basic, common, uncommon, rare or mythic.”


(deftype color ()


  `(satisfies color-values)


  "Type of valid Color Names (‘stringp’) or Color RGB (‘stringp’/‘listp’).")


;;----------------------------------------------;;


(defalias 'make-mtg-rarity #'mtg-rarity--create)


;;----------------------------------------------;;


(defconst mtg-default-rarity-list


  `(,(make-mtg-rarity :name 'common      :abbr 'C :color "black")
    (,(make-mtg-rarity :name 'uncommon    :abbr 'U :color "silver"))
    (,(make-mtg-rarity :name 'rare        :abbr 'R :color "gold")
    ,(make-mtg-rarity :name 'mythic      :abbr 'M :color "bronze")
    ;;
    ,(make-mtg-rarity :name 'timeshifted :abbr 'T :color "purple")
    ,(make-mtg-rarity :name 'basic :abbr 'B :color "black"))


  "Default ‘mtg-rarity-list’.")


;;----------------------------------------------;;


;;==============================================;;


(cl-defstruct (mtg-card-frame
               (:constructor mtg-card-frame--make)
               (:copier      mtg-card-frame--copy))


  "


=== Examples ===


• “Futureshifted” cards — have an ‘mtg-card-frame-version’ of symbol ‘future’.


• “Colorshifted” cards — have an ‘mtg-card-frame-effect’ of symbol ‘colorshifted’.


• “Timeshifted” cards — have an ‘mtg-card-frame-version’ of symbol ‘1993’[TODO doublecheck], and an ‘mtg-card-printing-rarity’ of symbol ‘timeshifted’. 
"


  (name    nil  :type (or null string))
  (version 2015 :type (or symbol integer))
  (effect  nil  :type (or null symbol))
 )


;; ‘frameEffect’
;;
;; “Values can be: colorshifted, compasslanddfc, devoid, draft, legendary, miracle, mooneldrazidfc, nyxtouched, originpwdfc, sunmoondfc or tombstone.”


;; ‘frameVersion’
;;
;; “Version of the card frame style. Can be: 1993, 1997, 2003, 2015 or future.”


;;----------------------------------------------;;


(cl-defun make-mtg-card-frame (&key name version effect)


  "Make an ‘mtg-card-frame’, with validation & defaulting."


  (let* ()
    (mtg-card-frame--make :name name :version version :effect effect)))


;;----------------------------------------------;;


(defconst mtg-default-card-frame


  (make-mtg-card-frame :name "Default" :version 2015 :effect nil)


  "The default/“modern” ‘mtg-card-frame’.")


;;==============================================;;


(cl-defstruct (mtg-card-frame-effect
               (:constructor mtg-card-frame-effect--make)
               (:copier      mtg-card-frame-effect--copy))


  (abbr        nil :type symbol)
  (name        nil :type (or null string))
  (description ""  :type string)


  (image      nil :type (or string image))
  (associates nil :type (list-of symbol))  ;; associated Keywords/Types.
 )


:abbr 'colorshifted :name "Colorshifted"


:abbr 'devoid :name "Devoid" :associates '(keyword devoid)


:abbr 'legendary :name "Legendary" :associates '(type legendary)


:abbr 'miracle :name "Miracle" :associates '(keyword miracle)


:abbr 'tombstone :name "Tombstone, Graveyard-matters" :associates '(keyword flashback)  ;; TODO mtg-tombstone-icon-keyword-list


:abbr 'draft :name "Draft-matters"


:abbr 'mooneldrazidfc :name "Moon Eldrazi, Double-faced Card"


:abbr 'nyxtouched :name "Nyx-touched"


:abbr 'originpwdfc :name "Origins Planeswalker, Double-faced Card"


:abbr 'sunmoondfc :name "Sun/Moon, Double-faced Card"


:abbr 'compasslanddfc :name "Compass Land, Double-faced Card"


;;----------------------------------------------;;


(defconst mtg-card-frame-effects


  '(colorshifted compasslanddfc devoid draft legendary miracle mooneldrazidfc nyxtouched originpwdfc sunmoondfc tombstone)


  "Known ‘mtg-card-frame-effect’s.


Type: a ‘listp’ of ‘symbolp’s.")


;;==============================================;;


(cl-defstruct (mtg-card-frame-version
               (:constructor mtg-card-frame-version--make)
               (:copier      mtg-card-frame-version--copy))


  (abbr        nil :type (or symbol integer))
  (name        nil :type string)  
  (description ""  :type string)


  (image      nil :type (or string image)) )


;;----------------------------------------------;;


(defconst mtg-card-frame-versions


  '(1993 1997 2003 2015 future)


  "Known ‘mtg-card-frame-version’s.


Type: a ‘listp’ of ‘symbolp’s.")




;;==============================================;;














;;==============================================;;


(cl-defstruct (mtg-card-layout
               (:constructor mtg-card-layout--make)
               (:copier      mtg-card-layout--copy))


  (name    nil :type string)
  (code    nil :type symbol))
  (faces#  1   :type (or natnum))
  (image   nil :type (or null string image))
 )


;;----------------------------------------------;;


(cl-defun make-mtg-card-layout (&key name code faces# image)


  "Make an ‘mtg-card-layout’, with validation & defaulting."


  ())


;;----------------------------------------------;;


(defconst mtg-normal-card-layout


  (make-mtg-card-layout :name "Normal" :code 'normal :faces# 1)


  "The “normal” ‘mtg-card-layout’.")


;;==============================================;;


(defconst mtg-card-layout-default mtg-normal-card-layout


  "Which ‘mtg-card-layout’ cards have by default.


Type: an ‘mtg-card-layout-p’.")


;;----------------------------------------------;;


(defconst mtg-default-card-layout-list


  `(,mtg-normal-card-layout
    ,(make-mtg-card-layout :name "" :code 'leveler)


    ,(make-mtg-card-layout :name "" :code 'double-faced :faces# 2)
    ,(make-mtg-card-layout :name "" :code 'flip :faces# 2)
    ,(make-mtg-card-layout :name "" :code 'meld :faces# 3)


    ,(make-mtg-card-layout :name "" :code 'aftermath :faces# 1)
    ,(make-mtg-card-layout :name "" :code 'split :faces# 2)


    ,(make-mtg-card-layout :name "" :code ' :faces# 1)
    ,(make-mtg-card-layout :name "" :code ' :faces# 1)
    ,(make-mtg-card-layout :name "" :code ' :faces# 1)
    ,(make-mtg-card-layout :name "" :code ' :faces# 1)
    ,(make-mtg-card-layout :name "" :code ' :faces# 1)


    token


    phenomenon
    plane
    scheme   
    vanguard
   )


  "Default ‘mtg-card-layout-list’.")


;;----------------------------------------------;;


(defvar mtg-card-layout-list mtg-default-card-layout-list


  "Kinds of MTG Card Layouts.")


;;----------------------------------------------;;


(defcustom mtg-card-layouts


  '(normal
    aftermath
    double-faced
    flip
    leveler
    meld
    phenomenon
    plane
    scheme
    split
    token
    vanguard
   )


  "Known Card Layouts.


Type: a ‘listp’ of ‘symbolp’s.


Each references an ‘mtg-card-layout’ in ‘mtg-card-layout-list’,
via its ‘mtg-card-layout-code’.


See also:


• variable ‘mtg-card-layout-list’."


  :type '(repeat (symbol :tag "Card Layout"))


  :safe #'listp
  :group 'mtg)


;; “Can be: normal, split, flip, transform, meld, leveler, saga, planar, scheme, vanguard, token, double_faced_token, emblem, augment, aftermath, or host. (If normal, it is usually omitted.)”


;;----------------------------------------------;;


(defun mtg-card-layout-how-many-faces (card-layout)


  "How many Card Faces CARD-LAYOUT has.


CARD-LAYOUT — an ‘mtg-card-layout-p’,
or a ‘symbolp’ referencing one (by ‘mtg-card-layout-code’)."


  (or (cl-case 
    aftermath
    double-faced
    flip
    leveler
    meld
    normal
    phenomenon
    plane
    scheme
    split
    token
    vanguard
    )
      nil))


;;==============================================;;


;;----------------------------------------------;;


         (PRINTING (mtg-printing-create :image         IMAGE
                                        :flavor        FLAVOR
                                        :frame         FRAME
                                        :layout        LAYOUT
                                        :border        BORDER
                                        :rarity        RARITY
                                        :edition       EDITION
                                        :typeline      TYPELINE
                                        :language      LANGUAGE
                                        :artist        ARTIST))


;;==============================================;;


(cl-defstruct (mtg-printing (:constructor mtg-printing--make)
                            (:copier     mtg-printing--copy))


  (language nil :type symbol)  ;; an ‘mtg-language-abbr’.
  (name     nil :type string)  ;; the printed name (not the Oracle name).
  (flavor   nil :type string)
  (typeline nil :type string)
  (artist   nil :type string)


  (image    nil :type (or string image))  ;; a URL (“indirect image”) or ‘imagep’ (“direct image”).
  (border   nil :type symbol)  ;; an ‘mtg-border-color-code’.
  (rarity   nil :type symbol)  ;; an ‘mtg-rarity-abbrev’.
  (edition  nil :type symbol)  ;; an ‘mtg-edition-abbrev’.
  (frame    nil :type symbol)  ;; an ‘mtg-frame-code’.
  (layout   nil :type symbol)  ;; an ‘mtg-card-layout-code’.


  (multiverseid nil :type (or integer string)))


;; :type (memq ())


;;----------------------------------------------;;


(defun mtg-json-parse-printing (object)


  ""


  (let* ((ENGLISH-PRINTING (_ object))


        (FOREIGN-PRINTING (let-alist object


    (cl-loop for FOREIGN-OBJECT in .foreignData
             with PRINTING = (copy-mtg-printing ENGLISH-PRINTING)


             do (let-alist FOREIGN-OBJECT
                  (setf (mtg-printing-language PRINTING) (mtg-language-abbr (mtg-get-language-by-name .language)))
                  (setf (mtg-printing-name PRINTING) .name)
                  (setf (mtg-printing-flavortext PRINTING) .flavorText)
                  (setf (mtg-printing-rulestext PRINTING) .text)
                  (setf (mtg-printing-typeline PRINTING) .type)
                (setf (mtg-printing- PRINTING) .)
                (setf (mtg-printing- PRINTING) .)


                ;; ^ Absent keys are nil (not errors), as ‘let-alist’ expands to ‘assq’s.


             collect PRINTING)))
         )


  ())


;;==============================================;;


(cl-defstruct (mtg-language
               (:constructor mtg-language--create)
               (:copier      nil))


  "Language in which MTG Cards have been printed (or can be).


URL ‘https://mtg.gamepedia.com/Printed_languages’"


  (abbr    nil :type symbol)
  (name    nil :type string)
  (endonym nil :type string)
  (flag    nil :type (or null string)))


  (-names  nil :type (list-of string))


;; M-: (mtg-language-create :language 'spanish :abbreviation 'es :endonym "Español")
;;  ⇒ #s(mtg-language spanish es "Español" nil)


;;----------------------------------------------;;


(cl-defun make-mtg-language (&key name abbr flag endonym
                                  names)


  "Construct an ‘mtg-language’, with validation & defaulting."


  (cl-assert (or name names) "either ‘:name’ or ‘:names’ is required.")


  (cl-check-type name 'string)
  (cl-check-type abbr 'symbol)
  (cl-check-type flag '(or null character string))
  (cl-check-type endonym 'string)


  (cl-check-type names '(list-of string))


  (cl-destructuring-bind (NAME . NAMES)
                         (append (when name (list name)) names))


    (let* ((FLAG (cl-typecase flag
                 (string    flag)
                 (character (make-string flag)))
         )


      (mtg-language--create :name NAME :abbr abbr :flag FLAG :endonym endonym :-names NAMES))))


;; (pcase-let* ((`(,NAME . ,NAMES) (append (if name (list name) ()) names)))) …)


;;==============================================;;


(defcustom mtg-languages


  '(en es fr it pt de ru ko ja zhs zht)


  "Known ‘mtg-language’s.


Type: a ‘listp’ of ‘symbolp’s;
each ‘symbolp’ must be an ‘mtg-language-abbr’ in ‘mtg-language-list’."


  :type '(repeat (symbol :tag "Language Abbreviation"))


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defconst mtg-language-default-list


  `(,(make-mtg-language :name 'english    :abbr 'en :endonym "English"   :flag "🇺🇸")
    ,(make-mtg-language :name 'german     :abbr 'de :endonym "Deutsch"   :flag "🇩🇪")
    ,(make-mtg-language :name 'french     :abbr 'fr :endonym "Français"  :flag "🇫🇷")
    ,(make-mtg-language :name 'italian    :abbr 'it :endonym "Italiano"  :flag "🇮🇹")
    ,(make-mtg-language :name 'spanish    :abbr 'es :endonym "Español"   :flag "🇲🇽")  ; by number of players.
    ,(make-mtg-language :name 'portuguese :abbr 'pt :endonym "Português" :flag "🇧🇷")  ; by number of players.
    ,(make-mtg-language :name 'japanese   :abbr 'jp :endonym "日本語"    :flag "🇯🇵")
    ,(make-mtg-language :name 'russian    :abbr 'ru :endonym "Русский"   :flag "🇷🇺")
    ,(make-mtg-language :name 'taiwanese  :abbr 'tw :endonym "繁體中文"  :flag "🇹🇼")
    ,(make-mtg-language :name 'korean     :abbr 'ko :endonym "한국어"    :flag "🇰🇷")
    ,(make-mtg-language :abbr 'zhs :endonym "简体中文"  :flag "🇨🇳" :names '("Chinese Simplified" "Chinese"))
    ,(make-mtg-language :name "Chinese Traditional" :abbr 'zht :endonym "TODO"  :flag "🇨🇳"))


  "Default ‘mtg-language-list’.")


;;----------------------------------------------;;


(defvar mtg-language-list mtg-language-default-list


  "All ‘mtg-language’s.


Type: a ‘listp’ of ‘mtg-language-p’s.


Language metadata (abbreviations and endonyms).")


;;----------------------------------------------;;


(defun mtg-get-language-by-name (name)


  "Return an ‘mtg-language’ whose ‘mtg-language-name’ matches NAME."


  (cl-loop for LANGUAGE in mtg-language-list


    with LANGUAGE-NAMES = (mtg-language-names LANGUAGE)


    if (cl-some (lambda (*NAME*) string-collate-equalp name *NAME*)) LANGUAGE-NAMES)
    return LANGUAGE))


;; M-: (mtg-language-abbr (mtg-get-language-by-name "Chinese Simplified"))
;;  ↪ 'zhs TODO
;;
;; M-: (mtg-language-abbr (mtg-get-language-by-name "Chinese Traditional"))
;;  ↪ 'zht
;;
;; M-: (mtg-language-abbr (mtg-get-language-by-name "Chinese"))
;;  ↪ 'zhs
;;


;;----------------------------------------------;;


(defun mtg-language-names (language &optional non-english)


  "Return all names of LANGUAGE.


Inputs:


• LANGUAGE — an ‘mtg-language-p’.


• NON-ENGLISH — a ‘booleanp’.
If NON-ENGLISH is non-nil, includes the ‘mtg-language-endonym’.


Output:


• a ‘listp’ of ‘stringp’s.
• Includes:


    • ‘mtg-language-name’ — The canonical exonym (in English).
    • ‘mtg-language-endonym’ — The language's primary endonym.
    • ‘mtg-language-aliases’ — Other names, in any language.


=== Examples ===


M-: (mtg-language-names 'zhs)
 ↪ \\='(\"Chinese Simplified\" \"Chinese\")


M-: (mtg-language-names 'zhs t)
 ↪ \\='(\"Chinese Simplified\" \"简体中文
\" \"Chinese\")
"


  (mtg-with-slots mtg-language (name endonym aliases) language


    (append (list name)
            (when non-english (list endonym))
            aliases)))


;;==============================================;;


;;----------------------------------------------;;


(defun mtg-json--parse-translations (object)


  "Parse an MTGJSON “foreignData” object.


Input:


• OBJECT — any mapping of type ‘json-object-type’, whose keys are any valid ‘json-key-type’ type.


URL ‘https://mtgjson.com/structures/foreign-data/’"


  (cl-check-type object #'mapp "a Json Object (c.f. ‘map.el’)")


  


  ())


;;----------------------------------------------;;


Chinese Simplified
Type
string
Example
"乙太之乱"
Description
Translation in Chinese Simplified.
Property#
Chinese Traditional
Type
string
Example
"乙太之亂"
Description
Translation in Chinese Traditional.
Property#
French
Type
string
Example
"La révolte éthérique"
Description
Translation in French.
Property#
German
Type
string
Example
"Äther-Rebellion"
Description
Translation in German.
Property#
Italian
Type
string
Example
"Rivolta dell’Etere"
Description
Translation in Italian.
Property#
Japanese
Type
string
Example
"霊気紛争"
Description
Translation in Japanese.
Property#
Korean
Type
string
Example
"에테르 봉기"
Description
Translation in Korean.
Property#
Portuguese (Brazil)
Type
string
Example
"Revolta do Éter"
Description
Translation in Portuguese (Brazil).
Property#
Russian
Type
string
Example
"«Эфирный Бунт»"
Description
Translation in Russian.
Property#
Spanish
Type
string
Example
"La revuelta del éter"
Description
Translation in Spanish.


;;----------------------------------------------;;


;;==============================================;;


;;


(json-false nil)
(json-null  nil)


;; ^ ‘json-null’ & ‘json-false’
;;
;; • in JSON, for optional fields under the MTGJSON schema, these coincide:
: «null» values, «false» values, and absent keys.
;;   in Elisp, we represent this equivalence by letting both ‘json-null’ and ‘json-false’ be nil, and by returning nil when looking up keys which aren't in the ‘listp’ / ‘hash-table-p’.
;;
;;






;;


(cl-defun mtg-json/parse-multiverse (object &key)


  "Parse OBJECT into an MTG Multiverse.


Inputs:


• OBJECT — a `hash-table-p'. [TODO a ‘mapp’]
  (See `mtg-json/schema').


Output: an `mtg-multiverse-p'.


Calls `mtg-json/parse-set'."


  (let* (())


    ()))


;;




(cl-defun mtg-json/parse-edition (object &key)


  "Parse OBJECT into an MTG Edition (a.k.a. “MTG Set”).


Inputs:


• OBJECT — a `hash-table-p'. [TODO a ‘mapp’]
  (See `mtg-json/schema').


Output: an `mtg-set-p'.


Calls `mtg-json/parse-card'."


  (let* (())


  (cl-loop for (KEY . VALUE) in OBJECT by #'


    ()))


;;


(cl-defun mtg-json/parse-card (alist &key)


  "Parse ALIST into an MTG Card.


Inputs:


• OBJECT — a `listp', an “alist”. [TODO a ‘sequence-p’]
  (See `mtg-json/schema').


Output: an `mtg-card-p'.


Calls `make-mtg-card'."


  (let-alist ALIST


    (make-mtg-card :name .name
: .
: .
: .
: .
: .
: .
: .
: .
: .
: .
: .
: .
: .
: .
:translations .
:rulings .
)))


;;




defalist mtg-json-schema-edition-alist




;;


defalist mtg-json-schema-card-alist


`(
(:name         . name)
:translations . (foreignData . ,#'mtg-json/parse-translations)
:rulings      . (rulings . ,#'mtg-json/parse-rulings)


Maps `keywordp's to `symbolp's:


* the ‘car’s are the “Kwarglist” of `make-mtg-card' (i.e. its `keywordp'-based Arglist).


* the ‘cdr’s are Json Keys of the ultimate / penultimate objects in `mtg-json-file' (where ‘json-key-type’ is symbol ‘symbol’)?


defalist mtg-json-card-translation-schema-alist


;;




NAME —


COST —


TYPES —


COLORS —


RULES —


POWER —


TOUGHNESS —


LOYALTY —


FACES —


SIDES —


RARITY —


EDITION —


FLAVOR —


ARTIST —


CMC a.k.a. CONVERTEDMANACOST —


CARDTYPES —


SUBTYPES —


SUPERTYPES —


CI a.k.a COLORIDENTITY —


;;


defalist mtg-json-card-ruling-schema-alist


(:ruling . text) ; e.g. "When Nicol Bolas’s enters-the-battlefield triggered ability resolves, first the next opponent in turn order (or, if it’s an opponent’s turn, that opponent) chooses a card in their hand without revealing it, then each other opponent in turn order does the same. Then all the chosen cards are discarded at the same time."
(:date   . date) ; e.g. "2018-07-13"


URL `https://mtgjson.com/structures/rulings/'


;;


defalist mtg-json-card-translation-schema-alist


URL `https://mtgjson.com/structures/foreign-data/'


'((foreignData name)         . name)       ; e.g. "Palladia-Mors, die Verwüsterin"


'((foreignData text)         . rules))       ; e.g. "Fliegend, Wachsamkeit, verursacht Trampelschaden Palladia-Mors, die Verwüsterin, hat Fluchsicherheit, falls sie noch keinen Schaden zugefügt hat."


'((foreignData flavorText)   . flavor))       ; e.g. "Schwester von Nicol Bolas. Überlebende des Krieges der Drachenältesten. Die bösartigste ihrer Art."


'((foreignData multiverseId) . multiverse)  ; e.g. 447637


'((foreignData language)     . language))       ; e.g. "German"


;;


defalist mtg-json-card-legality-schema-alist


URL `https://mtgjson.com/structures/legalities/'


;;
















  (cl-loop with PROGRESS-REPORTER = (make-progress-reporter
                      "collecting ucs names"
                      0 (length names))


        and do (progress-reporter-update PROGRESS-REPORTER count))


        finally (progress-reporter-done PROGRESS-REPORTER)))








;;; unicode-progress-reporter.el




(defvar unicode-progress-reporter-pulse-characters


  '(("Horizontal Blocks"
     "Left One Eighth Block"
     "Left One Quarter Block"
     "Left Three Eighths Block"
     "Left Half Block"
     "Left Five Eighths Block"
     "Left Three Quarters Block"
     "Left Seven Eighths Block"
     "Full Block")


    ("Moons"
     "New Moon Symbol"
     "Waxing Crescent Moon Symbol"
     "First Quarter Moon Symbol"
     "Waxing Gibbous Moon Symbol"
     "Full Moon Symbol"
     "Waning Gibbous Moon Symbol"
     "Last Quarter Moon Symbol"
     "Waning Crescent Moon Symbol")


    ("Vertical Blocks"
     "Lower One Eighth Block"
     "Lower One Quarter Block"
     "Lower Three Eighths Block"
     "Lower Half Block"
     "Lower Five Eighths Block"
     "Lower Three Quarters Block"
     "Lower Seven Eighths Block"
     "Full Block")


    ("Vertical Counting Rods"
     "Counting Rod Tens Digit One"
     "Counting Rod Tens Digit Two"
     "Counting Rod Tens Digit Three"
     "Counting Rod Tens Digit Four"
     "Counting Rod Tens Digit Five")


    ("Clocks"
     "Clock Face Twelve Oclock"
     "Clock Face One Oclock"
     "Clock Face Two Oclock"
     "Clock Face Three Oclock"
     "Clock Face Four Oclock"
     "Clock Face Five Oclock"
     "Clock Face Six Oclock"
     "Clock Face Seven Oclock"
     "Clock Face Eight Oclock"
     "Clock Face Nine Oclock"
     "Clock Face Ten Oclock"
     "Clock Face Eleven Oclock")


    ("Ogham Letters"
     "Ogham Letter Muin"
     "Ogham Letter Gort"
     "Ogham Letter Ngeadal"
     "Ogham Letter Straif"
     "Ogham Letter Ruis")


    ("Horizontal Counting Rods"
     "Counting Rod Unit Digit One"
     "Counting Rod Unit Digit Two"
     "Counting Rod Unit Digit Three"
     "Counting Rod Unit Digit Four"
     "Counting Rod Unit Digit Five")


    ("Triangles"
     "Black Up-Pointing Triangle"
     "Black Right-Pointing Triangle"
     "Black Down-Pointing Triangle"
     "Black Left-Pointing Triangle")


    ("ASCII"
     ?-
     ?\\
     ?|
     ?/
     )))












(defun helm-ucs-collect-symbols-alist (names)
  "Collect ucs symbols from the NAMES list."
  (cl-loop with pr = (make-progress-reporter
                      "collecting ucs names"
                      0 (length names))
           for (n . v) in names
           for count from 1
           for xcode = (format "#x%x:" v)
           for len = (length xcode)
           for diff = (- (car helm-ucs--max-len) len)
           for code = (format "(#x%x): " v)
           for char = (propertize (format "%c" v)
                                  'face 'helm-ucs-char)
           unless (or (string= "" n)
                      ;; `char-displayable-p' return a font object or
                      ;; t for some char that are displayable but have
                      ;; no special font (e.g 10) so filter out char
                      ;; with no font.
                      (not (fontp (char-displayable-p (read xcode)))))
           collect
           (concat code (make-string diff ? )
                   char "  " n)
           and do (progress-reporter-update pr count)))
























































;; 


;;


(defcustom mtg-bullet-chars
  '(?• ?*)
  "Characters that represent a bullet."


  :type '(choice
          (character :tag "Single display character")
          (repeat :tag "Multiple (possible) display characters" character))
  :safe #'listp
  :group 'mtg)


;;


(defun mtg-bullet-char () "Return a displayable ‘characterp’ (from ‘mtg-bullet-chars’)." (mtg--first-displayable mtg-bullet-chars))


;;


(defcustom mtg-dash-display-chars
  '(?─ ?━ ?-)
  "Characters that represent a (long) dash."


  :type '(choice
          (character :tag "Single display character")
          (repeat :tag "Multiple (possible) display characters" character))
  :safe #'listp
  :group 'mtg)


;;


(defun mtg-dash-char () "Return a displayable ‘characterp’ (from ‘mtg-dash-chars’)." (mtg--first-displayable mtg-dash-chars))


;;






;;


(defun mtg--first-displayable (seq)
  "Return the first displayable char/string in SEQ.


Inputs:


• SEQ — an ‘atomp’, or a ‘listp’ thereof.


Output:


• a ‘characterp’ or ‘stringp’.
It is 'char-displayable-p’."


  (let ((SEQ (if (listp seq) seq (list seq))))


    (cl-find-if #'mtg--char-or-string-displayable-p SEQ)))


;;


(defun mtg--char-or-string-displayable-p (char-or-string)
  "Whether CHAR-OR-STRING is displayable.


Inputs:


• CHAR-OR-STRING — a ‘characterp’ or ‘stringp’.


Output:


• a ‘booleanp’ (w.r.t. ‘char-displayable-p’)"


  (cond ((characterp char-or-string)
(char-displayable-p char-or-string))


((stringp char-or-string)
;; equivalent to ‘all’ for ‘char-displayable-p’:
(cl-loop CHAR across char-or-string
if (not (char-displayable-p CHAR))
then return nil
finally return t))))












































;;; PROGRESS REPORTER


mtg-docards
mtg-docards-with-progress-reporter




defmacro mtg-docards (spec &rest body)


  "


• SPEC — e.g.:


      • CARD
      • (CARD mtg-cards)
      • [name rulestext]
      • ([name rulestext] mtg-cards)


"


  (declare (indent 2)
           (debug ((&or symbolp vectorp [(&or symbolp vectorp) form &optional form]) body)))




defmacro mtg-docards-with-progress-reporter (spec reporter-or-message &rest body)


  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
















(defmacro dolist-with-progress-reporter (spec reporter-or-message &rest body)


  "Loop over a list and report progress in the echo area.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.
REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.
At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".


  (let ((prep (make-symbol "--dolist-progress-reporter--"))
        (count (make-symbol "--dolist-count--"))
        (list (make-symbol "--dolist-list--")))


    `(let ((,prep ,reporter-or-message)
           (,count 0)
           (,list ,(cadr spec)))


       (when (stringp ,prep)
         (setq ,prep (make-progress-reporter ,prep 0 (1- (length ,list)))))


       (dolist (,(car spec) ,list)
         ,@body


         (progress-reporter-update ,prep (setq ,count (1+ ,count))))


       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))










  (let ((PROGRESS-REPORTER (make-progress-reporter )))


    (cl-loop for CARD in mtg-cards
      do (progress-reporter-update PROGRESS-REPORTER "%s")


    (progress-reporter-done PROGRESS-REPORTER "%s")))


;;


(defun mtg-query-cards (query)


  (let ((PROGRESS-REPORTER (make-progress-reporter )))


    (cl-loop for EDITION in mtg-editions 


      append (cl-loop for CARD in EDITION 
               with RESULT = (funcall MATCH-QUERY CARD)
               if RESULT
               collect RESULT)


      do (progress-reporter-update PROGRESS-REPORTER "%s")
    )


    (progress-reporter-done PROGRESS-REPORTER "%s")))








































;;; SEARCH




(cl-defstruct mtg-card-match (:include mtg-card))


(cl-defstruct mtg-card-result (:include mtg-card))


;;




(cl-defmacro let-mtg-card (card &rest body)
  "`let-alist' for  `mtg-card-p's.
  `())


;;
;; e.g. ‘helm-buffers--pattern-sans-filters’:
;;
(defun helm-buffers--pattern-sans-filters (&optional separator)
  (let* ((SEPARATOR (or separator " ")))
    (cl-loop for PATTERN-MATCHING o in (helm-mm-split-pattern helm-pattern)
           unless (member (substring PATTERN 0 1) '("*" "/" "@" "!"))
           collect PATTERN into LIST
           finally return (string-join LIST SEPARATOR))))
;;


;;


(defun mtg-match-card-p (query card)


  "Whether QUERY matches CARD.


Inputs:


• QUERY — an `mtg-query-p'.
• CARD  — a `mtg-card-p'.


Output: a `booleanp'.


Related:


• `mtg-match-card-p' is faster than `mtg-match-card-p', but less informative.


Naming:


• `mtg-match-card' is to `mtg-match-card-p' as `string-match' is to `string-match-p'."


(let-mtg-query QUERY (.mtg-query-name)


 (let-mtg-card CARD (.mtg-card-name)


  (or (and .mtg-query-name (string-match-p .mtg-query-name .mtg-card-name))


      (and .mtg-query- (string-match-p .mtg-query- .mtg-card- ))


      (and .mtg-query- (string-match-p .mtg-query- .mtg-card- ))


      (and .mtg-query- (string-match-p .mtg-query- .mtg-card- ))


      (and .mtg-query- (string-match-p .mtg-query- .mtg-card- ))














  ())




;      (and .mtg-query- (string-match-p .mtg-query- .mtg-card- ))




;;


(defmacro cnf (var list)
  (cl-loop for e in list
           collect `(and (= 0 (% ,var ,e)) ,e) into conditions
           finally return `(or ,@conditions)))


(defun detect-unrolled (x)
  (with-detect x (2 3 5 7 11 13 17 19 23 29 31)))


;;


(deftype mtg-query* ()


  `(or mtg-query
       string
       function)


QUERY can be:


- an ‘mtg-query-p’.
- a ‘stringp’, parsed into an ‘mtg-query-p’.
- a ‘functionp’.


;;


(defun mtg-match-card (card query)


  "Match QUERY against CARD.


Inputs:


• CARD  — an `mtg-card-p'.
• QUERY — an `mtg-query-p'.


Output is either:


• nil     — i.e. QUERY doesn't match CARD.
• non-nil — i.e. /How/ QUERY matched CARD. Returns a copy of CARD, whose:


    • `stringp' fields may be `propertize'd  to annotate a matching sub-QUERY (in particular, with the semantic property ‘mtg-match’, which may be converted to display properties via `facep's like face ‘highlight’ or face ‘underline’).


    • `integerp' fields may be ...


    • `listp' fields may be ...


Related:


• `mtg-match-card' is more informative than `mtg-match-card-p', but slower.
•


Naming:


• `mtg-match-card' is to `mtg-match-card-p' as `string-match' is to `string-match-p'."


copy-mtg-card


  ())


;;


(defsubst mtg-card-providers-list ()


  (append mtg-default-card-providers-list mtg-extra-card-providers-list)


  "Known “Providers” of MTG Cards.


See:


• `mtg-extra-card-providers-list'.
• `mtg-default-card-providers-list'."




;;


(defcustom mtg-extra-card-providers-list


  '()


  "Additional “Providers” of MTG Cards.


Extends `mtg-default-card-providers-list'.


Register a provider to group their Custom MTG Cards. 


Different providers should be mutually exclusive (i.e. each card should be provided by a single provider), but can represent any scope/level Examples include: an individual designer, cards that are or were in a custom cube, a weekly design contest, a Custom Card forum, etc."


  :group 'mtg-cards)


;;


(cl-defun mtg-parse-query (query &key)


  "Parse QUERY into an `mtg-query-p'.


Inputs:


• QUERY — a `stringp'.
  User Input in the MTG Query format.
  (TODO see Info Node `MTG Query'.)
  
Output: an `mtg-query-p'."


  (let* (())


    ()))


;;


(defcustom mtg-displayed-newline-default


  "|"


  "How newlines are displayed for cards rendered on one-line.


See `mtg-display-card'.)


;;


(cl-defun mtg-display-card (card &key width displayed-newline no-properties-p no-unicode-p)


  "Display CARD on one-line, prettily.


Output: a `stringp'.


Inputs:


• CARD — an `mtg-card-p'.


• WIDTH — an optional, positive `integerp'.
  Output should be have a `string-width' of WIDTH or less, via:


    • Abbreviation — Replace longer phrases with their abbreviations
      e.g. “c.m.c.” for “converted mana cost”; e.g. “ueot” for “until end of turn”.
    • Truncation — If WIDTH is still too short, replace the suffixes of MTG Text Fields (including: Rules Text, Card Name, Flavor Text) with ellipses.


• DISPLAYED-NEWLINE — a `stringp' or `characterp'.
  Output has newlines. Thus, newline characters are replaced with DISPLAYED-NEWLINE.
  Defaults (when nil) to `mtg-displayed-newline-default' (which defaults to “|”, i.e. the VERTICAL BAR).


• NO-PROPERTIES-P — a `booleanp'.
  Output is `propertize'd (i.e. colored, bolded, etc) unless NO-PROPERTIES-P is non-nil.


• NO-UNICODE-P — a `booleanp'.
  Output has Non-Ascii Characters unless NO-UNICODE-P is non-nil (i.e. Output has only Ascii Characters if NO-UNICODE-P is nil).
  Unicode Characters can look better and be more concise."


  (let* ((DISPLAYED-NEWLINE (or displayed-newline mtg-displayed-newline-default)))


(defun haskell-interactive-mode-multi-line (expr)
  "If a multi-line expression EXPR has been entered, then reformat it to be:
:{
do the
   multi-liner
   expr
:}"
  (if (not (string-match-p "\n" expr))
      expr
    (let ((pre (format "^%s" (regexp-quote haskell-interactive-prompt)))
          (lines (split-string expr "\n")))
      (cl-loop for elt on (cdr lines) do
               (setcar elt (replace-regexp-in-string pre "" (car elt))))
      ;; Temporarily set prompt2 to be empty to avoid unwanted output
      (concat ":set prompt2 \"\"\n"
              ":{\n"
              (mapconcat #'identity lines "\n")
              "\n:}\n"
              (format ":set prompt2 \"%s\"" haskell-interactive-prompt2)))))




    ()))


;;


is:bikeland (alias cycleland, bicycleland)
is:bounceland (alias karoo)
is:canopyland (alias canland)
is:checkland
 is:dual
 is:fastland
 is:fetchland
is:filterland
 is:gainland
 is:painland
 is:scryland
 is:shadowland
 is:shockland
 is:storageland
 is:creatureland
is:triland
 is:tangoland (alias battleland)


;; ^ “is:bikeland (alias cycleland, bicycleland), is:bounceland (alias karoo), is:canopyland (alias canland), is:checkland, is:dual, is:fastland, is:fetchland, is:filterland, is:gainland, is:painland, is:scryland, is:shadowland, is:shockland, is:storageland, is:creatureland, is:triland, and is:tangoland (alias battleland).”


;; URL `https://scryfall.com/docs/syntax'
;;
;; URL `https://magic.wizards.com/en/articles/archive/making-magic/historic-story-2018-04-09'
;;
;; URL `https://nullprogram.com/blog/2016/12/11/'
;;


;; * Scryfall Syntax:
;; 
;;
;; ** Regular Expressions:
;;
;; You can use forward slashes // instead of quotes with the type:, t:, oracle:, o:, flavor:, ft:, and name: keywords to match those parts of a card with a regular expression.


Scryfall supports many regex features such as .*?, option groups (a|b), brackets [ab], character classes \d, \w, and anchors (?!), \b, ^, and $.


Forward slashes inside your regex must be escaped with \/.
;; 
;;
;; 
;; ** Shortcuts / Nicknames (/ Aliases / Abbreviations):
;;
;; The search system includes a few convenience shortcuts for common card sets:


You can find some interesting land groups with is:bikeland (alias cycleland, bicycleland), is:bounceland (alias karoo), is:canopyland (alias canland), is:checkland, is:dual, is:fastland, is:fetchland, is:filterland, is:gainland, is:painland, is:scryland, is:shadowland, is:shockland, is:storageland, is:creatureland, is:triland, and is:tangoland (alias battleland)


You can find all Masterpiece Series cards with is:masterpiece
;;
;;
;; ** Text Search:
;;
;; Use the o: or oracle: keywords to find cards that have specific phrases in their text box.
;;
;; You can put quotes " " around text with punctuation or spaces.
;;
;; You can use ~ in your text as a placeholder for the card’s name.
;;
;; This keyword usually checks the current Oracle text for cards, so it uses the most up-to-date phrasing available. For example, “dies” instead of “is put into a graveyard”.
;;
;; Use the fo: operator to search full Oracle text only, which includes reminder text.
;;
;;
;; ** Card Names, Exact:
;;
;; If you prefix words or quoted phrases with ! you will find cards with that exact name only.
;; 
;; This is still case-insensitive.
;;
;; 
;; ** Named Predicates (e.g. Multi-Faced Cards, “Batches”):
;;
;; > You can find cards that have more than one face with is:split (split cards), is:flip (flip cards), is:transform (cards that transform), is:meld (cards that meld), and is:leveler (cards with Level Up)
;;
;; Find cards that are cast as spells with is:spell. Find permanent cards with is:permanent, historic cards with is:historic, modal effects with is:modal, and vanilla creatures with is:vanilla.
;; 
;; 
;; ** Color:
;;
;; > You can find cards that are a certain color using the c: or color: keyword, and cards that are a certain color identity using the id: or identity: keywords.
;;
;; Both sets of keywords accepts full color names like blue or the abbreviated color letters w, u, r, b and g.
;;
;; You can use many nicknames for color sets: all guild names (e.g. azorius), all shard names (e.g. bant), all wedge names (e.g. abzan), and the four-color nicknames chaos, aggression, altruism, growth, artifice are supported.
;;
;; Use c or colorless to match colorless cards, and m or multicolor to match multicolor cards.
;;
;; 
;; ** Types:
;;
;; > Find cards of a certain card type with the t: or type: keywords. You can search for any supertype, card type, or subtype.
;;
;; 
;; ** Mana:
;;
;; > Use the m: or mana: keyword to search for cards that have certain symbols in their mana costs.
;;
;; This keyword uses the official text version of mana costs set forth in the Comprehensive Rules. For example, {G} represents a green mana.
;;
;; Shorthand for is allowed for symbols that aren’t split: G is the same as {G}
;;
;; However, you must always wrap complex/split symbols like {2/G} in braces.
;;
;; You can find cards of a specific converted mana cost with cmc, comparing with a numeric expression (>, <, =, >=, <=, and !=)
;;
;; You can filter cards that contain hybrid mana symbols with is:hybrid or Phyrexian mana symbols with is:phyrexian
;;
;; >You can use numeric expressions (>, <, =, >=, <=, and !=) to find cards with certain power, power/pow, toughness, toughness/tou, total power and toughness, pt/powtou, or starting loyalty, loyalty/loy.
;;
;;
;; ** Rarity:
;; 
;; Use r: or rarity: to find cards by their print rarity. You can search for common, uncommon, rare, mythic. You can also use comparison operators like < and >=.
;;
;; Use new:rarity to find reprint cards printed at a new rarity for the first time. You can find cards that have ever been printed in a given rarity using in: (for example, in:rare to find cards that have ever been printed at rare.)
;; 
;;
;; ** Sets/Blocks:
;;
;;
;; ** Format/Legality:
;;
;; Use the f: or format: keywords to find cards that are legal in a given format.


You can also find cards that are explicitly banned in a format with the banned: keyword and restricted with the restricted: keyword.


The current supported formats are: standard, modern, legacy, vintage, commander, future (future Standard), pauper, frontier, penny (Penny Dreadful), duel (Duel Commander), and oldschool (Old School 93/94).


You can use is:commander to find cards that can be your commander.


Finally, you can find cards on the Reserved List with is:reserved.
;;
;;
;; ** Dates / Reprints:
;;
;;
;; ** Languages:
;;
;;
;; ** Artist/Flavor/Watermark:
;;
;; Search for cards illustrated by a certain artist with the a:, art:, or artist: keywords.


Search for words in a card’s flavor text using the ft: or flavor: keywords.


Search for a card’s affiliation watermark using the wm: or watermark: keywords, or match all cards with watermarks using has:watermark.


For any of these, you can wrap statements with spaces or punctuation in quotes " ".


You can find cards being printed with new illustrations using new:art, being illustrated by a particular artist for the first time with new:artist, and with brand-new flavor text using new:flavor.
;;
;;
;;
;; ** Border/Frame/Foil/Resolution:
;;
;; Use the border: keyword to find cards with a black, white, silver, or borderless border.


You can find cards with a specific frame editiong using frame:1993, frame:1997, frame:2003, frame:2015, and frame:future. You can also search for particular frame-effects, such as frame:legendary, frame:colorshifted, frame:tombstone, frame:nyxtouched.


You can find cards with full art using is:full.


new:frame will find cards printed in a specific frame for the first time.


Each card is available in non-foil, in foil, or in both. You can find prints available in each with is:nonfoil and is:foil, or is:foil is:nonfoil to find prints (like most booster cards) available in both.


You can find cards in our database with high-resolution images using is:hires.
;;
;;
;; ** Logical Negation:
;;
All keywords except for include can be negated by prefixing them with a hyphen (-). This inverts the meaning of the keyword to reject cards that matched what you’ve searched for.


The is: keyword has a convenient inverted mode not: which is the same as -is:. Conversely, -not: is the same as is:.


Loose name words can also be inverted with -
;;
;;
;; Syntax for Display/Layout of Search Results:
;;
You can enter your display options for searches as keywords rather than using the controls on the page.


Select how duplicate results are eliminated with unique:cards, unique:prints (previously ++), or unique:art (also @@).


Change how results are shown with display:grid, display:checklist, display:full, or display:text.


Change how results are sorted with order:artist, order:cmc, order:power, order:toughness, order:set, order:name, order:usd, order:tix, order:eur, order:rarity, order:color, order:released, order:spoiled, or order:edhrec.


Select what printings of cards to preferentially show with prefer:oldest, prefer:newest, prefer:usd-low or prefer:usd-high (and the equivalents for tix and eur), or prefer:promo.


Change the order of the sorted data with direction:asc or direction:desc.
;;
;;






;; ‘mtg.el’ “Batches”…
;; 
;; Named Predicates:
;;
• Spell — Instant, Sorcery.
• Permanent — Land, Creature, Artifact, Enchantment, Planeswalker.
• Historic — Artifact, Legendary, Saga. (interestingly: Legendary is a super type, Artifact is a card type, and Saga is a sub type.)
•
• Arbor — Treefolk, Forest. ("Arbor" is Latin for "Tree".)
• Lupine — Wolf, Werewolf (+ Wolves and/or Werewolves). ("Lupine" is Latin for "Wolfy".)
• 
• 
• 
• 
;;
;; e.g. in ‘mtg-query’, « *historic » is equivalent to « *artifact;legendary;saga », 
;; a.k.a. « (has-type '(artifact legendary saga ) », 
;; a.k.a. « (or (has-cardtype '(artifact)) (has-supertype '(legendary)) (has-type '(saga))) ».
;;
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
;;


;; Example Queries:
;;
• « @SUBTYPE » — I used this to find most “Lords”, for the « ~lord » query. « SUBTYPE » is a variable, interpolated with any subtypes of the current card. For example, it finds all ‹Elves› that talk about other ‹Elves› in their Rules Text. When matching an ‹Elf› ‹Warrior›, « @SUBTYPE » expands to « (match-rules-text (rx-mtg (or "Elf" "Elves" "Warrior" "Warriors")) ». It finds “Generalized Lords” like ‹Eladamri, Lord of Leaves› (i.e. not just « TYPE [creatures] [you control] get +1/+1 »). NOTE it doesn't find, for example, ‹Soraya the Falconer›, which is a “‹Bird› Lord” that's a ‹Human›.
;;
• « @(rx-mtg (words (any TYPE) (| "or" "and" "and/or") (any TYPE))) » — I used this to find new “Batches”, for type queries like « *arbor » or « *lupine », and for text queries like « @arbor » or « @lupine ». e.g « *arbor » matches both ‹Dryad Arbor› and ‹Doran›; e.g « @arbor » matches ‹Treefolk Harbinger›.
;;
• Ambassadors — c.f. ‹Ambassador Oak›: “When ~ enters the battlefield, create a 1/1 green Elf Warrior creature token.”. 
;;
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
;;




(defconst mtg-normalize-card-name-regexp
"[-,' \f\t\n\r\v]+"


  "Regexp for splitting / joining in `mtg-normalize-card-name'.


Extends `split-string-default-separators' with punctuation for proper nouns (in English).")


(defvar mtg-card-name-obarray


  "‘obarray’ for interning Card Names.")




(defun mtg-intern-card-name (name)


"‘intern’s NAME (a Card Name) in `mtg-card-name-obarray'.


Inputs a `stringp'. Outputs a `symbolp'.


e.g...


M-: (mtg-intern-card-name \"Kiki-Jiki, Mirror Breaker\")
  'mtg/kiki-jiki-mirror-breaker"


  (let* ((STRING (mtg-qualify-card-name (mtg-normalize-card-name name))))


  (intern STRING mtg-card-name-obarray)))




(defun mtg-normalize-card-name (name)


"Normalizes NAME (a Card Name).


Inputs a `stringp'. Outputs a `stringp'."


  (downcase (join-string (save-match-data (split-string name mtg-normalize-card-name-regexp)) :omit-nulls ) "-")))




(defun mtg-qualify-card-name (name)


"Qualify NAME (a Card Name) under a package-specific (nonstandard) namespace.


Qualifies with symbol ‘mtg/’."


Inputs a `stringp'. Outputs a `stringp'."


  (declare (pure t))
 
  (concat "mtg/" name))


























































;;; SEARCH: INDEXING


;; Index ‘mtg-card’s by:
;;
;; • Name — on words. on chars?
;;
;; • Type — by each sub/super/card type. by each pair of cardtypes (e.g. Artifact Creature)? [TODO normalize the order of the card types; e.g. « *artifact,creature » and « *creature, artifact » should both be just as fast, and must find the same results obviously]. by each pair of subtype (in particular, each race/class, e.g. "Elf Warrior"). by some "cross-type tuples" (in particular, index on "Legendary" with each Cardtype and each Subtype)
;;
;; Card Color — by the “Powerset-of-Colors” (i.e. colorless, each color, each guild, each shard/wedge, each 4-color, 5-color).
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;;




























;;; SEARCH: CACHING




































































;;; RX


;; 


;;


(defun rx-and (form)
  "Parse and produce code from FORM.
FORM is of the form `(and FORM1 ...)'."
  (rx-check form)
  (rx-group-if
   (mapconcat (lambda (x) (rx-form x ':)) (cdr form) nil)
   (and (memq rx-parent '(* t)) rx-parent)))






(define-arx url-rx


    '((http     (seq bos (group "http") "://") )


      (https    (seq bos (group "https") "://") )


      (https?   (seq bos (group "http" (optional "s")) "://") )


      (protocol (seq bos (group (1+ (not (any ":")))) "://"))


      (host     (group (1+ (not (any "/")))))


      (path     (group "/" (1+ (not (any "?")))))


      (query    (seq "?" (group (1+ (not (any "#"))))))


      (fragment (seq "#" (group (1+ anything))))))












































JIT


(let ((lexical-binding t))
  (byte-compile #'foo))




































;;; Eldoc


;; 


;;












































;;; Toolbar:






































;;; Menubar:
 




































;;; Syntax


(defvar lua-sexp-alist '(("then" . "end")
                      ("function" . "end")
                      ("do" . "end")
                      ("repeat" . "until")))




























































;;; Indentation


(defun lua-indent-line ()
  "Indent current line for Lua mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (lua-comment-or-string-p)
        (setq indent (lua-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (lua-calculate-indentation nil))))


    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))


    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))


    indent))


;;


(defun lua-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (lua-string-p) (not lua-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)


    ;; At this point, we know that we're inside comment, so make sure
    ;; close-bracket is unindented like a block that starts after
    ;; left-shifter.
    (let ((left-shifter-p (looking-at "\\s *\\(?:--\\)?\\]\\(?1:=*\\)\\]")))
      (save-excursion
        (goto-char (lua-comment-or-string-start-pos))
        (+ (current-indentation)
           (if (and left-shifter-p
                    (looking-at (format "--\\[%s\\["
                                        (match-string-no-properties 1))))
               0
             lua-indent-level))))))


;;


(defun lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code."
  (save-excursion
    (let ((continuing-p (lua-is-continuing-statement-p))
          (cur-line-begin-pos (line-beginning-position)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (lua-calculate-indentation-override)


       (when (lua-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let* ((modifier
                 (lua-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))


       ;; 4. if there's no previous line, indentation is 0
       0))))


;;






;;




































































;;; Skeletons




































































;;; COMPANY


;; URL `http://sixty-north.com/blog/a-more-full-featured-company-mode-backend.html'


;;


(defconst mtg-words-default


  '()


  "Default list of all words used among Card Names, Rules Text, and Super/Sub/Card Types.")


;;


(defun company-mtg/doc-buffer (candidate)


"`:doc-buffer' for `company-mtg'."


  (pcase candidate
(NAME . KIND)


    (company-doc-buffer "")))


;;


;;


(defun mtg-company/propertize-card (text initials summary)


  (propertize text :initials initials :summary summary))


;; M-: (example/propertize-biography "alan" "AMT" "Alan Mathison Turing, OBE, FRS (/ˈtjʊərɪŋ/ tewr-ing; 23 June 1912 – 7 June 1954) was a British mathematician, logician, cryptanalyst, philosopher, pioneering computer scientist, mathematical biologist, and marathon and ultra distance runner.")


#("alan" 0 1 (:initials "AMT"
              :summary  "Alan Mathison Turing, OBE, FRS (/ˈtjʊərɪŋ/ tewr-ing; 23 June 1912 – 7 June 1954) was a British mathematician, logician, cryptanalyst, philosopher, pioneering computer scientist, mathematical biologist, and marathon and ultra distance runner."))


(defun mtg-company/render-annotation (text)


  "Render TEXT, extract `mtg-company/propertize-card' props."


  (format " [%s]" (get-text-property 0 :initials text)))






;; Imports...


(require 'company)


;; Constants...


(defconst sample-completions


  '(#("alan" 0 1
      (:initials
      "AMT"
      :summary
      (concat "Alan Mathison Turing, OBE, FRS (/ˈtjʊərɪŋ/ "
              "tewr-ing; 23 June 1912 – 7 June 1954) was a "
              "British mathematician, logician, cryptanalyst, "
              "philosopher, pioneering computer scientist, "
              "mathematical biologist, and marathon and ultra "
              "distance runner.")))


    #("john" 0 1
      (:initials
      "JVN"
      :summary
      (concat "John von Neumann (/vɒn ˈnɔɪmən/; December 28, "
              "1903 – February 8, 1957) was a Hungarian and "
              "American pure and applied mathematician, physicist, "
              "inventor and polymath.")))


    #("ada" 0 1
      (:initials
      "AAK"
      :summary
      (concat "Augusta Ada King, Countess of Lovelace (10 December "
              "1815 – 27 November 1852), born Augusta Ada Byron "
              "and now commonly known as Ada Lovelace, was an "
              "English mathematician and writer chiefly known for "
              "her work on Charles Babbage's early mechanical "
              "general-purpose computer, the Analytical Engine.")))


    #("don" 0 1
      (:initials
      "DEK"
      :summary
      (concat "Donald Ervin Knuth (/kəˈnuːθ/[1] kə-nooth; born "
              "January 10, 1938) is an American computer "
              "scientist, mathematician, and Professor Emeritus "
              "at Stanford University.")))))


;; Functions...


(defun sample-annotation (s)


  (format " [%s]" (get-text-property 0 :initials s)))


;;


(defun sample-meta (s)


  (get-text-property 0 :summary s))


;;


(defun sample-fuzzy-match (prefix candidate)


  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))


;; Company Backend...


(defun company-sample-backend (command &optional arg &rest ignored)


  "Company Backend sample with Fuzzy Matching, Popup Menu annotations, and Echo Area metadata."


  (interactive (list 'interactive))


  (case command


    (interactive (company-begin-backend 'company-sample-backend))


    (prefix (and (eq major-mode 'fundamental-mode)
                (company-grab-symbol)))


    (candidates
    (remove-if-not
      (lambda (c) (sample-fuzzy-match arg c))
      sample-completions))


    (annotation (sample-annotation arg))


    (meta (sample-meta arg))


    (no-cache 't)))


;;
















;; Tooltip Image:




(defun mtg-card-on-mouseover
(defun mtg-card-help-echo
(defun mtg-card-help-image




(defun mtg-card-on-mouseover (window buffer position)


  "Show ‘help-echo’ for the current card.


“Current” means “at ‘point’”, and “Card” can be:


• a card name, [TODO] for any supported language (see ‘mtg-language-alist’).
• a custom card being edited (in ‘mtg-mode’).
• a search result (in ‘mtg-table-mode’).


TODO If ‘tooltip-mode’ is on and ‘mtg-image-preview-in-tooltip’ says to
show an image preview, then do so.  Otherwise, show text help.


Customization (MTG):


• ‘mtg-tooltip-image-p’


Customization (Emacs):


• ‘tooltip-mode’
• ‘help-at-pt-display-when-idle’


Links:


URL ‘’"






























;; Capitalization in Card Names:
;;
;; • Articles, like “the”, are lowercase, unless they're the very first word, e.g.:
;;
;;     • « The Abyss »
;;     • « Advice from the Fae »
;;
;;
;;


































;;; IMAGES:


;;----------------------------------------------;;
;;; (PNG) Images: ---------------------------------;;
;;----------------------------------------------;;


;; ‘mtg-png’ provides these ‘imagep’s:
;;
;; • ‘mtg-png-*-symbol’ — png Images of MTG Symbols.
;;


(defimage mtg-png--symbol


  ((:type png :data ""))


  "(PNG) Image Specification for “{}”, the “ Symbol”.")


;;==============================================;;


(defimage mtg-png-tap-symbol


  ((:type png :data ""))
wget https://raw.githubusercontent.com/sboosali/mtg.el/eae281fb87b3305fdb1eba7a2130d9fac159ca80/images/mana/T.png


  "(PNG) Image Specification for “{T}”, the “Tap Symbol”.")


;;==============================================;;


;;----------------------------------------------;;
;;; (SVG) Images: ---------------------------------;;
;;----------------------------------------------;;


;; ‘mtg-svg’ provides these ‘imagep’s:
;;
;; • ‘mtg-svg-*-symbol’ — SVG Images of MTG Symbols.
;;
;; • ‘mtg-svg-*-icon’ — SVG Images of MTG Icons.
;;
;;


;;==============================================;;


mtg-symbol-list
:abbr 'T :image 'mtg-svg-tap-symbol


;;----------------------------------------------;;


(defun mtg-insert-image (symbol)


  "Insert an image of SYMBOL.


Output: a ‘listp’ of ‘stringp’s.


Inputs:


• SYMBOL — a ‘symbolp’.


Examples:


• M-: (mtg--string \"\")
   ↪ \\='()


  (declare (side-effect-free t))


  (cl-check-type text 'string)
  (cl-check-type  ')


  (cl-assert ())


  (let* (( ())
         ( ())
         )


    ()))


;;----------------------------------------------;;


;;==============================================;;


(defimage mtg-svg-tap-symbol


  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M85.332 59.918h-36.004l13.185-9.383c-4.898-3.887-10.566-5.828-16.984-5.828-3.211 0-5.414.613-6.59 1.836-1.184 1.227-1.777 3.445-1.777 6.654 0 8.873 4.563 18.34 13.691 28.396l-10.391 10.521c-12.09-14.705-18.129-27.844-18.129-39.424 0-6.928 2.086-12.447 6.27-16.545 4.18-4.098 9.746-6.148 16.668-6.148 8.453 0 17.664 3.215 27.641 9.635l7.728-13.182 4.692 33.468z' fill='#0D0F0F'/></g></svg>"))


  "(SVG) Image Specification for “{T}”, the “Tap Symbol”.")


;; e.g.
;;
;; M-: (insert-image mtg-svg-tap-symbol)
;;


;;----------------------------------------------;;


;;==============================================;;


;;----------------------------------------------;;


(defimage mtg-svg-blue-mana-symbol


  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g fill='none'><circle fill='#C1D7E9' cx='50' cy='50' r='50'/><path d='M67.488 83.719c-4.787 4.871-10.684 7.307-17.688 7.307-7.861 0-14.098-2.69-18.711-8.073-4.359-5.127-6.537-11.662-6.537-19.606 0-8.543 3.717-18.286 11.15-29.224 6.064-8.969 13.199-16.83 21.402-23.58-1.197 5.469-1.793 9.355-1.793 11.662 0 5.299 1.664 10.467 4.996 15.508 4.102 5.98 7.219 10.426 9.357 13.328 3.332 5.043 4.998 9.955 4.998 14.737.002 7.093-2.391 13.074-7.174 17.941zm-.129-27.362c-1.281-2.861-2.777-4.762-4.486-5.703.256.514.385 1.24.385 2.18 0 1.795-.512 4.357-1.539 7.689l-1.664 5.127c0 2.99 1.492 4.486 4.484 4.486 3.16 0 4.742-2.095 4.742-6.281 0-2.134-.64-4.632-1.922-7.498z' fill='#0D0F0F'/></g></svg>"))


  "Image Specification for “{U}”, the Blue-Mana Symbol (an ‘imagep’).")


;;==============================================;;


(defimage mtg-svg-two-generic-mana-symbol


  ((:type svg :data "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><g transform='translate(0 -1)' fill='none'><circle fill='#CAC5C0' cx='50' cy='50.998' r='50'/><path d='M77.442 71.103l-5.896 19.898h-48.991v-4.254c2.38-2.652 7.595-8.001 15.646-16.053 4.849-4.852 9.649-9.972 14.407-15.371 2.378-2.651 4.21-4.942 5.487-6.862 2.836-4.114 4.255-8.32 4.255-12.624 0-4.204-1.301-7.912-3.908-11.112-2.607-3.204-5.97-4.808-10.09-4.808-8.871 0-15.823 5.998-20.854 17.98l-4.395-1.647c5.947-16.829 15.321-25.249 28.131-25.249 6.313 0 11.687 2.149 16.124 6.448 4.439 4.3 6.656 9.604 6.656 15.92 0 8.052-4.617 15.918-13.858 23.601l-9.604 7.956c-6.131 5.127-11.212 9.929-15.231 14.412-.28.273-.826.916-1.647 1.921h25.521c3.932 0 6.907-.776 8.918-2.335 1.735-1.372 3.434-3.98 5.08-7.821h4.249z' fill='#0D0F0F'/></g></svg>"))


  "Image Specification for “{2}”, the Generic Two-Mana Symbol (an ‘imagep’).")


;;==============================================;;


(defimage mtg-svg--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{}”, the Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-white-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{W}”, the White Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


;;==============================================;;


(defimage mtg-svg-phyrexian--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{p/}”, the Phyrexian- Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-phyrexian--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{p/}”, the Phyrexian- Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-phyrexian-blue-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{p/U}”, the Phyrexian-Blue Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-phyrexian--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{p/}”, the Phyrexian- Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-phyrexian--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{p/}”, the Phyrexian- Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-phyrexian--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{p/}”, the Phyrexian- Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


;;==============================================;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-white-blue-hybrid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the White/Blue Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg---hyrbid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{/}”, the / Hybrid Mana Symbol (an ‘imagep’).")


;;==============================================;;


(defimage mtg-svg--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{}”, the Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


;;==============================================;;


(defimage mtg-svg-gray--hybrid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{2/}”, the Mono Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-gray-blue-hybrid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{2/U}”, the Monoblue Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-gray--hybrid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{2/}”, the Mono Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-gray--hybrid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{2/}”, the Mono Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


(defimage mtg-svg-gray--hybrid-mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{2/}”, the Mono Hybrid Mana Symbol (an ‘imagep’).")


;;----------------------------------------------;;


;;==============================================;;


(defimage mtg-svg--mana-symbol


  ((:type svg :data ""))


  "Image Specification for “{}”, the Mana Symbol (an ‘imagep’).")


;;==============================================;;


(defimage mtg-svg-_-symbol


  ((:type svg :data ""))


  "Image Specification for “{_}”, the _ Symbol (an ‘imagep’).")


;;==============================================;;


;;==============================================;;


(defimage mtg-svg-tombstone-icon


  ((:type svg :data "\
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg version="1.1" id="Layer_2" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
         width="198px" height="300px" viewBox="0 0 198 300" enable-background="new 0 0 198 300" xml:space="preserve">
<g>
        <path fill="#FFFFFF" d="M180.583,44.328C168.128,23.162,149.956,9.8,128.034,5.686c-0.106-0.021-32.277-5.075-32.406-5.09
                C92.38,0.2,89.031,0,85.677,0C59.252,0,37.062,11.888,21.506,34.378C7.437,54.719,0,82.072,0,113.481V251.67
                c0,2.396,0.789,4.727,2.246,6.631l28.59,37.399c2.071,2.71,5.289,4.3,8.704,4.3h147.512c6.047,0,10.949-4.894,10.949-10.931
                V116.463C198,88.632,191.979,63.688,180.583,44.328z"/>
        <path fill="#666666" d="M187.051,293.624H39.539c-1.422,0-2.764-0.662-3.627-1.792L7.323,254.433
                c-0.607-0.794-0.936-1.765-0.936-2.763V113.481c0-64.063,31.864-107.104,79.29-107.104c3.098,0,6.186,0.184,9.18,0.549
                l31.864,5.003c39.473,7.406,64.893,48.43,64.893,104.534v172.606C191.613,291.585,189.571,293.624,187.051,293.624z"/>
        <path fill="#CCCCCC" d="M83.294,15.557c-40.643,1.47-67.782,40.432-67.782,97.925v136.65l19.464,25.461V114.641
                C34.976,66.115,53.516,29.832,83.294,15.557z"/>
        <path fill="#CCCCCC" d="M44.101,284.515h138.389V114.641c0-57.802-27.807-96.637-69.195-96.637
                c-41.388,0-69.193,38.835-69.193,96.637V284.515z"/>
        <path fill="#999999" d="M152.986,87.146H73.604c-2.52,0-4.563-2.039-4.563-4.555c0-2.516,2.043-4.555,4.563-4.555h79.383
                c2.52,0,4.562,2.039,4.562,4.555C157.548,85.107,155.506,87.146,152.986,87.146z"/>
        <path fill="#999999" d="M136.3,121.762H90.289c-2.52,0-4.563-2.039-4.563-4.555c0-2.516,2.043-4.555,4.563-4.555H136.3
                c2.521,0,4.563,2.039,4.563,4.555C140.862,119.723,138.82,121.762,136.3,121.762z"/>
        <path fill="#999999" d="M152.986,171.863H73.604c-2.52,0-4.563-2.039-4.563-4.555c0-2.516,2.043-4.555,4.563-4.555h79.383
                c2.52,0,4.562,2.039,4.562,4.555C157.548,169.824,155.506,171.863,152.986,171.863z"/>
        <path fill="#999999" d="M138.629,211.944H87.96c-2.52,0-4.562-2.039-4.562-4.555c0-2.516,2.042-4.555,4.562-4.555h50.669
                c2.521,0,4.563,2.039,4.563,4.555C143.191,209.905,141.149,211.944,138.629,211.944z"/>
</g>
</svg>
\
"))


  "Image Specification for the “Tombstone Icon”.")


;;==============================================;;


(defimage mtg-svg-_-icon


  ((:type svg :data ""))


  "Image Specification for the “_ Icon”.")


(defimage mtg-svg-_-icon


  ((:type svg :data ""))


  "Image Specification for the “_ Icon”.")


;;==============================================;;


[TODO stippling]


defface :stipple


The background stipple, a bitmap.
Alternatively, the value can specify the bitmap directly, with a list of the form (width height data). Here, width and height specify the size in pixels, and data is a string containing the raw bits of the bitmap, row by row. Each row occupies (width + 7) / 8 consecutive bytes in the string (which should be a unibyte string for best results). This means that each row always occupies at least one whole byte.


;;==============================================;;




;;


(let* ((SVG (svg-create 400 400 :stroke-width 10))
       (SVG (prog1 SVG 
              (svg-gradient SVG "gradient1" 'linear '((0 . "red") (100 . "blue")))
              (svg-circle SVG 200 200 100 :gradient "gradient1"                              :stroke-color "green")))


      (IMAGE (svg-image SVG)))


  (insert-image IMAGE))


;;


mtg-default-image-height


;;==============================================;;


(defun markdown-display-inline-images (&optional beg end)
  "Add inline image overlays to image links in the buffer.
This can be toggled with `markdown-toggle-inline-images'
or \\[markdown-toggle-inline-images]."


  (interactive "r")


(BEG (or beg (point-min)))




  (unless (display-images-p)
    (error "Cannot show images"))


  (save-excursion
    (save-restriction
      (widen)


      (goto-char BEG)


      (while (re-search-forward markdown-regex-link-inline END t)


        (let ((start (match-beginning 0))
              (imagep (match-beginning 1))
              (end (match-end 0))
              (file (match-string-no-properties 6)))
          (when (and imagep
                     (not (zerop (length file))))
            (unless (file-exists-p file)
              (when (and markdown-display-remote-images
                         (member (downcase (url-type (url-generic-parse-url file)))
                                 markdown-remote-image-protocols))
                (setq file (markdown--get-remote-image file))))
            (when (file-exists-p file)
              (let* ((abspath (if (file-name-absolute-p file)
                                  file
                                (concat default-directory file)))
                     (image
                      (if (and markdown-max-image-size
                               (image-type-available-p 'imagemagick))
                          (create-image
                           abspath 'imagemagick nil
                           :max-width (car markdown-max-image-size)
                           :max-height (cdr markdown-max-image-size))
                        (create-image abspath))))
                (when image
                  (let ((ov (make-overlay start end)))
                    (overlay-put ov 'display image)
                    (overlay-put ov 'face 'default)
                    (push ov markdown-inline-image-overlays)))))))))))




(defun markdown-toggle-inline-images ()
  "Toggle inline image overlays in the buffer."
  (interactive)
  (if markdown-inline-image-overlays
      (markdown-remove-inline-images)
    (markdown-display-inline-images)))






















































































;;


defstring mtg-gatherer-url-multiverse-id-format


"https://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s"


NOTE Percent-Encoding: Percent Signs in the URL must be escaped (e.g. for “%20”, the Percent-Encoding SPC, write “%%20”; or use “+” if supported by the website, c.f. ‹application/x-www-form-urlencoded›).


;;


(defun mtg-open-gatherer-url (multiverse-id)
  "Open MULTIVERSE-ID at URL ‘gatherer.wizards.com/’.


MULTIVERSE-ID — a ‘stringp’ (‘format’ted via ‘mtg-gatherer-url-multiverse-id-format’), or a ‘natnump’, or a ‘symbolp’ (a String Variable or Natural Variable)."


  (when-let* ((MULTIVERSE-ID (cl-typecase multiverse-id
(number (number-to-string multiverse-id)) 
(symbol (symbol-value multiverse-id))
(string multiverse-id)))


(URL (format mtg-gatherer-url-multiverse-id-format MULTIVERSE-ID)))


    (browse-url-generic URL)))


;;










;;


defstring mtg-scryfall-url-multiverse-id-format


"https://api.scryfall.com/cards/multiverse/%d?format=%s&version=%s"


  "


Examples


M-: (format (standard-value 'mtg-scryfall-url-multiverse-id-format) 409574 'image 'art-crop)


;; URL ‘https://api.scryfall.com/cards/multiverse/409574?format=image&version=art_crop’"


;;


(cl-defun mtg-open-scryfall-url (multiverse-id &key format version)
  "Open MULTIVERSE-ID at URL ‘api.scryfall.com/’.


MULTIVERSE-ID — a ‘stringp’ (‘format’ted via ‘mtg-scryfall-url-multiverse-id-format’), or a ‘natnump’, or a ‘symbolp’ (a String Variable or Natural Variable).


FORMAT — a ‘symbolp’, either: symbol ‘’ or symbol ‘’ or symbol ‘’ or symbol ‘’.


VERSION — a ‘symbolp’, either: symbol ‘’ or symbol ‘’ or symbol ‘’ or symbol ‘’.


See URL ‘’."


  (when-let* ((MULTIVERSE-ID (cl-typecase multiverse-id
(number (number-to-string multiverse-id)) 
(string multiverse-id)
(symbol (symbol-value multiverse-id))))


(MULTIVERSE-ID* (cl-typecase MULTIVERSE-ID
(number (number-to-string MULTIVERSE-ID)) 
(string MULTIVERSE-ID)))


(SCRYFALL-FORMAT (mtg--recase-symbol-as-elisp (or format 'json)))


(SCRYFALL-VERSION (mtg--recase-symbol-as-elisp (or format nil)))


(URL (format mtg-scryfall-url-multiverse-id-format MULTIVERSE-ID* )))


    (browse-url-generic URL)))


The data format to return: json, text, or image. Defaults to json.


The image version to return when using the image format: small, normal, large, png, art_crop, or border_crop




https://api.scryfall.com/cards/multiverse/409574








(defun mtg--recase-symbol-as-elisp (symbol)


  "Convert SYMBOL along Emacs-Lisp naming conventions.


SYMBOL — a ‘symbolp’ (or ‘stringp’?).


Examples:


M-: (mtg--recase-symbol-as-elisp 'art_crop)
'art-crop


M-: (mtg--recase-symbol-as-elisp 'Thran)
'thran


See:


URL ‘https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html’"


  (when-let* ((SYMBOL symbol)
(STRING 
 (symbol-name SYMBOL))
((STRING* (mtg--recase-string-as-elisp SYMBOL)))
(SYMBOL* (intern STRING*)))
    SYMBOL*))


;;


(defun mtg--recase-string-as-elisp (string)


 "(Worker for ‘mtg--recase-symbol-as-elisp’)."


  (replace 
 (downcase string)))


;;












;; {"object":"card","id":"1b8de79d-5efe-4e21-8614-786689fcad58","oracle_id":"d21a89eb-7c5b-459a-acc7-12b20b13bf79","multiverse_ids":[409574],"mtgo_id":59695,"mtgo_foil_id":59696,"tcgplayer_id":110736,"name":"Strip Mine","lang":"en","released_at":"2015-10-02","uri":"https://api.scryfall.com/cards/1b8de79d-5efe-4e21-8614-786689fcad58","scryfall_uri":"https://scryfall.com/card/exp/43/strip-mine?utm_source=api","layout":"normal","highres_image":true,"image_uris":{"small":"https://img.scryfall.com/cards/small/front/1/b/1b8de79d-5efe-4e21-8614-786689fcad58.jpg?1562900778","normal":"https://img.scryfall.com/cards/normal/front/1/b/1b8de79d-5efe-4e21-8614-786689fcad58.jpg?1562900778","large":"https://img.scryfall.com/cards/large/front/1/b/1b8de79d-5efe-4e21-8614-786689fcad58.jpg?1562900778","png":"https://img.scryfall.com/cards/png/front/1/b/1b8de79d-5efe-4e21-8614-786689fcad58.png?1562900778","art_crop":"https://img.scryfall.com/cards/art_crop/front/1/b/1b8de79d-5efe-4e21-8614-786689fcad58.jpg?1562900778","border_crop":"https://img.scryfall.com/cards/border_crop/front/1/b/1b8de79d-5efe-4e21-8614-786689fcad58.jpg?1562900778"},"mana_cost":"","cmc":0.0,"type_line":"Land","oracle_text":"{T}: Add {C}.\n{T}, Sacrifice Strip Mine: Destroy target land.","colors":[],"color_identity":[],"legalities":{"standard":"not_legal","future":"not_legal","modern":"not_legal","legacy":"banned","pauper":"not_legal","vintage":"restricted","penny":"not_legal","commander":"legal","brawl":"not_legal","duel":"banned","oldschool":"not_legal"},"games":["mtgo","paper"],"reserved":false,"foil":true,"nonfoil":false,"oversized":false,"promo":false,"reprint":true,"variation":false,"set":"exp","set_name":"Zendikar Expeditions","set_type":"masterpiece","set_uri":"https://api.scryfall.com/sets/f6ccda04-e8ef-4260-8453-9408d788bacf","set_search_uri":"https://api.scryfall.com/cards/search?order=set&q=e%3Aexp&unique=prints","scryfall_set_uri":"https://scryfall.com/sets/exp?utm_source=api","rulings_uri":"https://api.scryfall.com/cards/1b8de79d-5efe-4e21-8614-786689fcad58/rulings","prints_search_uri":"https://api.scryfall.com/cards/search?order=released&q=oracleid%3Ad21a89eb-7c5b-459a-acc7-12b20b13bf79&unique=prints","collector_number":"43","digital":false,"rarity":"mythic","illustration_id":"4729ec53-1be1-4c3f-ab58-b456c96ac8b0","card_back_id":"0aeebaf5-8c7d-4636-9e82-8c27447861f7","artist":"Howard Lyon","border_color":"black","frame":"2015","full_art":false,"textless":false,"booster":true,"story_spotlight":false,"edhrec_rank":70,"prices":{"usd":null,"usd_foil":"99.41","eur":"77.86","tix":"2.81"},"related_uris":{"gatherer":"https://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=409574","tcgplayer_decks":"https://decks.tcgplayer.com/magic/deck/search?contains=Strip+Mine&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall","edhrec":"http://edhrec.com/route/?cc=Strip+Mine","mtgtop8":"https://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Strip+Mine"},"purchase_uris":{"tcgplayer":"https://shop.tcgplayer.com/product/productsearch?id=110736&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall","cardmarket":"https://www.cardmarket.com/en/Magic/Products/Singles/Zendikar-Expeditions/Strip-Mine?referrer=scryfall&utm_campaign=card_prices&utm_medium=text&utm_source=scryfall","cardhoarder":"https://www.cardhoarder.com/cards/59695?affiliate_id=scryfall&ref=card-profile&utm_campaign=affiliate&utm_medium=card&utm_source=scryfall"}}


Strip Mine
Land
{T}: Add {C}.
{T}, Sacrifice Strip Mine: Destroy target land.












































UNICODE


;; “Magic: the Gathering®”
;; 𝘔𝘢𝘨𝘪𝘤: 𝘵𝘩𝘦 𝘎𝘢𝘵𝘩𝘦𝘳𝘪𝘯𝘨®
;; 𝑀𝑎𝑔𝑖𝑐: 𝑡ℎ𝑒 𝐺𝑎𝑡ℎ𝑒𝑟𝑖𝑛𝑔®






?¦ ; “BROKEN BAR”




:chars "φʷ"
:chars "φᵘ"
:chars "φᵇ"
:chars "φʳ"
:chars "φᵍ"


:chars "φ¹"


:chars "𝟐ʷ"
:chars "𝟐ᵘ"
:chars "𝟐ᵇ"
:chars "𝟐ʳ"
:chars "𝟐ᵍ"








;; For displaying set/arithmetic/logical relations/operations in queries:


;;


?≔ ; is-defined-as (COLON EQUALS)
?≕ ; (EQUALS COLON)


;;


?∅ ; EMPTY SET (O slash)


;;


?∈ ; ELEMENT OF
?∉ ; NOT AN ELEMENT OF
?∋ ; CONTAINS AS MEMBER
?∌ ; DOES NOT CONTAIN AS MEMBER


;;


?⊂ ; SUBSET OF (Sideways U with cap to left)
SUPERSET OF (Sideways U with cap to right)        ⊃        &sup;         &‌#8835;        &‌#x2283;
NOT A SUBSET OF (Subset with Slash)        ⊄        &nsub;         &‌#8836;        &‌#x2284;
NOT A SUPERSET OF (Superset with slash)        ⊅        —        &‌#8837;        &‌#x2285;
SUBSET OF OR EQUAL TO (Subset with line below)        ⊆        &sube;         &‌#8838;        &‌#x2286;
SUPERSET OF OR EQUAL TO (Superset with line below)        ⊇        &supe;         &‌#8839;        &‌#x2287;
NEITHER A SUBSET OF NOR EQUAL TO        ⊈        —        &‌#8840;        &‌#x2288;
NEITHER A SUPERSET OF NOR EQUAL TO        ⊉        —        &‌#8841;        &‌#x2289;
SUBSET OF WITH NOT EQUAL TO        ⊊        —        &‌#8842;        &‌#x228A;
?⊋ ; SUPERSET OF WITH NOT EQUAL TO


;;


PLUS OR MINUS        ±


PLUS +
MINUS        −
DIVISION SIGN        ÷
TIMES X        ×


DIVISION SLASH ∕


TO-DO which looks nicer, for power/toughness?
1∕2  ; DIVISION SLASH
1/2  ; FORWARD SLASH


;;


NOT EQUALS        ≠        &ne;        &‌#8800;        &‌#x2260;


ALMOST EQUAL (ASYMPTOTIC)        ≈        &asymp;        &‌#8776;        &‌#x2248;


IDENTICAL TO (three lines)        ≡        &equiv;        &‌#8801;        &‌#x2261;
NOT IDENTICAL TO        ≢        —        &‌#8802;        &‌#x2262;


LESS THAN <
GREATER THAN >


LESS THAN OR EQUAL TO ≤
GREATER THAN OR EQUAL TO ≥


NOT LESS-THAN        ≮        &‌#8814;        &‌#x226E;
NOT GREATER-THAN        ≯        &‌#8815;        &‌#x226F;


NEITHER LESS-THAN NOR EQUAL TO        ≰        &‌#8816;        &‌#x2270;
NEITHER GREATER-THAN NOR EQUAL TO        ≱


LESS-THAN BUT NOT EQUAL TO        ≨        &‌#8808;        &‌#x2268;
GREATER-THAN BUT NOT EQUAL TO        ≩        &‌#8809;        &‌#x2269;


;;


?¬ ; NOT SYMBOL (Corner)


?∀ ; FOR ALL (Upside-down A)
?∃ ; THERE EXISTS (Backwards E)
?∄ THERE DOES NOT EXIST (Backwards E with slash)


;;










;; default ?⋕ ; ?# ?⋕
;; alist   '((?№ . (fr)))










to-do:


ℕ  ; NATURAL NUMBERS (Doublestruck N)


ℤ  ; INTEGERS (Doublestruck Z)


ℙ  ; PRIME NUMBERS (Doublestruck P)


ℚ  ; RATIONAL NUMBERS (Doublestruck Q)


ℝ  ; REAL NUMBERS (Doublestruck R)
ℜ  ; REAL NUMBERS A (Blackletter R)        


ℂ  ; COMPLEX NUMBERS (Doublestruck C)


℘  ; WEIERSTRASS POWER SET (Script Capital P)








































;;; HELM:


;; Helm MTG — All Names...


;; Helm Commands (MTG Names):




(defun helm-mtg/names ()


  "Helm Command for all names of Cards/Sets/Keywords/Artists/Colors/Formats/…


Merges these Helm Sources:


• `helm-mtg/-names/default-source'
• `helm-mtg/card-names/default-source'
• `helm-mtg/edition-names/default-source'
• `helm-mtg/keyword-names/default-source'
• `helm-mtg/subtype-names/default-source'
• `helm-mtg/cardtype-names/default-source'
• `helm-mtg/supertype-names/default-source'
•
• `helm-mtg/color-names/default-source'
• `helm-mtg/rarity-names/default-source'
`helm-mtg/card-layout-names/default-source'
• `helm-mtg/card-frame-names/default-source'
• `helm-mtg/artist-names/default-source'
• `helm-mtg/watermark-names/default-source'
•
• `helm-mtg/block-names/default-source'
• `helm-mtg/format-names/default-source'"


(helm :sources '()


 :buffer "*Helm MTG Names*"))








(defconst helm-mtg/card-name-prompt "Card Name: ")


(defvar 'helm-mtg/card-name-history '())


(defcustom helm-mtg/card-name-actions


  '(("" . t)
    ("" . t))


  "Action List for `helm-mtg/card-names'."


  :type '(alist :key-type string :value-type function)


  :group 'helm-mtg)




(defun helm-mtg/card-names-initialize ()


;;TODO append to a Text Register, move it to the Kill Ring (i.e. Clipboard) as either (1) a different (non-persistent) action or (2) as a finalizer before `helm-quit'.


  )




(defun helm-mtg/card-names-persistent-action (_)


;;TODO append to a Text Register, move it to the Kill Ring (i.e. Clipboard) as either (1) a different (non-persistent) action or (2) as a finalizer before `helm-quit'.


  )




;;;###autoload
(defun helm-mtg/card-names ()


(helm :sources 


 :buffer "*Helm MTG Card Names*"


 :prompt 'helm-mtg/card-name-prompt


 :init #'helm-mtg/card-names-initialize


 :mode-line "① Press ‹F1› to... ② Press ‹C-j› to copy the selected/marked names. ③ Press ‹TAB› to list all actions."


 :persistent-help "?"


 :history 'helm-mtg/card-name-history
 
  :persistent-action #'helm-mtg/card-names-persistent-action


  :action 'helm-mtg/card-name-actions


  :group 'helm-mtg)






    :multiline t


    :multimatch nil


    :requires-pattern 2






(defun mtg-helm//get-selected-or-marked-candidates (&optional _candidate)


  "Get both the Selected Candidate and any Marked Candidates.


Related:


* `helm-get-selection' 
* `helm-marked-candidates'"


  (let* ((SELECTED (helm-get-selection))


         (MARKED (helm-marked-candidates))


    (cons SELECTED MARKED)))






;; TODO *Source*-specific help via `(helm :message helm-mtg/card-names-source-help-string)`; displayed under ‹C-h m› and ‹C-c ?›.








(defun helm-delete-backward-no-update (arg)
  "Disable update and delete ARG chars backward.
Update is reenabled when idle 1s."
  (interactive "p")
  (with-helm-alive-p
    (unless helm--suspend-update-interactive-flag
      (helm-suspend-update 1))
    (backward-delete-char arg)
    (run-with-idle-timer
     1 nil
     (lambda ()
       (unless helm--suspend-update-interactive-flag
         (helm-suspend-update -1)
         (helm-check-minibuffer-input)
         (helm-force-update))))))
(put 'helm-delete-backward-no-update 'helm-only t)




























DATA


;; MTG Data…
;; 
;; Interning:
;;
;; • ‘mtg-intern’
;; • M-: (mtg-intern "Lim-Dûl the Necromancer")
;;    ↪ 'lim-dul-the-necromancer
;;
;; ‘mtg-*/’ Namespaces:
;;
;; • ‹mtg-cards/*› namespace
;; • ‹mtg-sets/*› namespace
;;
;; • M-: mtg-cards/lim-dul-the-necromancer
;;    ↪ #(mtg-card :name "Lim-Dûl the Necromancer" …)
;; • M-: mtg-cards/time-spiral
;;    ↪ #(mtg-card :name "Time Spiral" :cost '(4 U U) …)
;; • M-: mtg-sets/time-spiral
;;    ↪ #(mtg-set :name "Time Spiral" :code 'tsp …)
;;
;; ‘mtg-get-*-by-name’ Accessors:
;;
;; • M-: (mtg-get-card-by-name "Time Spiral")
;;    ↪ #(mtg-card :name "Time Spiral" :cost '(4 U U) …)
;;
;; • M-: (mtg-get-set-by-name "Time Spiral")
;;    ↪ #(mtg-set :name "Time Spiral" :code 'tsp …)
;;
;;
;;


;;


(defun mtg-get-card-by-name (name)


  ""


  (when-let* ((SYMBOL (mtg-intern name 'mtg-cards))
         (OBJECT (intern-soft SYMBOL)))


    OBJECT))


;;


(defun mtg-get-set-by-name (name)


  ""


  (when-let* ((SYMBOL (mtg-intern name 'mtg-sets))
         (OBJECT (intern-soft SYMBOL)))


    OBJECT))


;;


(defun mtg-get-keyword-by-name (name)


  ""


  (when-let* ((SYMBOL (mtg-intern name 'mtg-keywords))
         (OBJECT (intern-soft SYMBOL)))


    OBJECT))


;;










































TABLE


;;----------------------------------------------;;


(defconst mtg-default-table-list-format


  (vector `("Name"      33  t)  ; Longest card name.
          `("Cost"      nil t)
          `("Colors"    nil t)
          `("Type"      nil t)
          `("Body"      nil t)  ; both Power/Toughness and Loyalty.
          `("Rules"     nil t)


          `("Rarity"    nil t)
          `("Set"       nil t)
          `("Flavor"    nil t)
          `("Artist"    nil t)
          `("Frame"     nil t)
          `("Watermark" nil t)
          )




  "Default `tabulated-list-format' for `mtg-table-mode'.


Type: a `vectorp' of `listp's of « (NAME WIDTH SORTER) » triplets.


Each `listp' mirrors the slots of an ‘mtg-printed-card’.")




;;----------------------------------------------;;






























































;;; QUERYING:


;;----------------------------------------------;;


* `(and creature-p (>= 3 (count subtypes)))` or ` (>= 3 (count creature-types))`: “all creatures with three or more subtypes”. this query is useful to find subtypes that are neither a race-subtype nor a class-subtype (e.g. `Zombie`). Results include the `SHM` *Duos* (e.g. `Emberstrike Duo`, an Elemental Warrior Shaman`; `Gravelgill Duo`, a Merfolk Rogue Warrior`), which support the theme of “class-matters”; zombie wizard, which represent liches






{1}{BR}


























;;==============================================;;


;;----------------------------------------------;;


(defun mtg-jit-run-query (query card)


  "JIT-‘mtg-run-query’ (Just-in-Time ‘byte-compile’d).


URL ‘https://nullprogram.com/blog/2016/12/11/’"


  (let* ((memq-list (cl-loop for tag in tags
                             collect `(memq ',tag entry)))


         (function `(lambda (db)
                      (cl-loop for entry in db
                               count (or ,@memq-list))))


         (compiled (byte-compile function)))


    (funcall compiled db)))


;;----------------------------------------------;;


mtg-run-query


(cl-defun mtg-query-cards (query &optional cards &key (byte-compile t))


  "Query CARDS for ‘mtg-card-p’s matching QUERY.


• QUERY — an ‘mtg-card-query-p’s.


• CARDS — an optional ‘sequencep’ of ‘mtg-card-p’s. 
Defaults to the variable ‘mtg-cards’.


• BYTE-COMPILE — an optional ‘booleanp’
Defaults to t.
If BYTE-COMPILE is non-nil, we ‘byte-compile’ the matching-function (which is called on every card, i.e. hundreds or thousands of times).


"


  (let* ((CARDS (or cards mtg-cards)))


    (cl-loop for CARD being the elements of CARDS
      if (mtg-match-card query CARD)
      collect CARD)))


;;----------------------------------------------;;


(defun mtg-match-card-query (query card)


  (let ((mtg-match-card-query--card card))


    (mtg-match-card-query-- query)))


;; ^ TODO ‘let’ or ‘setq’.


;;


(defun mtg-match-card-query-- (query)
;;


;;==============================================;;








TODO mtg-card-predicate:


;; Follow variables:


  (while (and (boundp NAME) (symbolp (symbol-value NAME)))
     (cl-callf symbol-value NAME))






















































































UNICODE


;;==============================================;;


(defvar mtg-unicode-punctuation-alist


  `(("--" . (:en ?— :es ?: :fr t :it t :pt t :de t :ru t :zh ?～ :ko t :ja ?ー))


    ("*"  . (:en ?• :es t :fr t :it t :pt t :de t :ru t :zh t :ko t :ja t))


    ("\"" . (:en ?“ :es t :fr "« " :it t :pt t :de ?„ :ru t :zh ?「 :ko ? :ja ?「))
    ("\"" . (:en ?” :es t :fr " »" :it t :pt t :de ?“ :ru t :zh ?」 :ko ? :ja ?」))


    ("("  . (:es t :fr t :it t :pt t :de t :ru t :zh ?（ :ko t :ja ?（))
    (")"  . (:es t :fr t :it t :pt t :de t :ru t :zh ?） :ko t :ja ?）))


    ("/"  . (:en t :es t :fr t :it t :pt t :de t :ru t :zh t :ko t :ja t))


    (","  . (:en :es t :fr t :it t :pt t :de t :ru t :zh ?， :ko t :ja ?，))


    (";"  . (:es t :fr t :it t :pt t :de t :ru t :zh ? :ko ? :ja ?))


    (":"  . (:es t :fr t :it t :pt t :de t :ru t ? :zh ?： :ko t :ja ?：))


    ("."  . (:en t :es t :fr t :it t :pt t :de t :ru t :zh ?。 :ko t :ja ?。))


    ("+"  . (:en t :es t :fr t :it t :pt t :de t :ru t :zh t :ko t :ja ?＋))


    ("-"  . (:en t :es t :fr t :it t :pt t :de t :ru t :zh t :ko t :ja ?－))


    ("1"  . (:en t :es t :fr t :it t :pt t :de t :ru t :zh t :ko t :ja ?１))
    ;;; 2,3,…


 ;; (""  . (:es t :fr t :it t :pt t :de t :ru t :zh t :ko t :ja t))
   )


  "


Type: an Association-‘listp’, from ‘stringp’ to (‘keywordp’-)Property-‘listp’.


Each Property (in the Property-List) can be:


• a ‘stringp’ — 
• a ‘characterp’ — equivalent to a (singleton) ‘stringp’.
• t — means “(known to be) the same as ‘:en’”. If ‘:en’ is t, then it's the same as the ‘car’.
• nil — means “unknown”, equivalent to absence.")


;; (Templates:)
;;
;; (:en ? :es ? :fr ? :de ? :it ? :pt ? :ru ? :zh ? :ko ? :ja ?)
;; (:en nil :es nil :fr nil :de nil :it nil :pt nil :ru nil :zh nil :ko nil :ja nil)




;; Translations of “Choose one —”:
;;
;; • “Choose one —” (en)
;;
;; • “Elige uno:” (es)
;;
;; • “Choisissez l'un —” (fr)
;;
;; • “Scegli uno —” (it)
;;
;; • “Escolha um —” (pt)
;;
;; • “Bestimme eines —” (de)
;;
;; • “Выберите одно —” (ru)
;;
;; • “选择一项～” (zh, simplified)
;;
;; • “하나를 선택한다 —” (ko)
;;   [TODO] or this: 以下から１つを選ぶ。
;;
;; • “ソーサリー” (ja)
;;
;; • “選擇一項～” (zh, traditional)
;;
;; 
;;


;; Translations of “+1/+1”:
;;
;; • “＋１/＋１” (ja)
;; 


;; －４/－０


;;==============================================;;








Icons:


- Tombstone icon — in the top left corner, denotes “this card is active in the graveyard”. e.g. on cards with `Flashback`; e.g. on the `Incarnation` cycles (`Valor`, `Wonder`, `Filth`, `Anger`, `Brawn`, `Glory`, `Genesis`); e.g. `Ichorid`; e.g.`Riftstone Portal`.


- Night icon — 🌘 Waning Crescent Moon. for the Back-Face Double-Faced Cards (e.g. Werewolves, ‹Insectile Aberration›).








;;==============================================;;


;;----------------------------------------------;;
 
(defconst mtg-tombstone-icon-string "TODO"
  "‘mtg-tombstone-icon’ as a (Unicode) String.")


;;----------------------------------------------;;


(progn


  (defimage mtg-tombstone-icon-svg "TODO"
    "‘mtg-tombstone-icon’ as an SVG.")


  (defconst mtg-tombstone-icon-svg-image 'mtg-tombstone-icon-svg))


;;----------------------------------------------;;
 
(defcustom mtg-tombstone-icon ""


  "The Tombstone Icon.


In Odyssey Block, the Tombstone Icon appeared at the top-left corner of “graveyard-relevant”, including:


• “Wonder” (see variable ‘mtg-tombstone-icon-card-list’).
• “Deep Analysis” and all cards with “Flashback” (see variable ‘mtg-tombstone-icon-keyword-list’).


Type: a ‘stringp’ or ‘imagep’.


URL ‘https://mtg.gamepedia.com/Tombstone_icon’"


  :type '(choice (string :tag "Unicode")
                 (symbol :tag "Image"))


  :set        #'mtg-custom-set
  :initialize #'custom-initialize-default


  :safe t
  :group 'mtg)


;;----------------------------------------------;;


(defcustom mtg-tombstone-icon-card-list


  '(
    ;; Original:


    "Valor"
    "Wonder"
    "Filth"
    "Anger"
    "Brawn"
    "Riftstone Portal"
    "Ichorid"
    "Genesis"
    "Glory"


    ;; Custom…


    ;; “You may cast ~ from your graveyard”:


    "Gravecrawler"
    "Haakon, Stromgald Scourge"
    "Hogaak, Arisen Necropolis"
    "Marang River Prowler"
    "Oathsworn Vampire"
    "Risen Executioner"
    "Scourge of Nel Toth"
    "Skaab Ruinator"
    "Squee, the Immortal"
    "Worldheart Phoenix"


    ;; “Return ~ from your graveyard to the battlefield”:


    "Necrosavant"
    "Reassembling Skeleton"
    ;;
    "Advanced Stitchwing"
    "Akoum Firebird"
    "Akuta, Born of Ash"
    ""
    ""
    ""
    ""
    ""
    ;;TODO https://scryfall.com/search?as=grid&order=name&q=oracle%3A%22Return+~+from+your+graveyard+to+the+battlefield%22


    ;; “If ~ is in your graveyard”:


    "Auntie's Snitch"
    "Blood Operative"
    "Bridge from Below"
    "Carrionette"
    "Death Spark"
    "Firemane Angel"
    "Ghastly Remains"
    "Gigapede"
    "Krovikan Horror"
    "Nether Shadow"
    "Pyrewild Shaman"
    "Pyre Zombie"
    "Thunderblade Charge"
    "Vengeful Pharaoh"


    ;; “Return ~ from your graveyard to your hand”:


    "Punishing Fire"
    ;;TODO https://scryfall.com/search?as=grid&order=name&q=oracle%3A%22Return+~+from+your+graveyard+to+your+hand%22


    ;; “As long as ~ is in your graveyard”:


    "Dearly Departed")


  "MTG Cards which have an ‘mtg-tombstone-icon’.


Examples:


• “Reassembling Skeleton” — “{1}{B}: Return ~ from your graveyard to the battlefield tapped.” i.e. a self-reanimating Activated Ability.


• “Gravecrawler” — “You may cast ~ from your graveyard as long as you control a Zombie.” i.e. a self-reanimating Static Ability.


• “Wonder” — “As long as ~ is in your graveyard and you control an Island, creatures you control have flying.”


• “” — “: Return ~ from your graveyard to your hand.” i.e. a self-regrowing Activated Ability.


• “Punishing Fire” — “Whenever an opponent gains life, you may pay {R}. If you do, return ~ from your graveyard to your hand.” i.e. a self-regrowing Tired Ability.


Type: a ‘listp’ of ‘mtg-card-p’s or ‘stringp’s (‘mtg-card-name’s).


Links:


• URL ‘https://scryfall.com/search?q=-oracle%3Aflashback+is%3Atombstone&unique=cards&as=grid&order=name’
• URL ‘https://scryfall.com/search?q=oracle%3A%22You+may+cast+%7E+from+your+graveyard%22&unique=cards&as=grid&order=name’
• URL ‘https://scryfall.com/search?q=oracle%3A%22You+may+cast+%7E+from+your+graveyard%22&unique=cards&as=grid&order=name’
• URL ‘https://scryfall.com/search?as=grid&order=name&q=oracle%3A%22Return+~+from+your+graveyard+to+the+battlefield%22’
• URL ‘https://scryfall.com/search?as=grid&order=name&q=oracle%3A%22if+~+is+in+your+graveyard%22’
• URL ‘https://scryfall.com/search?q=oracle%3A%22As+long+as+%7E+is+in+your+graveyard%22&unique=cards&as=grid&order=name’
• URL ‘https://scryfall.com/search?as=grid&order=name&q=oracle%3A%22Return+~+from+your+graveyard+to+your+hand%22’"


  :type '(repeat (string :tag ""))


  :set        #'mtg-custom-set
  :initialize #'custom-initialize-default


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(defcustom mtg-tombstone-icon-keyword-list


  '(
    ;; Original:


    "Flashback"


    ;; Custom (“flashing back”):


    "Aftermath"
    "Jump-start"
    "Retrace"


    ;; Custom (reanimating):


    "Unearth"
    "Embalm"


    ;; Custom (regrowing):


    "Dredge"
    "Recover"


    ;; Custom:


    "Scavenge"
   )


  "MTG Keywords which imply an ‘mtg-tombstone-icon’.


Type: a ‘listp’ of ‘mtg-keyword-p’s or ‘mtg-keyword-name’s."


  :type '(repeat (string :tag ""))


  :set        #'mtg-custom-set
  :initialize #'custom-initialize-default


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


















supertypes:


- *Legendary* — 👑 (Crown), 


cardtypes:


;; URL ‘https://magic.wizards.com/en/articles/archive/future-sight%E2%80%99s-card-type-symbols-2007-12-26’


Instant — ⚡ (HIGH VOLTAGE)
Sorcery — ()
Artifact — 🏺 (AMPHORA)
Enchantment — ()
Creature — 🐷 (PIG FACE), generic creature (for specific creature types, see below)
Land — ()


? 📜 Scroll




Symbols:


- Level Symbols — e.g. `{1-4}` or `level{1-4}`; e.g. `{5+}` or `level{5+}`. 








P/T:


Power? — 🗡 Dagger
Toughness? —






Colors:


Multicolor? — 🌈 Rainbow








creature (sub)types:


* The most frequent subtypes:


- *Human* — `` (Woman), `` (Man).
- *Wizard* — `🧙` (Mage), 














 (Zombie-like) subtypes:


Zombie
Mutant
Ally






MTG-Specific subtypes:


Vedalken = 37 (aUw)


Kor = 35 (W)


Myr = 35 (A)


Kavu = 41 (GR)


Sliver = 96 (bugrw)


Kithkin = 60 (uW)


Viashino = 35 (gR)


Thallid


Thrull










Robots:


Golem = 94 (A)


Construct = 89 (A)














Elemental = 352 (bugRw)


Beast = 329 (uGr)


Horror = 145 (Bu)


Shapeshifter = 80 (abUgrw)


Illusion (is illusion like zombie?)


Ogre = 78 (bR)


Faerie = 76 (bUg)


Wurm = 76 (bGr)


Drake = 71 (U)


Treefolk = 66 (bG)
myMinotaur = 53 (bR)


Orc = 41 (BR)


Centaur = 47 (Grw)


Lizard = 46 (bGR)


Elephant = 45 (GW)


Plant = 40 (bG)


Spider = 40 (G)


Dwarf = 38 (R)


Griffin = 37 (W)


Ape — 🦍 Gorilla


Dryad = 28 (Gw)


Boar = 27 (Gr)


Scarecrow = 26 (A)


Serpent = 26 (U)


Shade = 26 (B)


Rhino = 25 (GW)


Troll = 25 (bGr)


Ooze = 24 (bGr)


Horse = 21 (abgrw)








Ape — MONKEY.
Ape — MONKEY.
Boar — PIG
















Soldier = 516 (urW)


Warrior = 492 (bgRw)


Shaman = 315 (bGR)


Cleric = 286 (bW)


Rogue = 195 (BUr)


Knight = 179 (brW)


Scout = 92 (uGrw)


Monk = 78 (ugrW)


Archer = 62 (Grw)


Berserker = 56 (bgR)


Spellshaper = 56 (bugrw)


Assassin = 41 (B)


Rebel = 50 (bW)


Mercenary = 40 (Br)


Artificer = 46 (Ugrw)


Minion = 39 (B)


Nomad = 29 (grW)


Samurai


Ninja










- ** — `` (),


* The primary subtypes per-color:


- *Human*? — `` (),
- *Merfolk* — `🧜` (Merperson), 
- *Zombie* — `` (),
- *Goblin* — `` (),
- *Elf* — `🧝` (Elf),


* The “iconic” subtypes per-color:


- *Angel* — `` (),
- *Sphinx* — `` (), 
- *Demon* — 😈 (`Smiling Face With Horns`)
- *Dragon* — `🐉` (DRAGON), `🐲` (DRAGON FACE)
- *Hydra* — `` (),


MTG-specific races:


Homarid — 🦞 Lobster
Thrull — ?


* Undead subtypes:


- *Spirit* — `👻` (Ghost)
Vampire — 🧛 Vampire
Skeleton — 🦴 Bone, ☠️ Skull and Crossbones
Wraith 
- *Zombie* (see above)


* "class" subtypes (the Unicode character should look humanoid):


- Advisor — 


- Artificer — 🛠️ (Hammer and Wrench)
- *Wizard* (see above)


* "race" subtypes:


- *Bat* — `🦇` (`BAT`)
- *Bird* — `🐦` (`BIRD`), 🦉 Owl, `🦜` Parrot, 🦚 Peacock, 🦅 Eagle, 🦢 Swan, 🕊 Dove, 🦆 Duck, 🐧 Penguin, 🦃 Turkey, 🐔 Chicken, 🐓 Rooster.
- *Insect* — 🐝 Honeybee, 🦟 Mosquito, 🐛 Bug, 🦋 Butterfly, 🐜 Ant, 🐞 Lady Beetle, 🦗 Cricket; many `Insect`s are *Bees* or *Ants*.
- *Dinosaur* — 🦕 Sauropod, 🦖 T-Rex.


Crocodile — 🐊 Crocodile
Snake — 🐍 Snake
Wall — 🧱 (Brick), 🏰 (European Castle),Djinn 🧞 (Genie)
Cat — 🦁 Lion Face, 🐈 Cat, 🐱 Cat Face
Squirrel 🐿️ Chipmunk
Rat 🐀 Rat
🐘 (Elephant)
Faerie 🧚 Fairy
Camel 🐪 (Dromedary Camel), 🐫 (Bactrian Camel)
Fish — 🦈 Shark, 🐟 Fish, 🐬 Dolphin, 🐠 Tropical Fish, 🐡 Blowfish; most `Fish` creatures are *Sharks* or *Eels*.
Crab — 🦀 Crab
Werewolf — 
Wolf — 🐺 Wolf Face
Hound — 🐕 Dog, 🐶 Dog Face
Fox — 🦊 Fox Face
Fungus — 🍄 Mushroom
Horse — 🐴 Horse Face, 🐎 Horse
Whale — 🐋 Whale, 🐳 Spouting Whale
Turtle 🐢 Turtle
Unicorn — 🦄 Unicorn Face
Boar — 🐗 Boar, 🐷 Pig Face, 🐖 Pig
Goat — 🐐 Goat
Sheep — 🐏 Ram, 🐑 Ewe; all `Sheep` have *Ram* in their name (e.g. *Nyx-Fleece Ram*).
Hippo — 🦛 Hippopotamus
Rhino — 🦏 Rhinoceros
Badger — 🦡 Badger
Bear — 🐻 Bear Face, 🐼 Panda Face, 🐨 Koala.
Rabbit — 🐰 Rabbit Face, 🐇 Rabbit
Lizard — 🦎 Lizard
Octopus — 🐙 Octopus
Ooze — 🐌 Snail; most `Ooze` look like green bubbles or green slugs (i.e. no shell).
Scorpion — 🦂 Scorpion.
Spider — 🕷 Spider, 🕸 Spider Web.
Squid — 🦑 Squid.














mtg-unicode-gender-alist
mtg-unicode-gender


a ‘symbolp’, ‘characterp’, or nil. 
the ‘symbolp’s are the key of `mtg-unicode-gender-alist`








mtg-unicode-skin-tone-alist
mtg-unicode-skin-tone


a ‘symbolp’, ‘characterp’, or nil. 
the ‘symbolp’s are the key of `mtg-unicode-skin-tone-alist`


nil means use the “default emoji”, for example, “Vampire” instead of: Woman Vampire (URL ‘’), MAN VAMPIRE (URL ‘’), Dark Skinned Vampire (URL ‘’), Light-skinned Vampire (URL ‘’), etcetera.


symbol :tag "Skin Tone"
character :tag "Unicode Character"








This version of the 🧛 Vampire emoji has the 🏿 Dark Skin Tone applied, which is displayed as a Black Skin Tone on supported platforms.


The Woman Vampire emoji is a sequence of the 🧛 Vampire and ♀ Female Sign emojis. These are combined using a zero width joiner between each character and display as a single emoji on supported platforms.






































DABBREV


;; ‘mtg-dabbrev-*’


mtg-dabbrev-expand
mtg-dabbrev-complete




(defconst 
mtg-dabbrev-case-fold-search t
  "‘mtg-dabbrev-case-fold-search’ for ‘mtg-mode’.")


(defconst 
mtg-dabbrev-case-replace nil
  "‘dabbrev-case-replace’ for ‘mtg-mode’.")


(defconst mtg-dabbrev-upcase-means-case-search t
  "‘dabbrev-upcase-means-case-search’ for ‘mtg-mode’.")




define-derived-mode


  (setq-local dabbrev-case-fold-search mtg-dabbrev-case-fold-search)
  (setq-local dabbrev-case-replace     mtg-dabbrev-case-replace)
  (setq-local dabbrev-upcase-means-case-search     mtg-dabbrev-upcase-means-case-search)
















































[TODO: EDITS]


.


(defun mtg--xfont-by-name (font-name)
  "Lookup an XFont which matches FONT-NAME (wraps ‘x-list-fonts’)."


(defun mtg--xfont-by-name (font-name)
  "Lookup an XFont which matches FONT-NAME (wraps ‘set-face-font’)."


.


(defun mtg--xfont-by-family (font-family)


  "Lookup an XFont which matches FONT-FAMILY (wraps function ‘x-family-fonts’)."


Output:


• a ‘vectorp’ of? (a ‘stringp’?).


Inputs:


• FONT-FAMILY — a ‘stringp’.


Examples:


• M-: (mtg--xfont-by-family \"Beleren\")
    ↪ \"TODO\""


  (let* ((FONTS (when (fboundp 'x-family-fonts)
                  (condition-case e                  ; catch "Invalid font name" error.
                      (x-family-fonts font-family)
                    ((error) e))))
         )
    (car-safe FONTS)))


;; (x-family-fonts &optional FAMILY FRAME)


;; [family width point-size weight slant fixed-p full registry-and-encoding]
;;
;; >The first five elements correspond to face attributes; if you specify these attributes for a face, it will use this font.
;; >
;; >The last three elements give additional information about the font. fixed-p is non-nil if the font is fixed-pitch. full is the full name of the font, and registry-and-encoding is a string giving the registry and encoding of the font.
;;


.


.
















;;==============================================;;


;;; CONSTANTS:


;;==============================================;;


(defconst dgn-vertical-whitespace-regexp


  (rx (any ?\u000a ?\u000b ?\u000c ?\u000d
           ?\u0085 ?\u2028 ?\u2029))


  "Regexp that matches any Vertical-Whitespace character.


The ASCII Vertical-Whitespace characters are:


• U+000A — “LINE FEED (LF)”;
a.k.a. “\\n” a.k.a. “EOL” a.k.a. “Line-Break” a.k.a “^J”.


• U+000B — “VERTICAL TAB (VT)”;
a.k.a. “\\v” a.k.a. “LINE TABULATION” a.k.a “^K”.


• U+000C — “FORM FEED (FF)”;
a.k.a. “\\f” a.k.a. “Page-Break” a.k.a “^L”.


• U+000D — “CARRIAGE RETURN (CR)”;
a.k.a. “\\r” a.k.a “^M”.


The Unicode Vertical-Whitespace characters are:


• U+0085 — “NEXT LINE (NEL)”.


• U+2028 — “LINE SEPARATOR (LS)”.


• U+2029 — “PARAGRAPH SEPARATOR (PS)”.


Links:


• URL ‘https://en.m.wikipedia.org/wiki/Newline#Representation’")


;;----------------------------------------------;;


;;----------------------------------------------;;


;;----------------------------------------------;;










































































;;==============================================;;


;;; GENERICS:


;;==============================================;;


(defgeneric mtg-abc (_) nil
  ".")


;;


(defmethod mtg-abc ((this mtg-xyz))
  (mtg-xyz-abc this))


;;


;;==============================================;;


(defgeneric mtg-string (this) (mtg-text-string this)
  ".")


;;


(defmethod mtg-string ((this mtg-card))
  (mtg-card-description this))


;;TODO (defun mtg-card-description (card) "Return CARD as a human-readable string.")


;;


;;==============================================;;


(defgeneric mtg-power (object) _
  "Return the (Creature-)Power of OBJECT.")


;; 


(defmethod mtg-power ((this mtg-pt))
  (mtg-pt-power this))


;; 


(defmethod mtg-power ((this mtg-card))
  (mtg-pt-power (mtg-card-pt this)))


;; 


;;==============================================;;


(defgeneric mtg-toughness (object) _
  "Return the (Creature-)toughness of OBJECT.")


;; 


(defmethod mtg-toughness ((this mtg-pt))
  (mtg-pt-toughness this))


;; 


(defmethod mtg-toughness ((this mtg-card))
  (mtg-pt-toughness (mtg-card-pt this)))


;; 




;;==============================================;;


(defgeneric mtg-cmc (object)
  "Return the Converted Mana Cost of OBJECT.
Defaults to « (‘constant’ 0) ».)


;; 


(defmethod mtg-cmc ((_ (eql nil)))
  0)


;; 


(defmethod mtg-cmc ((this mtg-mana-cost))
  (mtg-mana-cost-cmc this))


;;


(defmethod mtg-cmc ((this mtg-mana-symbol))
  (mtg-mana-symbol-cmc this))


;;


(defmethod mtg-cmc ((this mtg-card))
  (mtg-card-cmc this))


;;


(defmethod mtg-cmc ((this symbol))
  "Converted Mana Cost of the named ‘mtg-card’."
  (if-let* ((CARD (mtg-symbol-value this 'card)))
           (mtg-cmc-card CARD)
    0))


;;==============================================;;


(defgeneric mtg-colors (object)
  "Return the ‘mtg-colors’ of OBJECT.".)


;;


(defmethod mtg-colors ((_ (eql nil)))
  "‘mtg-colorless’"
  (mtg-colorless))


;;


(defmethod mtg-colors ((_ (eql t)))
  "‘mtg-colorful’"
  (mtg-colorful))


;;


(defmethod mtg-colors ((this bool-vector))
  "‘make-mtg-colors’"
  (make-mtg-colors this))


;;


(defmethod mtg-colors ((this list))
  "‘make-mtg-colors’"
  (make-mtg-colors this))


;;


(defmethod mtg-colors ((this string))
  (mtg-parse-colors this))


;;==============================================;;


(defgeneric mtg-abbr-char (this)


  (cond ((recordp this)
           (cl-struct-slot-value (cl-typep this) 'abbr this))
        (t
           (error "‘mtg-abbr-char’: « %s » can't be abbreviated (has no “abbr” field)" this)))


  "Return an abbreviation for THIS, as a ‘characterp’.


Usage:


• Used by function ‘mtg-read-multiple-choice’ to generically extract a mnemonic abbreviation.")


;;


(defmethod mtg-abbr-char ((this mtg-color))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-rarity))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (string-to-char (symbol-name (mtg--abbr this))))


;;


(defmethod mtg-abbr-char ((this mtg-))
  (symbol-name (mtg--abbr this)))


;; e.g. :abbr '


;;


;; (defmethod mtg-abbr-char ((this mtg-block))
;;   (symbol-name (mtg--abbr this)))
;; 
;; ;; e.g. :abbr 'ia :name "Ice Age Block"


;;


;; (defmethod mtg-abbr-char ((this mtg-edition))
;;   (symbol-name (mtg--abbr this)))
;; 
;; ;; e.g. :abbr 'abu :name "Alpha Beta Unlimited"


;;


;;(defmethod mtg-abbr-char ((this mtg-language))
;;  (symbol-name (mtg--abbr this)))
;;
;; ;; e.g. :abbr 'fr


;;


;;(defmethod mtg-abbr-char ((this mtg-xyz))
;;  (string-to-char (symbol-name (mtg-xyz-abbr this))))
;; e.g. :abbr '


;;==============================================;;


(defgeneric mtg-name-string (this)


  (cond ((recordp this)
           (cl-struct-slot-value (cl-typep this) 'name this))
        (t
           (error "‘mtg-name-string’: « %s » is anonymous (has no “name” field)" this)))


  "Return a name of THIS, as a ‘stringp’.


Usage:


• Used by function ‘mtg-read-multiple-choice’ to generically extract an informative name.")


;;


(defmethod mtg-name-string ((this mtg-xyz))
  (symbol-name (mtg-xyz-name this)))


;;


(defmethod mtg-name-string ((this mtg-xyz))
  (mtg-xyz-name this))


;;


(defmethod mtg-name-string ((this mtg-block))
  (mtg-block-name this)))


;; e.g. :name "Ice Age Block"


;;


(defmethod mtg-name-string ((this mtg-edition))
  (mtg-edition-name this)))


;; e.g. :name "Alpha Beta Unlimited"


;;


;;(defmethod mtg-name-string ((this mtg-xyz))
;;  (symbol-name (mtg-xyz-name this)))
;; e.g. :name ""


;;==============================================;;






























;;==============================================;;


;;----------------------------------------------;;


(defconst mtg-color-index-white 0
  "Index in ‘mtg-color-list’ of ‘white’.")


;;----------------------------------------------;;


(defconst mtg-color-index-blue 1
  "Index in ‘mtg-color-list’ of ‘blue’.")


;;----------------------------------------------;;


(defconst mtg-color-index-black 2
  "Index in ‘mtg-color-list’ of ‘black’.")


;;----------------------------------------------;;


(defconst mtg-color-index-red 3
  "Index in ‘mtg-color-list’ of ‘red’.")


;;----------------------------------------------;;


(defconst mtg-color-index-green 4
  "Index in ‘mtg-color-list’ of ‘green’.")


;;----------------------------------------------;;


(defun mtg-blue-p (object)
  "Whether OBJECT is blue.
Wraps generic function ‘mtg-colors’."
  (mtg-colors-blue-p (mtg-colors object)))


(gv-define-setter mtg-blue-p (flag object)
  `(setf (mtg-color-blue-p ,object) ,flag))


;;----------------------------------------------;;


(define-inline mtg-colors-blue-p (colors)


  "Whether COLORS includes blue.


This is a Generalized Variable.
For example:


  (let ((COLORS (make-mtg-colors \\='(r g))))
    (setf (mtg-colors-blue-p COLORS) t)
    COLORS)


  ↪ (make-mtg-colors \\='(u r g))


  ↪ &5\"TODO\""


  (inline-letevals ((COLORS colors))
    (inline-quote
      (aref ,COLORS mtg-colors-index-blue))))


(gv-define-setter mtg-color-blue-p (flag colors)
  `(aset ,colors mtg-color-index-blue ,flag))


;;==============================================;;


































;;==============================================;;


;;; UTILITIES:


;;==============================================;;


;;----------------------------------------------;;




;;----------------------------------------------;;


(define-inline mtg-text-string (text &optional text-properties)


  "Return TEXT as a string.


Output is:


• a ‘stringp’.
• has no Text Properties, unless TEXT-PROPERTIES is non-nil.
• is always a new object (i.e. not ‘eq’ to the input).


Inputs:


• TEXT — a “Textual Type”, i.e.:


    • a ‘stringp’ — Return a copy of TEXT.


    • a ‘symbolp’ — Return the symbol's name.


    • nil — Return “” (i.e. the empty string, not “nil”).


    • a ‘characterp’ — Return the char as a singleton string.


    • a ‘bufferp’ — Return the buffer's contents.


    • an ‘overlayp’ — Return the text spanned by the overlay
(in the overlay's buffer, not the current one).
If TEXT-PROPERTIES is non-nil,
the input's Overlay-Properties
become the output's Text-Properties.


• TEXT-PROPERTIES — a ‘booleanp’.
Whether the output may include Text Properties.
Default is nil, which means the output never has any Text Properties.


=== Examples ===


Lisp Types:


M-: (mtg-text-string nil)
 ↪ \"\"


M-: (mtg-text-string \"abc\")
 ↪ \"\"


M-: (mtg-text-string ?Σ)
 ↪ \"Σ\"


M-: (mtg-text-string \\='xyz)
 ↪ \"xyz\"


Editor Types:


M-: (mtg-text-string (current-buffer))
 ↪ \"mtg.el\"


=== Laws ===


• Idempotence —


    ∀s. (mtg-text-string (mtg-text-string s)) ≡ (mtg-text-string s)


=== Notes ===


This function provides the default implementation
of generic function ‘mtg-text’ (which see)


=== Implementation ===


We dispatch to the following functions, given the following types of argument TEXT:


• a ‘stringp’ — function ‘substring-no-properties’;
or function ‘substring’ if TEXT-PROPERTIES is non-nil.


• a ‘symbolp’ — function ‘symbol-name’.


• a ‘characterp’ — function ‘string’.


• a ‘bufferp’ — function ‘buffer-substring-no-properties’;
or function ‘buffer-substring’, if TEXT-PROPERTIES is non-nil.


• an ‘overlayp’— like for a ‘bufferp’,
given function ‘overlay-buffer’, function ‘overlay-start’ and function ‘overlay-end’;
also function ‘overlay-properties’, if TEXT-PROPERTIES is non-nil.
See function ‘mtg--overlay-string’."


  (declare (side-effect-free t))


  (inline-letevals (text)
    (inline-quote


      (cl-etypecase text


        (string (if text-properties
                    (substring text)
                  (substring-no-properties text)))


        (symbol (if text (symbol-name text) ""))


        (character (string text))


        (buffer (if text-properties
                    (buffer-substring text)
                  (buffer-substring-no-properties text)))


        (overlay (mtg--overlay-string text text-properties))))


    (if text-properties
        (set-text-properties 0 (length STRING) (overlay-properties text) STRING)
      STRING)))))))


;;----------------------------------------------;;


(defun mtg--overlay-string (overlay &optional properties extra-overlay-only-properties ignore-implicit-overlay-only-properties)


  "Return the text (currently) spanned by OVERLAY.


IF PROPERTIES is non-nil,
Overlay Properties are added,
as Text Properties, to the output.


EXTRA-OVERLAY-ONLY-PROPERTIES is a list of
(names of) Overlay Properties which aren't
specially-meaningfully Text Properties,
to be removed from the output.
Defaults to property ‘priority’ and property ‘evaporate’.


If IGNORE-IMPLICIT-OVERLAY-ONLY-PROPERTIES is non-nil,
the two implicit Overlay Properties are surpressed.


(the text comes from the overlay's buffer, not the current one)."


  (declare (side-effect-free t))


  (cl-check-type overlay overlay)


  (let ((STRING (with-current-buffer (overlay-buffer overlay)


    (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))))


    (if (not properties)
        STRING


      (let* ((OVERLAY-PROPERTIES (overlay-properties overlay))
             (NON-TEXT-PROPERTIES (append extra-overlay-only-properties (unless ignore-implicit-overlay-only-properties '(priority evaporate))))
             (STRING (apply #'propertize STRING OVERLAY-PROPERTIES)))


        (prog1 STRING 
               (remove-list-of-text-properties 0 (length STRING) NON-TEXT-PROPERTIES STRING)))))))


;;----------------------------------------------;;


(define-inline mtg--hash-get (key map)


  "Lookup KEY in MAP (both as a ‘symbolp’ and a ‘keywordp’).


Inputs:


• KEY — a ‘symbolp’.


• MAP — a ‘hash-table-p’.


=== Examples ===


M-: (mtg--hash-get \\='name #s(hash-table data (knick nil name \"Richard Garfield\")))
 ↪ \"Richard Garfield\"


M-: (mtg--hash-get \\='name #s(hash-table data (:knick nil :name \"Richard Garfield\")))
 ↪ \"Richard Garfield\""


  (declare (side-effect-free t))


  (inline-letevals (key map)
    (inline-quote


      (or (gethash ,key ,map)
          (gethash (keyword ,key) ,map)))))


;;----------------------------------------------;;


(define-inline mtg--list-get (key map)


  "Lookup KEY in MAP (both as a ‘symbolp’ and a ‘keywordp’).


Inputs:


• KEY — a ‘symbolp’.


• MAP — a ‘p’.


=== Examples ===


M-: (mtg--list-get \\='name \\='(knick nil name \"Richard Garfield\"))
 ↪ \"Richard Garfield\"


M-: (mtg--list-get \\='name \\='(:knick nil :name \"Richard Garfield\"))
 ↪ \"Richard Garfield\""


  (declare (side-effect-free t))


  (inline-letevals (key map)
    (inline-quote


      (or (plist-get ,key ,map) (plist-get (keyword ,key) ,map)
          (assq ,key ,map) (assq (keyword ,key) ,map)))))


;;----------------------------------------------;;


(define-inline mtg--symbol-char (s)


  "Convert S to a character.


Output:


• a ‘characterp’.


Inputs:


• S — a ‘symbolp’.


=== Examples ===


M-: (mtg--symbol-char t)
  ↪ ?t"


M-: (mtg--symbol-char \\='TRUE)
  ↪ ?T"


  (declare (side-effect-free t))


  (inline-letevals (s)
    (inline-quote


      (string-to-char (symbol-name ,s)))))


;;----------------------------------------------;;


(define-inline mtg--text-oneline-p (text)


  "Whether TEXT is a one-liner (i.e. no newlines).


Output:


• a ‘booleanp’.


• t — TEXT is a one-liner.
Contains no vertical whitespace characters,
according to variable ‘mtg-vertical-whitespace-regexp’.


• nil — TEXT is multiline.


Inputs:


• TEXT — a ‘stringp’."


  (declare (side-effect-free t))


  (inline-letevals (text)
    (inline-quote


      (not (string-match-p mtg-vertical-whitespace-regexp ,text)))))


;;----------------------------------------------;;


(define-inline mtg--current-line-blank-p ()


  "Whether the current line is blank (i.e. all whitespace characters).


Output:


• a ‘booleanp’.


• t — TEXT is a blank.
Contains only whitespace characters.


• nil — TEXT isn't blank.


Inputs:


• TEXT — a ‘stringp’."


  (declare (side-effect-free t))


  (inline-letevals (text)
    (inline-quote


      (string-match-p "^\\s-*$" (thing-at-point 'line)))))


;;----------------------------------------------;;


(define-inline mtg-canonicalize-string (text)


  "Canonicalize TEXT.


Inputs:


• TEXT — a ‘stringp’.


Output:


• a ‘stringp’.


• Unicode — Normalize equivalent Unicode substrings/codepoints, via function ‘mtg--normalize-unicode’.


• Casing — “Collapse” the casing, via function ‘downcase’.


• Separators — “Simplify” all separators (symbolic/punctuation characters) as a (singlw) hyphen character (i.e. “-”).


• Whitespace — .


• — .


• — .


• — .


• — .


Related:


• function ‘mtg-normalize-string’ is “lighter-weight” (than this function).


=== Examples === "


  (inline-letevals ((TEXT text))
    (inline-quote
      (downcase ,TEXT))))


;  (join-string (save-match-data (split-string name REGEXP)) :omit-nulls ) "-")))


;;----------------------------------------------;;


(define-inline mtg-normalize-string (text)


  "Normalize TEXT.


Inputs:


• TEXT — a ‘stringp’.


Output:


• a ‘stringp’.


• Whitespace — Replace blank substrings with a (single) space character (i.e. “ ”).


• — .


• — .


• — .


• — .


Related:


• function ‘mtg-canonicalize-string’ is “heavier-weight” (than this function).


=== Examples === "


  (inline-letevals ((TEXT (cl-typecase text (string text) (character (string text))))))
    (inline-quote


      (mtg--replace-regexp-in-string ,TEXT "[[:space:]]+" " "))))


;;----------------------------------------------;;


(define-inline mtg--normalize-unicode (text)


  "Normalize the Unicode representation of TEXT.


=== Inputs/Output ===


• TEXT — a ‘stringp’ [TODO or ‘characterp’?].


• Return a ‘stringp’.


=== Examples ===


(mtg-normalize-string \"na\u00EFve\")
;; ↪ “naïve”


(mtg-normalize-string \"nai\u0308ve\")
;; ↪ “naïve”


(string= \"na\u00EFve\"  \"nai\u0308ve\")
;; ↪ nil


(string= (mtg-normalize-string \"na\u00EFve\") (mtg-normalize-string \"nai\u0308ve\"))
;; ↪ t


=== Laws ===


This function:


• induces Equivalence-Classes on strings.


• is Idempotent.


=== Links ===


• Wraps function ‘ucs-normalize-NFC-string’.


• URL ‘https://stackoverflow.com/questions/31429865/trie-for-unicode-character-set’


> Consider the word café, which can be represented as either:
> A = [U+0063 U+0061 U+0066 U+0065 U+0301] (ends with e and a combining accent)
> But also as:
> B = [U+0063 U+0061 U+0066 U+00E9] (end with é, the combined form)"


  (ucs-normalize-NFC-string text))


;;----------------------------------------------;;


(defun mtg-normalize-card-name (text)


  "Normalize TEXT (a ‘stringp’) as an ‘mtg-card-name’.


• Unicode —
• ASCII — 


=== Examples ===


(mtg-normalize-card-name \"café\")
;; ↪ “cafe”


(mtg-normalize-card-name \"CAFÉ\")
;; ↪ "cafe”


(mtg-normalize-card-name \"naïve\")
;; ↪ “naive”


(mtg-normalize-card-name \"na\u00EFve\")
;; ↪ “naive”


(mtg-normalize-card-name \"nai\u0308ve\")
;; ↪ “naive”"


  (inline-letevals ((TEXT text))
    (inline-quote
      ( ,TEXT))))


  (mtg-asciify-string
   (mtg-normalize-string
    (downcase text))))


;;----------------------------------------------;;


;;==============================================;;


;;----------------------------------------------;;


(define-inline mtg--struct-to-plist (struct &optional struct-type)


  "Return a PList of STRUCT's Slots' names & values.


STRUCT — any ‘defstruct’ (a ‘recordp’) 


Return — a Property-‘listp’ from ‘symbolp’s to ‘objectp’s.


=== Examples ===


M-: (defstruct (p2) (x 0) (y 0))
M-: (mtg--struct-to-plist #s(p2 1 2))
 ↪ '(x 1 y 2)


=== Related ===


(Info mode ‘(elisp)Property-Lists’ /
URL ‘https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html’)"


  (cl-assert (recordp struct) "STRUCT must be a ‘defstruct’ record.")


  (let* ((STRUCT-TYPE (or struct-type ( struct)))
(STRUCT-INFO (cdr (cl-struct-slot-info STRUCT-TYPE)))
 )


    (cl-loop for (SLOT-NAME SLOT-VALUE) on STRUCT-INFO by #'cadr  ;TODO
)))


         (SLOT-BINDINGS 
           (cl-loop for SLOT-NAME in SLOT-NAMES
                    with SLOT-VALUE = (cl-struct-slot-value struct-type SLOT-NAME object)
                    collect `(,SLOT-NAME ,SLOT-VALUE)))
           )


;;==============================================;;


(define-inline mtg--replace-string-in-string (string string-from string-into)


  "Replace STRING-FROM with STRING-INTO in STRING (all occurrences)."


  (inline-letevals (string (STRING-FROM (regexp-quote string-from) string-into)
    (inline-quote


      (replace-regexp-in-string ,STRING-FROM ,string-into ,string nil 'literal))))


;;----------------------------------------------;;


(define-inline mtg--replace-regexp-in-string (string regexp-from string-into)


  "Replace REGEXP-FROM with STRING-INTO in STRING (all occurrences)."


  (inline-letevals (string (REGEXP-FROM regexp-from) string-into)
    (inline-quote


      (replace-regexp-in-string ,REGEXP-FROM ,string-into ,string nil nil))))


;;----------------------------------------------;;


(define-inline mtg--propertize-concat (&rest args)


  "Return a ‘propertize’d ‘stringp’ from  ARGS.


ARGS is a list of strings and plists.  The strings in ARGS are
concatenated to produce an output string.  In the output string,
each string from ARGS will be have the preceding plist as its
property list, or no properties if there is no plist before it.
As a simple example,


None of the ARGS are modified, but the return value may share
structure with the plists in ARGS.


=== Examples ===


M-: \(mtg--propertize-concat \"foo \" \\='(face italic) \"bar\" \" baz\" nil \
\" quux\")


 ↪ \"foo bar baz quux\"


 ;; would return the string \"foo bar baz quux\" where the substring
\"bar baz\" has a `face' property with the value `italic'.


"


  (inline-letevals (args)
    (inline-quote


  (with-temp-buffer


    (cl-loop with PLIST = ()
             for ARG in ,args
             do TODO








    (cl-loop with current-plist = nil
             for x in ,args do


             (cl-etypecase x


               (string (let ((begin (point)))
                         (insert x)
                         (set-text-properties begin (point) current-plist)))


               (list (unless (zerop (mod (length x) 2))
                       (error "Odd number of args in plist: %S" x))
                     (setq current-plist x))))


    (buffer-string))))


;;----------------------------------------------;;


(defun mtg--button (string &rest properties)


  "Return (a copy of) STRING as a text-button with PROPERTIES.


See ‘make-text-button’."


  (with-temp-buffer


    (insert string)
    (apply #'make-text-button (point-min) (point-max) properties)
    (buffer-string)))


;;----------------------------------------------;;


(defun mtg--file-name-extensions (filepath &optional join-p)


  "Extract FILEPATH's File-Extensions.


Output:


• a ‘listp’ of ‘stringp’s, if JOIN-P is nil (the default).
• a ‘stringp’, if JOIN-P is non-nil.


Inputs:


• FILEPATH — a ‘stringp’.
Defaults to variable ‘buffer-file-name’.


• JOIN-P — an optional ‘booleanp’.


=== Examples ===


M-: (mtg--file-name-extensions \"~/example.tar.gz\")


 ↪ \\='(\"tar\" \"gz\")


M-: (mtg--file-name-extensions \"~/example.tar.gz\" t)


 ↪ \".tar.gz\"


M-: (mtg--file-name-extensions)


 ↪ \"el\"


=== Related ===


• function ‘file-name-extension’."


  (declare (side-effect-free t))


  (cl-check-type filepath '(or null string))
  (cl-check-type join-p   'boolean)


  (when-let* ((FILEPATH
 (or filepath buffer-file-name))


  (EXTENSIONS 
    (cl-loop for S = (file-name-base S)


             while (and S (not (string-blank-p S)))


    with E = (file-name-extension S)


    when (and E (not (string-blank-p E)))
   collect E into EXTENSIONS


    do (cl-callf file-name-sans-extension S)


    finally return EXTENSIONS)))


    (if join-p 
(string-join EXTENSIONS ".")
      EXTENSIONS)))


;;----------------------------------------------;;








;;----------------------------------------------;;


;; TODO:


(defun mtg--replace-matches-in (s &rest regexps)


  "Return a copy of S with all matches of REGEXPS removed.
Elements of REGEXPS may also be two-element lists \(REGEXP
SUBEXP), where SUBEXP is the number of a subexpression in
REGEXP.  In that case, only that subexpression will be removed
rather than the entire match."


  ;; Use a temporary buffer since replace-match copies strings, which
  ;; would lead to N^2 runtime.


;; (from ‘ert-filter-string’)


  (with-temp-buffer
    (insert s)
    (dolist (x regexps)
      (cl-destructuring-bind (regexp subexp) (if (listp x) x `(,x nil))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match "" t t nil subexp))))
    (buffer-string)))




































































FONT


use a Variable-Pitch Font, at least for “prose” (Rules Text and Flavor Text), and maybe even for all “text” (including the Name Line and Type Line, to make sure they fit).


`string-width` for Variable-Pitch Font










fontset


for the prettification / abbreviation (e.g. `mtg-prettify-symbols-alist` and `mtg-symbolize-text`), make sure your font can display symbols (particularly in the TODO U+ABCD-WXYZ *Fontset*). 


;; Unicode Blocks for Symbols / Emoji:
;;
;; >Main articles: Dingbats (Unicode block), Emoticons (Unicode block), Miscellaneous Symbols (Unicode block), Miscellaneous Symbols and Pictographs (Unicode block), Supplemental Symbols and Pictographs (Unicode block), Symbols and Pictographs Extended-A (Unicode block), and Transport and Map Symbols (Unicode block)
Unicode 12.0 represents emoji using 1,311 characters spread across 24 blocks, of which 26 are Regional Indicator Symbols that combine in pairs to form flag emoji, and 12 (#, * and 0–9) are base characters for keycap emoji sequences:[1][78]
;; >
;; >637 of the 768 code points in the Miscellaneous Symbols and Pictographs block are considered emoji. 230 of the 244 code points in the Supplemental Symbols and Pictographs block are considered emoji. All of the 16 code points in the Symbols and Pictographs Extended-A block are considered emoji. All of the 80 code points in the Emoticons block are considered emoji. 97 of the 110 code points in the Transport and Map Symbols block are considered emoji. 82 of the 256 code points in the Miscellaneous Symbols block are considered emoji. 33 of the 192 code points in the Dingbats block are considered emoji.
;;
;;




















TEST


;; https://github.com/voxpupuli/puppet-mode/blob/1813c7bc46f178aeab5d78d5268dda0dd756c305/test/puppet-mode-test.el#L107


(defun test-mtg/test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.
If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (puppet-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face))


























;;; Effects






























;;; Related
;;
;; • ‘calculator-operators’
;; • ‘’
;;


















(TEMPLATES)




Inputs:


• — a ‘symbolp’.


Output:


• a ‘booleanp’.




(deftype mtg- ()


  `(or …))






;;


(defun _-dwim (&optional beg end)


  "“Do-What-I-Mean”: _ between BEG and END.


BEG — an optional ‘number-or-marker-p’.
Defaults to the ‘region-beginning’, or the current ‘line-beginning-position’.


END — an optional ‘number-or-marker-p’.
Defaults to the ‘region-end’, or the current ‘line-end-position’."


  (interactive "*r")  ; ‹*› means writable buffers only.
                      ; ‹r› means the current ‘region’.


  (cl-check-type BEG '(or null integer-or-marker))
  (cl-check-type END '(or null integer-or-marker))


  (let ((BEG (or beg (if (use-region-p) (region-beginning) (line-beginning-position))))
        (END (or end (if (use-region-p) (region-end) (line-end-position)))))
    (let ((BEG (min BEG END))
          (END (max END BEG))


      …)))


;;












;; :
;;
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;; • 
;;






;; :
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;; • 
;;
;;












;;----------------------------------------------;;


(defconst mtg-default-_-list


  `()


  "Default ‘mtg-_-list’.")


;;----------------------------------------------;;


(defvar mtg-_-list mtg-default-_-list


  ".")


;;----------------------------------------------;;






;;==============================================;;


(defimage mtg-svg-_-icon


  ((:type svg :data ""))


  "Image Specification for the “_ Icon”.")


;;==============================================;;


(defimage mtg-svg-_-symbol


  ((:type svg :data ""))


  "Image Specification for “{_}”, the _ Symbol (an ‘imagep’).")




;;----------------------------------------------;;


(defcustom mtg--list


  '(""
    ""
   )


  ".


Type: a ‘listp’ of ‘stringp’s."


  :type '(repeat (string :tag ""))


  :set        #'mtg-custom-set
  :initialize #'custom-initialize-default


  :safe #'listp
  :group 'mtg)


;;----------------------------------------------;;


(define-inline mtg--text (text)


  " TEXT.


=== Inputs/Output ===


• TEXT — a ‘stringp’.


• Return a ‘stringp’."


  (declare (side-effect-free t))


  (inline-letevals (text)
    (inline-quote
      ( ,text))))


;;----------------------------------------------;;


(define-inline mtg--p (object)


  "Return non-nil if OBJECT is a ."


  (declare (side-effect-free t))


  (inline-letevals ((OBJECT object))
    (inline-quote
      (eq ' (car-safe ,OBJECT)))))


;;----------------------------------------------;;


(define-inline mtg- (object)


  " OBJECT."


  (declare (side-effect-free t))


  (inline-letevals ((OBJECT object))
    (inline-quote
      ( ,OBJECT))))


;;----------------------------------------------;;


(defun mtg--string (text)


  "Parse TEXT.


Output: a ‘listp’ of ‘stringp’s.


Inputs:


• TEXT — a ‘stringp’.


Examples:


• M-: (mtg--string \"\")
   ↪ \\='()


  (declare (side-effect-free t))


  (cl-check-type text 'string)
  (cl-check-type  ')


  (cl-assert ())


  (let* (( ())
         ( ())
         )


    ()))


;;----------------------------------------------;;


(defun mtg--string ()


  " for ‘mtg-’.


Output: a ‘p’ of ‘p’s.


Inputs:


•  — a ‘p’.


Examples:


• M-: (mtg-- )
   ↪ \\='()


  (declare (side-effect-free t))


  (cl-check-type  ')


  (cl-assert ())


  (let* (( ())
         ( ())
         )


    ()))


;;----------------------------------------------;;


(defconst mtg- nil
  "‘’ for ‘mtg-mode’.")


;;----------------------------------------------;;


;;==============================================;;


;;----------------------------------------------;;












TODO...


different naming conventions:


mtg-json-schema-edition-alist
mtg-edition-json-schema-alist
mtg-json/schema-edition-alist
mtg-json/edition-schema-alist
mtg-json-set-schema-alist
mtg-set-json-schema-alist
mtg-json-schema-set-alist








Links:


• URL ‘https://www.yawgatog.com/resources/magic-rules/’


• URL ‘https://scryfall.com/docs/api/card-symbols’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’


• URL ‘’