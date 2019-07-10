

;;==============================================;;

;;; Data: Functions...

(defun mtg-data/card-names (&optional type)

  "Accessor for `mtg-data/card-names-vector'.

Inputs:

• TYPE — a `symbolp', one of:

    • symbol ‘list’.
    • symbol ‘vector’.
    • symbol ‘string’.

Output:

• a `sequencep'.
  If TYPE is non-nil, returns that `type-of' sequence.

Compatibility:

• For future-compatibility, consumers should either:

    • ❶ consume the output only as a `sequencep'
        (i.e. not as a `vectorp').
    • ❷ specify TYPE."

  (let* ()

    (if (and type (symbolp type))
        (seq-into mtg-data/card-names-vector type)
      mtg-data/card-names-vector)))

;; ^ M-: (defconst sboo-mtg-card-name-list (mtg-data/card-names 'list))

;;----------------------------------------------;;

(defun mtg-data/get-card-names-count ()

  "Derive the `length' of `mtg-data/card-names'.

Output:

• a `natnump'.
  Currently, ~20,000"

  (let* ((CARD-NAMES (mtg-data/card-names))
         )

    (length CARD-NAMES)))

;;----------------------------------------------;;

(defun mtg-data/get-longest-card-name-length ()

  "Derive the longest among `mtg-data/card-names'.

Output:

• a `natnump'.
  Currently, 33."

  (let* ((CARD-NAMES (mtg-data/card-names))
         )

    (seq-reduce (lambda (*LENGTH* *STRING*) (max *LENGTH* (length *STRING*)))
                CARD-NAMES
                0)))

;;----------------------------------------------;;

(defun mtg-data/get-card-name-articles-list ()

  ""

  (let* ((CARD-NAMES         (mtg-data/card-names 'list))

         (CARD-NAME-WORDS    (cl-loop for s in CARD-NAMES
                                collect (split-string s "[-/ ]+" :omit-nulls)
                                  into STRINGS
                                finally
                                  return (delete-dups (apply #'append STRINGS))))

         (CARD-NAME-ARTICLES (cl-loop for s in CARD-NAME-WORDS
                                if (mtg-data/word-is-alphabetic-and-is-not-capitalized-p s)
                                collect (intern s)
                                  into SYMBOLS
                                finally
                                  return (cl-sort SYMBOLS 'string< :key #'symbol-name)))
         )

    CARD-NAME-ARTICLES))

;;----------------------------------------------;;

(defun mtg-data/word-is-alphabetic-and-is-not-capitalized-p (s)

  ""

  (let* ((CHAR             (string-to-char s))
         (CHAR-LOWERCASE?  (= CHAR (downcase CHAR)))
         (CHAR-ALPHABETIC? (eq 'Ll (get-char-code-property CHAR 'general-category)))
         )

    (and CHAR-ALPHABETIC? CHAR-LOWERCASE?)))

;;----------------------------------------------;;

(defconst mtg-data/default-card-name-articles-list (mtg-data/get-card-name-articles-list)

  "Default `mtg-data/get-card-name-articles-list'.")

;; ^ (a abara\'s an and as at but by en for from il in into le legged o\' of on or the to upon with)

;; ^ (a an and as at but by en for from il in into le of on or the to upon with)

