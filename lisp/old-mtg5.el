
(require 'ucs-normalize)

(defun mtg-json-read-file (file)
  "Read a «.json» FILE from MtgJson."
  (let ((json-object-type 'plist))
    (json-read-file file)))

(defconst mtg-json-data
  (mtg-json-read-file "./MH2.json.gz"))

(cl-defun mtg-intern (string &key (dfc nil))
  "‘intern’ STRING for elisp.

Output:

• Casing — Casing is normalized (to ‘downcase’).
• Separators — Separator characters (including: whitespace, underscore, forward/backward slashes, etc) are replaced with hyphens. Hyphens are preserved.
• Alphabet — The alphabet is simplified to ASCII, or Latin without accents (e.g. “Lim-Dûl” becomes symbol ‘lim-dul’).
• Punctuation — Other characters (non-separator punctuation) are dropped.
(e.g. “Yawgmoth's” becomes symbol ‘yawgmoths’). Non-graphical characters are dropped too. 
• English — Currently, only English-language phrases are considered (i.e. for the official/canonical names of cards/sets). Eventually, I might try to guess the language from non-Latin scripts, and munge that language's casing/punctuating/quoting/etc conventions.
• Non-Injectivity — While it's possible that unique card names can be collapsed onto the same symbol (e.g. ). Luckily, most such card names are either “degenerate”, in that they'd be too similar to already existing ones, or “unconventional”, in that they don't follow the standard naming conventions / writing style. However, if necessary, you can circumvent the reference system induced by ‘mtg-intern’ by using ‘mtg-symbol-value’ (which wraps ‘symbol-value’).

Input

• DFC (default being nil) can be `split' or `aftermath'.

Examples:

• M-: (mtg-intern \"Merfolk Looter\")
  → 'merfolk-looter
• M-: (mtg-intern \"Looter il-Kor\")
  → 'looter-il-kor
• M-: (mtg-intern \"Lim-Dûl's Vault\")
  → 'lim-dul-s-vault
• M-: (mtg-intern \"Concealing Curtains // Revealing Eye\")
  → 'concealing-curtains-revealing-eye
• M-: (mtg-intern \"Fire // Ice\" :dfc 'split)
  → 'fire-and-ice
• M-: (mtg-intern \"Never // Return\" :dfc 'aftermath)
  → 'never-to-return
;; • M-: (mtg-intern \"Yawgmoth's Will\")
;;   → 'yawgmoths-will
;; • M-: (mtg-intern \"Ætherize\")
;;   → 'aetherize
"

  (let* ((DOUBLE-SLASH (cl-case dfc
                          ('split     "and")
                          ('aftermath "to")
                          (t          "")))
         (CLEAN-STRING (downcase (ucs-normalize-NFC-string string)))
         (SMUSH-STRING (save-match-data
                         (string-join
                          (split-string
                           (string-replace "//" DOUBLE-SLASH CLEAN-STRING)
                           (rx (not alphanumeric)) t)
                          "-")))
         (ASCII-STRING (cl-loop for UNI-CHAR across SMUSH-STRING
                             for (ASCII-CHAR . _) = (get-char-code-property UNI-CHAR 'decomposition)
                             concat (string ASCII-CHAR)))
         )
    (intern ASCII-STRING)))

;;

(progn

(cl-defun mtg-parse-json-data (&optional (data mtg-json-data))
  ""
  (let* ((DATA (plist-get (plist-get data :data) :cards)))
    (cl-loop for DATUM across DATA
          with CARDS = (make-hash-table :test 'eq :size (length DATA))
          for NAME = (plist-get DATUM :name)
          for ID   = (mtg-intern NAME)
          for TEXT = (plist-get DATUM :text)
          for CARD = (list :name NAME
                           :text TEXT
                           )
          do (puthash ID CARD CARDS)
          finally return CARDS)))

(gethash 'wavesifter (mtg-parse-json-data)))

;; (:name "Wavesifter"
;;  :manaCost "{3}{G}{U}" :manaValue 5.0 :power "3" :toughness "2" :colors ["G" "U"] :colorIdentity ["G" "U"]
;;  :types ["Creature"] :subtypes ["Elemental"] :supertypes [] :layout "normal"
;;  :text
;;  "Flying\nWhen Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")\nEvoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)"
;;  :keywords ["Evoke" "Flying" "Investigate"]
;;  :rarity "common" :setCode "MH2" :number "217" :borderColor "black" :frameVersion "2015" :language "English" :artist "Nils Hamm"
;;  :printings ["J21" "MH2" "MKC"] 
;;  :identifiers (:multiverseId "522293" :scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" :scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0")
;;  :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af")
