;;; mtg.el --- Magic The Gathering search engine and custom card editor -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0
;; Prefix: mtg-
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

;; Editor for “Magic: The Gathering”.
;;
;; Features include:
;;
;; • ‘mtg-mode’  — Major Mode for editing custom MTG Cards.
;; • ‘mtg-query’ — Search Engine for MTG Cards 
;; • ‘mtg-json’  — 
;; • ‘mtg-’      — 
;; • ‘mtg-’      — 
;; 
;; Integrations include:
;;
;; • ‘helm-mtg’    — ‘helm’ integration: Helm TUIs for .
;; • ‘company-mtg’ — ‘company’ integration: Company Backends for card names/types/keywords/….
;; • ‘yas-mtg’     — ‘yasnippet’ integration: Snippets for phrases/cycles/….
;; •
;;
;; ‘mtg-mode’ features:
;;
;; ① Write custom cards conveniently:
;;
;;    • Completion — Complete these groups of words/phrases:
;; 
;;        • Card Names  — There are ~20,000 card names.
;;        • Keywords    — i.e. Keyword Abilities / Keyword Actions / Ability Words.
;;        • Types       — i.e. Card Types / Sub Types / Super Types.
;;        • Editions    — i.e. Set Codes / Set Names.
;;
;;    ↪ Benefits include:
;; 
;;        • Type fewer characters.
;;        • Make fewer mistakes.
;;        • 
;;        • 
;; 
;;    • Formatting — 
;; 
;;        • Capitalization — 
;;        • Keywords    — i.e. Keyword Abilities / Keyword Actions / Ability Words.
;;        • Types       — i.e. Card Types / Sub Types / Super Types.
;;        • Editions    — i.e. Set Codes / Set Names.
;;
;;    ↪ Benefits include:
;; 
;;    • Skeletons — 
;;
;;        • Cycles  — e.g. by Color, by Rarity.
;;        • Phrases — e.g. typing “etb ” (i.e. e-t-b-SPC) automatically expands to “enters the battlefield ”.
;;
;;    ↪ Benefits include:
;;
;;    • Linting — 
;; 
;;        • Pitfalls — e.g. « The rules text “if ~ would die” isn't valid; instead, write “if ~ would be put into a graveyard from anywhere” or “when ~ dies” ».
;;        •  — 
;;        •  — 
;;        •  — 
;;
;;    ↪ Benefits include:
;;
;; ② Export your custom set as:
;;
;;     • [✓] MTGJSON          — as a ‹.json› file (a.k.a. a JSON Object), with the schema.
;;     • [✓] Magic Set Editor — as a ‹.tar› file (a.k.a. a Tar Archive), with the ‹.mse-set› schema.
;;     • [✓] /r/custommagic   — as a ‹.md› file (a.k.a. Markdown), in Reddit-flavored Markdown, with the Subreddit-specific pseudo-links.
;;     • [❌] MTG Salvation    — as a ‹.bb› file (a.k.a. BBCode). 
;;
;;    Render your custom set for:
;;
;;     • [✓] websites   — as a ‹.html› file (a.k.a. a Web Page), that's beautifully styled and completely standalone.
;;     • [❌] printers   — as a ‹.pdf› file (?),
;;
;;         • HTML Styling    — via SVG mana symbols, the “Belern” font, and appropriate Bold/Italic (e.g. italics for flavor/remdinder text).
;;         • Standalone HTML — all assets (e.g. artwork PNGs, CSS symbols) are embedded (via “Data URIs” and inline <style>s).
;;           Thus, no 
;;
;; ③ 
;;
;; ④ 
;;
;; ⑤ 
;;
;; ‘mtg-query’ features:
;;
;; ① DSL .
;;   e.g. « %u *instant` » finds
;;
;; ② No Installation — This file embeds most data.
;;
;;    Thus, as long as Emacs knows where ‘mtg.el’ is (i.e. within your ‘load-path’) and is able to run it successfully (i.e. to ‘load’ it), you can immediately begin querying.
;;    I.E. Without downloading anything else, without being required to put the right file in the right place, without losing track of any files, etc.
;;    . 
;;
;;    The current version of this file has all data (except coprighted artwork) for all cards from “Alpha/Beta/Unlimited” to “War Of The Spark”.
;;
;; ③ Offline — (Obviously.)
;;
;;     Unlike online MTG search engines (e.g. URL `https://scryfall.com' or URL `https://gatherer.wizards.com'),
;;    ‘mtg-query’ both runs offline (it's just the code in this file) and stores its data offline.
;;
;;    Thus, you don't need an internet connection or anyting to search though this relatively miniscule database.
;; 
;; ③ Extensibility — Emacs
;;
;;    • Data Extensibility — Search through custom cards and custom sets.
;;
;;      Register them (easily) wtih ‘mtg-sources’:
;;
;;          • e.g. TODO « (add-to-list 'mtg-sources "my-custom-set.mtg" »).
;;          • e.g. TODO « (add-to-list 'mtg-sources "https:///www.planesculptors.net/set/lorado" ») via ""https://drive.google.com/uc?export=download&id=1BY-t55McScHEDB09R7QMs52JHG1MTycp".
;;
;;    • Query Extensibility — Write your own predicates. For example:
;;
;;        • e.g. « `square » — “square-statted creatures” have the same power and toughness (i.e. ‹1/1›'s, ‹2/2›'s, ‹3/3›'s, ...).
;;          TODO « POWER==TOUGHNESS ».
;;
;;        • e.g. « *arbor »  — “arbor cards” are Treefolk-or-Forest cards.
;;          c.f. ‹Treefolk Harbinger›, which reads « When Treefolk Harbinger enters the battlefield, you may search your library for a Treefolk or Forest card, reveal it, then shuffle your library and put that card on top of it. »
;;          TODO « *treefolk;forest ».
;;
;; ④ 
;;
;; ⑤ 
;;
;;
;; ‘helm-mtg’ features:
;;
;; ①
;;
;; ②
;;
;; ③ 
;;
;; ‘company-mtg’ features:
;;
;; ① Annotations — Unlike ‘mtg-complete/*’, Company can annotate candidates. For example:
;;
;;        • Card Names  — are annotated with a summary of the card (color, type, cost).
;;        • Keywords    — are annotated with their Reminder Text.
;;        • Set Codes   — are annotated with their full names (e.g. « AN “Arabian Nights” »).
;;
;;   Completion was is helpful for
;;   Disambiguating between:
;;
;;        • Different Legends — e.g. ‹Borborygmus› vs ‹Borborygmus Enraged›. e.g. ‹Yagmoth's Bargain› vs ‹Yagmoth's Will›.
;;        • Similar Names    — are annotated with a summary of the card (color, type, cost).
;;        • Types    — are annotated with .
;;        • Sets     — are annotated with their full names (e.g. « AN “Arabian Nights” »).
;;
;;
;;   [Author's Note] I post a lot about MTG (on forums and with my group chat).
;;                   Way before this package, I wrote my own MTG Company Backend for card names,
;;                   Its usefulness was one of the inspirations for everthing else.
;;
;; ②
;;
;; ③ 
;;
;; ‘yas-mtg’ features:
;;
;; ① Unlike ‘mtg-skeleton/*’: — 
;;
;;        • 
;;        • 
;;        • 
;;        • 
;;
;; ②
;;
;; ③ 
;;
;; Links:
;;
;; • MTGJSON          — URL `https://mtgjson.com/'
;; • Magic Set Editor — URL `http://magicseteditor.sourceforge.net/'
;; • /r/custommagic   — URL `https://www.reddit.com/r/custommagic'
;; • MTG Salvation    — URL `https://www.mtgsalvation.com/forums/magic-fundamentals/custom-card-creation'
;; • Planesculptors   — URL `https:///www.planesculptors.net'
;;
;; • Scryfall — URL `https://scryfall.com'
;; • Gatherer — URL `https://gatherer.wizards.com'
;;
;; 

;;; Code:

;;----------------------------------------------;;
;;; Imports ------------------------------------;;
;;----------------------------------------------;;

;; builtin requirements:

(eval-when-compile
  (require 'rx)
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'json)
  (require 'url)
  (require 'map)
  (require 'seq))

;;==============================================;;

;; project requirements:

(progn
  (provide 'mtg-data))

;;----------------------------------------------;;
;;; Constants ----------------------------------;;
;;----------------------------------------------;;

(defconst mtg-version "0.0"

  "Package Version of ‘mtg.el’.")

;;----------------------------------------------;;

(defconst mtg-known-spell-cardtypes

  '(instant sorcery)

"Card Types which are known  to be Spell Types.

a ‘listp’ of ‘symbolp’s.

Related:

• ‘mtg-known-spell-supertypes’
• ‘mtg-card/is-known-spell-p’")

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
• PRINTINGS     — ‘car’ (i.e. the first item) should be the Original Printing.
                  ‘cdr’ are Re-Printings, in any order.
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
• PRINTINGS     ∷ a (nonempty) `listp' of `mtg-printing-p's.
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

  ;; Non-Mechanical & Internal (i.e. present on the card itself):

  (printings     nil)

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

                              printings
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
• PRINTINGS     — a `listp' of `mtg-printing-p's and/or Property-Lists thereof.
  To directly create a singleton ‘mtg-card-printings’, see “Inputs (Printing)” below.
• RULINGS       — a `stringp'.
• LEGALITIES    — a p`listp' (a Property-List).
• SCRYFALL      — a `stringp'.
• UUID          — a `stringp' or `symbolp'.

Inputs (Printing):

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
• ARTIST        — .

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
  (artist        nil))

;; M-: (mtg--create :abbr ' :name "")
;;  ⇒ #s(mtg- )

;;----------------------------------------------;;

(cl-defun make-mtg-printing (&key image flavor frame layout rarity edition typeline language artist date)

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

;;==============================================;;

(cl-defstruct (mtg-translations
               (:constructor mtg--create)
               (:copier      nil))

  abbr (name ""))

;; M-: (mtg--create :abbr ' :name "")
;;  ⇒ #s(mtg- )

;;==============================================;;

(cl-defstruct (mtg-language
               (:constructor mtg-language-create)
               (:copier      nil))

  name abbr endonym (flag nil))

;; M-: (mtg-language-create :name 'spanish :abbr 'es :endonym "Español")
;;  ⇒ #s(mtg-language spanish es "Español" nil)

;;==============================================;;

(cl-defstruct (mtg-edition
               (:constructor mtg-edition-create)
               (:copier      nil))

  abbr name (type 'expansion) (image nil))

;; M-: (mtg-edition-create :abbr 'abu :name "Alpha Beta Unlimited")
;;  ⇒ #s(mtg-edition abu "Alpha Beta Unlimited" nil)

;;==============================================;;

(cl-defstruct (mtg-block
               (:constructor mtg-block-create)
               (:copier      nil))

  abbr (name "") (editions '()))

;; M-: (mtg-block-create :abbr 'ia :name "Ice Age Block" :editions '())
;;  ⇒ #s(mtg-block ia "Ice Age Block" ())

;;==============================================;;

(cl-defstruct (mtg-card-ruling 
               (:constructor mtg-card-ruling-create)
               (:copier      nil))

  text (date nil))

;; M-: (mtg--create :abbr ' :name "")
;;  ⇒ #s(mtg- )

;;==============================================;;

(cl-defstruct (mtg-card-legality
               (:constructor mtg-card-legality-create)
               (:copier      nil))

  "

Legal Statuses:

legal
restricted
banned

Formats:

standard, modern, legacy, vintage, commander, future (future Standard), pauper, frontier, penny (Penny Dreadful), duel (Duel Commander), and oldschool (Old School 93/94).

"

   (legalities nil))

;; M-: (mtg--create :abbr ' :name "")
;;  ⇒ #s(mtg- )

;;==============================================;;

(cl-defstruct (mtg-symbol
               (:constructor mtg-symbol-create)
               (:copier      nil))

  name abbr (image nil) (char nil))

;; M-: (mtg-symbol-create :name 'tap :abbr 'T :image 'mtg-tap-symbol-svg-image :char 'mtg-tap-symbol-char)
;;  ⇒ #s(mtg-symbol tap T mtg-tap-symbol-svg-image mtg-tap-symbol-char)

;;==============================================;;

(cl-defstruct (mtg-rarity
               (:constructor mtg-rarity-create)
               (:copier      nil))

  name abbr color)

;; M-: (mtg-rarity-create :name 'rare :abbr 'r)
;;  ⇒ #s(mtg-rarity rare r nil)

;;----------------------------------------------;;
;;; Types: “Enums” -----------------------------;;
;;----------------------------------------------;;


;;----------------------------------------------;;
;;; Groups -------------------------------------;;
;;----------------------------------------------;;

(defgroup mtg nil

  "“Magic: The Gathering” Search Engine and (Custom-)Card Editor."

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/mtg.el")

  :prefix "mtg-"
  :group 'applications)

;;----------------------------------------------;;

(defgroup mtg-card nil

  "“Magic: The Gathering” Cards (add custom keywords, colors, etc)."

  :prefix "mtg-"
  :group 'mtg)

;;----------------------------------------------;;

(defgroup mtg-json nil

  "Read/Parse ‹.json› datafiles w.r.t. the URL `mtgjson.com' schemata."

  :prefix "mtg-"
  :group 'mtg)

;;----------------------------------------------;;
;;; Variables (‘mtg-card’) ---------------------;;
;;----------------------------------------------;;

(defcustom mtg-colors

  '(white blue black red green)

  "All MTG Colors.

This `listp' is Ring/Set. It determines:

• The canonical identifier for each color.
  (See `mtg-monocolor-alist' for abbreviations and knicknames.)

• The canonical (ring-)ordering among the bicolors.
  I.E. “white” before “blue”, but “green” before “white” (by default).

Customization:

• Custom Colors — You must register any colors introduced by your custom set. 
  For example, to customize programmatically, evaluate:

    M-: (add-to-list 'mtg-colors-list 'purple :append)
      ↪ '(white blue black red green purple)

  For example, to customize graphically, execute:

    M-x (customize-variable 'mtg-colors)

Related:

• "

  :type '(repeat (symbol :tag "MTG Color"))

  :safe #'listp
  :group 'mtg-card)

;;----------------------------------------------;;

(defcustom mtg-bicolors

  '(azorius dimir rakdos gruul selesnya
    orzhov golgari simic izzet boros)

  "All MTG “Bi-Colors” (i.e. two-color pairs).

This `listp' is Ring/Set. Like `mtg-colors', it determines:

• The canonical identifier for each bicolor.
  (See `mtg-bicolor-alist' for abbreviations and knicknames.)

• The canonical ordering among the bicolors.

Related:

— `mtg-guilds-alist' — Ravnica Knicknames (e.g. symbol `azorius' for symbol `wu')."

  :type '(repeat (symbol :tag "MTG “Bi-Color”"))

  :safe #'listp
  :group 'mtg-card)

;;;TODO:
 ;;
 ;; White + Blue = Azorius
 ;; Blue + Black = Dimir
 ;; Black + Red = Rakdos
 ;; Red + Green = Gruul
 ;; Green + White = Selesnya
 ;; White + Black = Orzhov
 ;; Blue + Red = Izzet
 ;; Black + Green = Golgari
 ;; Red + White = Boros
 ;; Green + Blue = Simic

;;----------------------------------------------;;

(defcustom mtg-tricolors

  '(bant esper grixis jund naya          ; ← Shards
    mardu temur abzan jeskai sultai)     ; ← Wedges

  "All MTG “Tri-Colors” (i.e. three-color triplets).

(See the documentation of `mtg-colors'.)

Ordering (by default):

• Shards then Wedges.
• Shards — the colors in `mtg-colors' order, and their Ally Colors.
  For example, “bant” is the first shard, since it's “white” (the first color)
  and white's allies.
• Wedges — the colors in `mtg-colors' order, and their Enemy Colors.
  For example, “mardu” is the first wedge, since it's “white”
  and white's enemies.

Related:

• "

  :type '(repeat (symbol :tag "MTG “Tri-Color”"))

  :safe #'listp
  :group 'mtg-card)

;;;TODO:
 ;;
 ;;  Red + green + black = Jund
 ;;  White + green + blue = Bant
 ;;  Black + red + blue = Grixis
 ;;  Green + white + red = Naya
 ;;  Blue + white + black = Esper

;;==============================================;;

(defcustom mtg-monocolor-alist

  `((w . white)
    (u . blue)
    (b . black)
    (r . red)
    (g . green))

  "Any abbreviations/knicknames of the `mtg-colors'.

An “Association List”, a `listp' of `consp's of `symbolp's.
Each `cdr' must be in `mtg-colors'.

This associates each ‘symbolp’ [TODO and/or ‘stringp’ and/or ‘characterp’?] with an MTG Color."

  :type '(alist :key-type   (symbol :tag "Alias")
                :value-type (symbol :tag "MTG Color"))

  :safe #'listp
  :group 'mtg-card)

;;----------------------------------------------;;

(defcustom mtg-bicolor-alist

  `(

    ;; Mana Costs:

    (wu . azorius)
    (ub . dimir)
    (br . rakdos)
    (rg . gruul)
    (gw . selesnya)
    (wb . orzhov)
    (bg . golgari)
    (gu . simic)
    (ur . izzet)
    (rw . boros)

    ;; Mana Costs (Reversed):

    (uw . azorius)
    (bu . dimir)
    (rb . rakdos)
    (rg . gruu)l
    (wg . selesnya)
    (bw . orzhov)
    (gb . golgari)
    (ug . simic)
    (ru . izzet)
    (wr . boros)

    ;;

    )

   "All MTG color-triplets (a.k.a. shardes & wedges)."

  :type '(repeat (symbol :tag "MTG Color"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-tricolor-alist

  `(

    ;; Pronounceable Mana Costs:

    (rug . temur)
    (bug . sultai)

    ;; :

    ;; :

    )

   "All MTG color-triplets (a.k.a. shardes & wedges)."

  :type '(repeat (symbol :tag "MTG Color"))

  :safe #'listp
  :group 'mtg)

;;;TODO:
;;  Blue + red + white = Jeskai (clan on Tarkir), Numot (dragon from Apocalypse) or Raka (from Rakavolver)
;;  Red + white + black = Mardu (clan on Tarkir), Oros (dragon from Apocalypse) or Dega (from Degavolver)
;;  Black + green + blue = Sultai (clan on Tarkir), Vorosh (dragon from Apocalypse) or Ana (from Anavolver)
;;  Green + blue + red = Temur (clan on Tarkir), Intet (dragon from Apocalypse) or Ceta (from Cetavolver)
;;  White + black + green = Abzan (clan on Tarkir) Teneb (dragon from Apocalypse) Necra (from Necravolver), Junk , or Doran

;; Informal usages:

;; Red + white + black = Borzhov
;; Red + green + blue = Grizzet
;; In addition, it's especially common for red + blue + green and black + blue + green to be called by their abbreviations — "RUG" and "BUG" — because these are names that are easy to remember and pronounce.

;;----------------------------------------------;;

(defcustom mtg-guild-list

  '(azorius
    dimir
    rakdos
    gruul
    selesnya
    orzhov
    izzet
    golgari
    boros
    simic)

   "All MTG guilds i.e. (color pairs)."

  :type '(repeat (symbol :tag "MTG Color"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

Four colors
Most decks do not have four full colors. As with three color enemies, if they reach this many colors, it's a shard with a splash of another color. So you're more likely to see something like "American splash black" instead of "Yore".

Names for four-color identities come from one of two sources:

The names of the Nephilims from Guildpact.
The names of the four-colour “guild identities” defined during Commander 2016's design.
Reference the one color the four-color combination is missing, thus Non-(color).
So the four colour identities' names are:

 Blue + black + red + green = Glint-Eye, or Chaos, or Non-white
 Black + red + green + white = Dune (or Dune-Brood), or Aggression, or Non-blue
 Red + green + white + blue = Ink-Treader, or Altruism, or Non-black
 Green + white + blue + black = Witch (or Witch-Maw), or Growth, or Non-red
White + blue + black + red = Yore (or Yore-Tiller), or Artifice, or Non-green

;;==============================================;;

(defcustom mtg-super-types-list

  '(basic legendary snow)

  "Known Super-Types.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Supertype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-types-list

  '(instant                             ; ?🗲
    sorcery                             ; ?
    land                                ; ?
    artifact                            ; ?
    enchantment                         ; ?
    creature                            ; ?
    planeswalker                        ; ?
    conspiracy                          ; ?
    )

  "Known Card-Types.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Card type"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-spell-subtypes-list

  '(

   )

  "Known Subtypes for spells (i.e. instants and sorceries).

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Subtype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-land-subtypes-list

  '(

   )

  "Known Subtypes for lands.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Subtype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-artifact-subtypes-list

  '(

   )

  "Known Subtypes for artifacts.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Subtype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-enchantment-subtypes-list

  '(

   )

  "Known Subtypes for enchantments.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Subtype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-creature-subtypes-list

  '(

   )

  "Known Subtypes for creatures.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Subtype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-planeswalker-subtypes-list

  '(

   )

  "Known Subtypes for planeswalkers.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Subtype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-subtypes-alist

  `((spell        . mtg-spell-subtypes-list)
    (land         . mtg-land-subtypes-list)
    (artifact     . mtg-artifact-subtypes-list)
    (enchantment  . mtg-enchantment-subtypes-list)
    (creature     . mtg-creature-subtypes-list)
    (planeswalker . mtg-planeswalker-subtypes-list)
   )

  "Known Subtypes, by Card-Type.

an association `listp':

• from `symbolp'
• to EITHER a `listp' of `symbolp's OR a `symbolp' thereof.

`mtg-subtypes-alist' represents both ‘instant’ and ‘sorcery’
(which are in `mtg-card-types-list') as ‘spell’
(which isn't in `mtg-card-types-list')."

  :type '(alist :key-type   (symbol :tag "Card type")
                :value-type (choice (variable :tag "List Variable")
                                    (repeat (symbol :tag "Subtypes"))))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-symbols

  '(
   )

  "MTG Symbols.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "MTG Symbol"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-symbol-alist

  `((tap                    . ,(make-mtg-symbol :name 'tap                    :abbr 'T   :char ?Ⓣ))
    (untap                  . ,(make-mtg-symbol :name 'untap                  :abbr 'Q   :char ?🅤))

    ;; Mana...

    (white-mana             . ,(make-mtg-symbol :name 'white-mana             :abbr 'W   :char ?🌞))
    (blue-mana              . ,(make-mtg-symbol :name 'blue-mana              :abbr 'U   :char ?🌢))
    (black-mana             . ,(make-mtg-symbol :name 'black-mana             :abbr 'B   :char ?💀))
    (red-mana               . ,(make-mtg-symbol :name 'red-mana               :abbr 'R   :char ?⛰))
    (green-mana             . ,(make-mtg-symbol :name 'green-mana             :abbr 'G   :char ?🌲))

    (colorless-mana         . ,(make-mtg-symbol :name 'colorless-mana         :abbr 'C   :char ?◇))
    (snow-mana              . ,(make-mtg-symbol :name 'snow-mana              :abbr 'S   :char ?❄))
    (energy-mana            . ,(make-mtg-symbol :name 'energy-mana            :abbr 'E   :char ?⚡))
    (variable-X-mana        . ,(make-mtg-symbol :name 'variable-X-mana        :abbr 'X   :char ?X))
    (variable-Y-mana        . ,(make-mtg-symbol :name 'variable-Y-mana        :abbr 'Y   :char ?Y))
    (variable-Z-mana        . ,(make-mtg-symbol :name 'variable-Z-mana        :abbr 'Z   :char ?Z))

    (phyrexian-white-mana   . ,(make-mtg-symbol :name 'phyrexian-white-mana   :abbr 'P/W :char ?ϕ))
    (phyrexian-blue-mana    . ,(make-mtg-symbol :name 'phyrexian-blue-mana    :abbr 'P/U :char ?ϕ))
    (phyrexian-black-mana   . ,(make-mtg-symbol :name 'phyrexian-black-mana   :abbr 'P/B :char ?ϕ))
    (phyrexian-red-mana     . ,(make-mtg-symbol :name 'phyrexian-red-mana     :abbr 'P/R :char ?ϕ))
    (phyrexian-green-mana   . ,(make-mtg-symbol :name 'phyrexian-green-mana   :abbr 'P/G :char ?ϕ))

    (monohybrid-white-mana  . ,(make-mtg-symbol :name 'monohybrid-white-mana  :abbr '2/W :char ?🌞))
    (monohybrid-blue-mana   . ,(make-mtg-symbol :name 'monohybrid-blue-mana   :abbr '2/U :char ?🌢))
    (monohybrid-black-mana  . ,(make-mtg-symbol :name 'monohybrid-black-mana  :abbr '2/B :char ?💀))
    (monohybrid-red-mana    . ,(make-mtg-symbol :name 'monohybrid-red-mana    :abbr '2/R :char ?⛰))
    (monohybrid-green-mana  . ,(make-mtg-symbol :name 'monohybrid-green-mana  :abbr '2/G :char ?🌲))

    (zero-generic-mana      . ,(make-mtg-symbol :name 'zero-generic-mana      :abbr '0   :char ?⓪))
    (one-generic-mana       . ,(make-mtg-symbol :name 'one-generic-mana       :abbr '1   :char ?⓵))
    (two-generic-mana       . ,(make-mtg-symbol :name 'two-generic-mana       :abbr '2   :char ?⓶))
    (three-generic-mana     . ,(make-mtg-symbol :name 'three-generic-mana     :abbr '3   :char ?⓷))
    (four-generic-mana      . ,(make-mtg-symbol :name 'four-generic-mana      :abbr '4   :char ?⓸))
    (five-generic-mana      . ,(make-mtg-symbol :name 'five-generic-mana      :abbr '5   :char ?⓹))
    (six-generic-mana       . ,(make-mtg-symbol :name 'six-generic-mana       :abbr '6   :char ?⓺))
    (seven-generic-mana     . ,(make-mtg-symbol :name 'seven-generic-mana     :abbr '7   :char ?⓻))
    (eight-generic-mana     . ,(make-mtg-symbol :name 'eight-generic-mana     :abbr '8   :char ?⓼))
    (nine-generic-mana      . ,(make-mtg-symbol :name 'nine-generic-mana      :abbr '9   :char ?⓽))
    )

  "Symbol metadata (abbreviations and endonyms).

`listp' of `mtg-symbol-p's:

• each `mtg-symbol-name’ should be in `mtg-symbols'."

  :type '(alist :key-type   (symbol     :tag "Symbol")
                :value-type (mtg-symbol :tag "Symbol Info"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-card-border-color-list

  '(black white silver)

  "Known Border Colors.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Border Color"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-frame-list

  '(old new timeshifted future)

  "Known Card Frames.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Card Frame"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-layout-list

  '(
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

  "Known Card Layouts.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Card Layout"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-languages

  '(english
    german
    french
    italian
    spanish
    portuguese
    japanese
    chinese
    russian
    taiwanese
    korean)

  "Language names.

`listp' of `symbolp's.

Languages into which cards have been translated."

  :type '(repeat (symbol :tag "Language Name"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-language-alist

  `((english    . ,(mtg-language-create :name 'english    :abbr 'en :endonym "English"   :flag "🇺🇸"))
    (german     . ,(mtg-language-create :name 'german     :abbr 'de :endonym "Deutsch"   :flag "🇩🇪"))
    (french     . ,(mtg-language-create :name 'french     :abbr 'fr :endonym "Français"  :flag "🇫🇷"))
    (italian    . ,(mtg-language-create :name 'italian    :abbr 'it :endonym "Italiano"  :flag "🇮🇹"))
    (spanish    . ,(mtg-language-create :name 'spanish    :abbr 'es :endonym "Español"   :flag "🇲🇽")) ; by population.
    (portuguese . ,(mtg-language-create :name 'portuguese :abbr 'pt :endonym "Português" :flag "🇧🇷")) ; by population.
    (japanese   . ,(mtg-language-create :name 'japanese   :abbr 'jp :endonym "日本語"    :flag "🇯🇵"))
    (chinese    . ,(mtg-language-create :name 'chinese    :abbr 'cn :endonym "简体中文"  :flag "🇨🇳"))
    (russian    . ,(mtg-language-create :name 'russian    :abbr 'ru :endonym "Русский"   :flag "🇷🇺"))
    (taiwanese  . ,(mtg-language-create :name 'taiwanese  :abbr 'tw :endonym "繁體中文"  :flag "🇹🇼"))
    (korean     . ,(mtg-language-create :name 'korean     :abbr 'ko :endonym "한국어"    :flag "🇰🇷")) ; by population.
    )

  "Language metadata (abbreviations and endonyms).

`listp' of `mtg-language-p's:

• each ‘mtg-language-name’ should be in `mtg-languages'."

  :type '(repeat (symbol :tag "Language Info"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-formats-list

  '(block
    classic
    commander
    extended
    legacy
    modern
    standard
    vintage)

  "Known MTG formats.

AN MTG Format is a set of MTG Sets, with its own Banned&Restricted List,
and (possibly) its own Rules changes.

`listp' of `symbolp's

Customization:

• Programmatically — via `add-to-list'.
• Graphically — via checklist widget."

  :type '(repeat (symbol :tag "MTG Format"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-rarities

  '(common uncommon rare mythic
    timeshifted land)

  "Known MTG Rarities.

`listp' of `symbolp's.."

  :type '(repeat (symbol :tag "Symbol Name"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-rarity-alist

  `((common      . ,(mtg-rarity-create :name 'common      :abbr 'C :color "black"))
    (uncommon    . ,(mtg-rarity-create :name 'uncommon    :abbr 'U :color "silver"))
    (rare        . ,(mtg-rarity-create :name 'rare        :abbr 'R :color "gold"))
    (mythic      . ,(mtg-rarity-create :name 'mythic      :abbr 'M :color "bronze"))
    ;;
    (timeshifted . ,(mtg-rarity-create :name 'timeshifted :abbr 'T :color "purple")))

  "Rarity metadata (abbreviations and endonyms).

`listp' of ‘consps’ (from ‘symbolp’ `mtg-rarity-p'):

• each ‘mtg-rarity-name’ should be in `mtg-rarity-list'."

  :type '(alist :key-type   (symbol     :tag "Rarity")
                :value-type (mtg-rarity :tag "Rarity Info"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-edition-name-list

  '(al
    be
    un
    rv
    summer
    e4
    e5
    e6
    e7
    e8
    e9
    e10
    m10
    m11
    m12
    m13
    m14
    m15
    ori
    an
    aq
    lg
    dk
    fe
    hl
    mr
    vi
    wl
    tp
    sh
    ex
    us
    ul
    ud
    mm
    ne
    pr
    in
    ps
    ap
    od
    tr
    ju
    on
    le
    sc
    mi
    ds
    dn5
    chk
    bok
    sok
    rav
    gp
    di
    ia
    ai
    cs
    tsts
    ts                                  ; ?⌛
    pc                                  ; ?꩜
    fut                                 ; ?👁
    lw
    mt
    shm
    eve
    ala
    cfx
    arb
    zen
    wwk
    roe
    som
    mbs
    nph
    isd
    dka
    avr
    rtr
    gtc
    dgm
    ths
    bng
    jou
    ktk
    frf
    dtk
    bfz
    ogw
    soi
    emn
    kld
    aer
    akh
    hou
    xln
    rix
    dom
    bbd
    m19
    c18
    grn
    rna
    war
    mh1
    m20
    c19
    )

  "Known MTG Editions.

a `listp' of `mtg-edition-p's.

URL `https://mtg.gamepedia.com/Template:List_of_Magic_sets'"

  :type '(repeat (symbol :tag "Edition"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-edition-kind-list

  '(expansion core reprint
    un box from-the-vault premium-deck duel-deck starter commander planechase archenemy promo vanguard masters conspiracy masterpiece)

  "Kinds of MTG Editions.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Edition Type"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-block-list

  (list (mtg-block-create :abbr 'antediluvian   :name "Antediluvian Sets"      :editions '())
        (mtg-block-create :abbr 'ordinal        :name "Ordinal Core Sets"      :editions '())
        (mtg-block-create :abbr 'cardinal       :name "Cardinal Core Sets"     :editions '())
        (mtg-block-create :abbr 'mirage         :name "Mirage"                 :editions '())
        (mtg-block-create :abbr 'rath           :name "The Rath Cycle"         :editions '())
        (mtg-block-create :abbr 'urza           :name "The Urza Cycle"         :editions '())
        (mtg-block-create :abbr 'masques        :name "Masques"                :editions '())
        (mtg-block-create :abbr 'invasion       :name "Invasion"               :editions '())
        (mtg-block-create :abbr 'odyssey        :name "Odyssey"                :editions '())
        (mtg-block-create :abbr 'onslaught      :name "Onslaught"              :editions '())
        (mtg-block-create :abbr 'mirrodin       :name "Mirrodin"               :editions '())
        (mtg-block-create :abbr 'kamigawa       :name "Kamigawa"               :editions '())
        (mtg-block-create :abbr 'ravnica        :name "Ravnica"                :editions '())
        (mtg-block-create :abbr 'iceage         :name "Ice Age"                :editions '())
        (mtg-block-create :abbr 'timespiral     :name "Time Spiral"            :editions '(ts pc fut))
        (mtg-block-create :abbr 'lorwyn         :name "Lorwyn"                 :editions '())
        (mtg-block-create :abbr 'shadowmoor     :name "Shadowmoor"             :editions '())
        (mtg-block-create :abbr 'alara          :name "Shards Of Alara"        :editions '())
        (mtg-block-create :abbr 'zendikar       :name "Zendikar"               :editions '())
        (mtg-block-create :abbr 'scars          :name "Scars Of Mirrodin"      :editions '())
        (mtg-block-create :abbr 'innistrad      :name "Innistrad"              :editions '())
        (mtg-block-create :abbr 'ravnica2       :name "Return To Ravnica"      :editions '())
        (mtg-block-create :abbr 'theros         :name "Theros"                 :editions '())
        (mtg-block-create :abbr 'khans          :name "Khans Of Tarkir"        :editions '())
        (mtg-block-create :abbr 'zendikar2      :name "Battle For Zendikar"    :editions '())
        (mtg-block-create :abbr 'shadows        :name "Shadows Over Innistrad" :editions '())
        (mtg-block-create :abbr 'kaladesh       :name "Kaladesh"               :editions '())
        (mtg-block-create :abbr 'amonkhet       :name "Amonkhet"               :editions '())
        (mtg-block-create :abbr 'ixalan         :name "Ixalan"                 :editions '())
        (mtg-block-create :abbr 'ravnica3       :name "Guilds of Ravnica"      :editions '())
        (mtg-block-create :abbr 'war            :name "War of the Spark"       :editions '())
        )

  "Known MTG Blocks (of MTG Editions).

a `listp' of `mtg-block-p's."

  :type '(repeat (mtg-block :tag "Block"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;



;;----------------------------------------------;;
;;; Variables (‘mtg-json’) ---------------------;;
;;----------------------------------------------;;

(defcustom mtg-search-path

  '(default-directory
    user-emacs-directory
    mtg-xdg-data-dir
    ".")

  "Search Path for data files.

a `listp' of: `stringp's-and/or `symbolp's."

  :type '(repeat (string :tag "Directory Literal")
                 (symbol :tag "Directory Variable"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-name-punctuation-characters-list

  (list ?\-
        ?\,
        ?\.
        ?\:
        ?\"
        ?\'
        ?\&
        ?\!
        ?\?
        )

  "Punctuation in Card Names

a `listp' of `characterp's.

For example, these black-bordered cards
have such punctuation characters:

• “Borrowing 100,000 Arrows”
• “To Arms!”
• “Looter il-Kor”
• “Yawgmoth's Will”"

  :type '(repeat (character))

  :safe #'listp
  :group 'mtg)

;; ^ « " !\"',-.01:?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzàáâéíöúû" »

;;----------------------------------------------;;

(defcustom mtg-english-card-name-downcased-words

  (when (bound-and-true-p mtg-known-english-card-name-downcased-words)
    mtg-known-english-card-name-downcased-words)

  "Words which are ‘downcase’d in Card Names.

Exceptions to English Titlecasing (which ‘capitalize’s most words), including:

• Prepositions
• MTG-specific modifiers — e.g. “en-Vec” and “il-Kor”.

a `listp' of `stringp's."

  :type '(repeat (string :tag "Word"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;
;;; Accessors ----------------------------------;;
;;----------------------------------------------;;

(cl-defun mtg-cards (&key type force quick)

  "Accessor for variable `mtg-cards'.

Inputs:

• TYPE — an optional `symbolp', one of:

  • symbol ‘vector’
  • symbol ‘list’

  If non-nil, convert the output to the ‘type-of’ TYPE.

  see “Sequence Type” w.r.t. `seq-into'.

• FORCE — a `booleanp'.
  (Defaults to nil).

• QUICK — a `booleanp'.
  (Defaults to nil).

Initialize `mtg-cards' from `mtg-json-file',
only if necessary (or if FORCE is non-nil)."

  (let* ((TYPE (or type))
         (DATA (cond (quick mtg-data/card-names-vector)

                     ((not (or mtg-cards (not force)))  (progn
                                                          (setq mtg-cards (mtg-read-cards))
                                                          mtg-cards))))
         )


    (if TYPE
        (seq-into DATA TYPE)
      DATA)))

;; ^ e.g.:
;;
;; M-: (length (mtg-cards :quick t :force nil))
;;   ↪ 19310
;;
;; M-: (type-of (mtg-cards :type 'list :quick t :force nil))
;;   ↪ 'cons
;;
;; M-: (type-of (mtg-cards :type 'vector :quick t :force nil))
;;   ↪ 'vector
;;

;;----------------------------------------------;;

(defun mtg-get-search-path ()

  "Gets and `eval's everything in variable `mtg-search-path'.

Output:

• a `listp' of `stringp's.

Examples:

• M-: (mtg-get-search-path)
    ↪ (\"/home/sboo/elisp/mtg/lisp/\" \"~/.emacs.d/\" \"./\")"

  (let* ()

    (cl-loop for STRING-OR-SYMBOL in mtg-search-path

       for STRING = (cl-typecase STRING-OR-SYMBOL
                      (string STRING-OR-SYMBOL)
                      (symbol (cond ((fboundp STRING-OR-SYMBOL) (funcall STRING-OR-SYMBOL))
                                    ((boundp  STRING-OR-SYMBOL) (symbol-value STRING-OR-SYMBOL))
                                    (t nil))))

       for DIR = (if STRING (file-name-as-directory STRING))

       if DIR
       collect DIR into SEARCH-PATH
       end

       finally return (seq-uniq SEARCH-PATH))))

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;

;;(cl-loop for STRING in mtg-data/card-names-vector do (intern STRING))

;;----------------------------------------------;;
;;; JSON ---------------------------------------;;
;;----------------------------------------------;;
;; JSON Variables:

(defcustom mtg-json-default-file

  "Vintage.json.gz"

  "Raw Data for `mtg-cards'.

A `stringp', a “File Location”, which can be:

• an absolute filepath.
• a relative filepath — must be under `mtg-search-path'.
• a URI — scheme must be HTTP(S).

File Contents are JSON or ELisp data.

Supported file extensions for the “File Location” are:

• ‹.gz›   — assumes the File Contents are compressed.
• ‹.json› — assumes the File Contents can be `json-read' as JSON.
• ‹.el›   — assumes the File Contents can be `read' as Elisp struct (i.e. no code)."

  :type '(choice
          (string :tag "File")
          (string :tag "URI"))

  :safe #'stringp
  :group 'mtg-json)

;;----------------------------------------------;;

(defcustom mtg-json-basenames

  '("Vintage" "Modern" "Standard" "mtg")

  "Basenames of the ‹mtg.json› datafile.

Order: from highest priority to lowest priority.

‹mtgjson.com› compiles different formats (“Vintage”, “Modern”, “Standard”)
under the same schema. `mtg-json-basenames' determines which are 
preferred and/or acceoptable, when present locally.
All can be parsed (successfully) by the same function `mtg-json-parse'.

URL `https://mtgjson.com/downloads/compiled/'"

  :type '(choice
          (string :tag "Filename"))

  :safe #'listp
  :group 'mtg-json)

;;==============================================;;
;; JSON Functions:

(cl-defun mtg-read-cards ()

  "Return an `mtg-cards' struct."

  TODO)

;;----------------------------------------------;;

(defun mtg-json-read (&optional json)

  "Read a « .json » file of MTG Cards.

Inputs:

• JSON — a `stringp'.
  Defaults to `mtg-json-file'.
  The location of an ‘mtg.json’ file,
  whose contents may be compressed.

Output:

• a `hash-table-p' of property-`listp's and/or `vectorp's.
  an Elisp Json Object."

  (let ((json-object-type 'hash-table)
        (json-array-type  'vector)
        (json-key-type    'keyword)
        )

    (let* ((JSON  (or json mtg-json-file))
           (TABLE (json-read-file JSON))
           )

      TABLE)))

;; ^ e.g.:
;;
;; M-: (defconst mtg-vintage-cards-plists (mtg-json-read "../data/Vintage.json.gz"))
;;
;; M-: (with-temp-file "../gitignored/Vintage.el" (prin1 mtg-vintage-cards (current-buffer)))
;;
;; ~$ gzip -c9 ./gitignored/Vintage.el > ./data/Vintage.el.gz
;;
;;

;;----------------------------------------------;;

(defun mtg-json-parse (&optional json)

  "Parse a « .json » file of MTG Cards.

Inputs:

• JSON — a `stringp'.
  Defaults to `mtg-json-file'.
  The location of an ‘mtg.json’ file, or of an ‘mtg.json.gz’ file
  (whose contents may be compressed by the program ‘gzip’).

Output:

• a set of `mtg-card's.

Notes:

• Schema — JSON's schema should conform to, at least, either:

    • the MTGJSON card schema.  [✓]
    • the Scryfall card schema. [❌] (TODO)

  ... as of circa 2019.
  i.e. the Json MUST HAVE all Required Fields of the two schemata (above);
  it MAY HAVE extra fields (which are gracefully ignored.)

Links:

• URL `https://mtgjson.com/downloads/compiled/'
• URL `https://scryfall.com/docs/api/cards'"

  (let* ((TABLE (mtg-read-cards json))

         )

    ()))

;; ^ e.g.:
;;
;; M-: (defconst mtg-vintage-cards-structs (mtg-json-parse "../data/Vintage.json.gz"))
;;

;;----------------------------------------------;;

;;----------------------------------------------;;

;;----------------------------------------------;;

;;----------------------------------------------;;

;;----------------------------------------------;;

;;----------------------------------------------;;

;;----------------------------------------------;;

(defun mtg-json/parse-rulings (array)

  "Parse an MTGJSON “rulings” array.

Input:

• ARRAY — any ‘'json-array-type’.

URL `https://mtgjson.com/structures/rulings/'"

  (cl-check-type array #'sequencep "a Json Array (c.f. ‘seq.el’)")

  (cl-loop )) 

;;----------------------------------------------;;

(defun mtg-json/parse-legalities (object)

  "Parse an MTGJSON “legalities” object.

Input:

• OBJECT — any ‘'json-object-type’, whose keys are any ‘json-key-type’.

URL `https://mtgjson.com/structures/legalities/'"

  (cl-check-type object #'mapp "a Json Object (c.f. ‘map.el’)")

  (cl-loop ))

;;----------------------------------------------;;

(defun mtg-json/parse-translations (object)

  "Parse an MTGJSON “foreignData” object.
Input:

• OBJECT — any ‘'json-object-type’, whose keys are any ‘json-key-type’.

URL `https://mtgjson.com/structures/foreign-data/'"

  (cl-check-type object #'mapp "a Json Object (c.f. ‘map.el’)")

  ())

;;----------------------------------------------;;

;;----------------------------------------------;;

(defun mtg-json/mtgjson/fetch ()

  "Fetch « .json » datafiles from URL `https://mtgjson.com' ."

  (progn

    `("wget" "https://mtgjson.com/json/AllCards.json.gz")
    `("wget" "https://mtgjson.com/json/AllSets.sqlite.gz")
    `("wget" "https://mtgjson.com/json/Vintage.json.gz")

    ()))

;;==============================================;;
;; JSON Utilities:

(defun mtg--read-lines (filepath)

  "Read/Parse FILEPATH, whose contents are one (quoted) string per line.

Input:

• FILEPATH — a `stringp'.

Output:

• a `listp' of `stringp's."

  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (mapcar #'read
     (split-string (buffer-string) "\n" t))))

;;----------------------------------------------;;
;;; Regexps ------------------------------------;;
;;----------------------------------------------;;

(defun mtg-card-name-regexp ()

  "Return a `regexpp' matching an ‘mtg-card-name’.

For example, this command matches these ‘mtg-card-name’s:

• “Empty the Warrens”
• “Borrowing 100,000 Arrows”"

  (let* ((CHAR-REGEXP
          (rx-to-string `(char alpha digit ,@mtg-card-name-punctuation-characters-list) t))

         (ARTICLE-REGEXP
          (regexp-opt mtg-english-card-name-downcased-words 'symbols))

         (WORD-REGEXP
          (rx-to-string `(or (regexp ,ARTICLE-REGEXP)
                             (and word-start
                                  (and (char upper digit)
                                       (0+ (regexp ,CHAR-REGEXP)))
                                  word-end))
                        t))

         (PHRASE-REGEXP
          (rx-to-string `(1+ (and blank (regexp ,WORD-REGEXP)))))
         )

    PHRASE-REGEXP))

;; ^ M-: (mtg-card-name-regexp)
;;     ⇒ "\\(?:\\(?:\\(a\\(?:nd\\|[nst]\\)?\\|b\\(?:ut\\|y\\)\\|en\\|f\\(?:or\\|rom\\)\\|i\\(?:nto\\|[ln]\\)\\|o[fnr]\\|t\\(?:he\\|o\\)\\|upon\\|with\\)\\|\\<[[:upper:]][!\"',-.:?[:digit:][:alpha:]]*\\>\\)+\\)"

;; e.g. ‘mtg-card-name’s:
;;
;; • card Ancestral Recall card
;; • card Looter il-Kor card
;; • card Borrowing 100,000 Arrows card
;; • card To Arms! card
;; • card Empty the Warrens card
;;
;; (re-search-backward "\\(?:\\(?:\\(a\\(?:nd\\|[nst]\\)?\\|b\\(?:ut\\|y\\)\\|en\\|f\\(?:or\\|rom\\)\\|i\\(?:nto\\|[ln]\\)\\|o[fnr]\\|t\\(?:he\\|o\\)\\|upon\\|with\\)\\|\\<[[:upper:]][!\"',-.:?[:digit:][:alpha:]]*\\>\\)+\\)")
;;

;;----------------------------------------------;;
;;; Search -------------------------------------;;
;;----------------------------------------------;;

;;

(defvar mtg-query-mode-syntax-table

  ()

  "Syntax Table for `mtg-query-mode'.")

;;

(define-derived-mode mtg-query-mode fundamental-mode

  ()

  "Major Mode for writing “MTQ Queries”.

`mtg-query' parses its minibuffer via `mtg-query-mode-syntax-table'.
Because it doesn't initialize `mtg-query-mode' itself
(i.e. enable `font-lock-mode' / run `
`mtg-query-mode-hook' / etc),
you can customize this mode without slowing down the command.")

;;

;; (defun mtg-search-parse-filter (filter)

;;   "Parse the search-FILTER into a plist of search-constraints.

;; Inputs:

;; • FILTER — a `stringp'.

;; Output:

;; • a `listp' of `keywordp's and `atomp's (a plist)."

;;   (let ((must-have ())
;;         (must-not-have ())
;;         (after nil)
;;         (matches ())
;;         (not-matches ())
;;         (limit nil)
;;         (feeds ()))

;;   ;TODO; split the string on whitespace, while preserving (horizontal) whitespace within quotation marks. see `csv-split-string' (`csv-mode''s parser)?

;;     (let* ((FILTER (or filter)))

;;       (cl-loop for CHAR in FILTER

;;          for CHAR-KIND = (cl-case CHAR
;;                            (?\" 'quote)
;;                            (?\( 'open-paren)
;;                            (?\) 'close-paren)
;;                            (_ nil))

;;          do ))



;;     (cl-loop for TOKEN in FILTER-TOKENS

;;              for type = (aref TOKEN 0)

;;              do (cl-case type

;;                   (?+
;;                    (let ((symbol (intern (substring element 1))))
;;                      (unless (eq '## symbol)
;;                        (push symbol must-have))))

;;                   (?-
;;                    (let ((symbol (intern (substring element 1))))
;;                      (unless (eq '## symbol)
;;                        (push symbol must-not-have))))

;;                   (?@ (setf after (elfeed-time-duration (substring element 1))))

;;                   (?! (let ((re (substring element 1)))
;;                         (when (elfeed-valid-regexp-p re)
;;                           (push re not-matches))))
;;                   (?# (setf limit (string-to-number (substring element 1))))
;;                   (?= (let ((url (substring element 1)))
;;                         (push url feeds)))
;;                   (otherwise (when (elfeed-valid-regexp-p element)
;;                                (push element matches)))))

;;     `(,@(when after
;;           (list :after after))
;;       ,@(when must-have
;;           (list :must-have must-have))
;;       ,@(when must-not-have
;;           (list :must-not-have must-not-have))
;;       ,@(when matches
;;           (list :matches matches))
;;       ,@(when not-matches
;;           (list :not-matches not-matches))
;;       ,@(when limit
;;           (list :limit limit))
;;       ,@(when feeds
;;           (list :feeds feeds)))))

;; Examples:
;;
;; * the `^ancestral *instant @draw` pattern — narrows to Card-Draw Instants. i.e. cards:
;;
;;     - whose Card Name starts with *Ancestral*.
;;     - whose Card Type includes *Instant*.
;;     - whose Rules Text includes *draw* case-insensitive and between word-boundaries. (e.g. *Draw ...* and *“... draw.”* both match, but *drawn* doesn't match).
;;
;; * the `elf *elf,warrior 1/` pattern — narrows to Card-Draw Instants. i.e. cards:
;;
;;    - whose Card Name includes the word *Elf*.
;;    - whose Card Type includes both *Elf* and *Warrior*.
;;    - whose Power is exactly `1` (and whose Toughness can be anything.)
;;

;; Related:
;;
;; • `elfeed-search-parse-filter'
;; • `helm'
;;

;; Links:
;;
;; • URL `https://github.com/skeeto/elfeed'
;; • URL `https://nullprogram.com/blog/2016/12/27/'
;; • URL `https://nullprogram.com/blog/2016/12/11/'
;;

;; Elfeed:
;;
;; e.g. Elfeed Filter:
;;
;; Here’s a filter that finds all entries from within the past year tagged “youtube” (+youtube) that mention Linux or Linus (linu[sx]), but aren’t tagged “bsd” (-bsd), limited to the most recent 15 entries (#15):
;;
;;     @1-year-old +youtube linu[xs] -bsd #15
;;
;;

;;----------------------------------------------;;
;;; Motion -------------------------------------;;
;;----------------------------------------------;;

(progn

  (defun mtg-forward-card-name (&optional count)

    "Move across MTG Card Names.

Inputs:

 • COUNT — an optional `integerp'.
   When `called-interactively-p', the “Prefix Argument”.
   If COUNT is:
       ° positive — Move forwards to (the end of) the next card name.
       ° negative — Move backwards to (the end of) the prior card name.

Effects:

• Moves `point'.

Metadata:

• Registered with ‘thingatpt.el’ as `mtg-card-name’ —
  Implements the property `forward-op'
  for the “thing” symbol `mtg-card-name'.

Usage

• M-: (thing-at-point 'mtg-card-name)

Notes:

• Card Names, idiomatically:

    • must end with, and should start with, a `capitalize'd word.
      (should mostly contain capitalized words, anyways).
    • may have some punctuation characters (comma, hyphen, apostrophe, colon).
    • may include `downcase'd articles.

For example, this command skips across/until these ‘mtg-card-name’s:

• “Empty the Warrens”
• “Borrowing 100,000 Arrows”"

    (interactive "P")

    (let* ((COUNT (or count +1))
           )

      (if (natnump COUNT)

          (re-search-forward (mtg-card-name-regexp) nil t COUNT)

        (while (< COUNT 0)

          (if (re-search-backward (mtg-card-name-regexp) nil t)
              (skip-syntax-backward "w_"))

          (setq COUNT (1+ COUNT))))))

  (put 'mtg-card-name 'forward-op 'mtg-forward-card-name))

;; ^ Notes:
;;
;; • « (put '_ 'forward-op #'_) » registers a “Thing” for `thingatpt.el'.
;; • `forward-symbol' calls `re-search-forward': « (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move arg) »
;; • « "\\(\\sw\\|\\s_\\)+" » matches one-or-more Symbol Characters (w.r.t. Syntax Class)
;;   and/or Word Characters (w.r.t. Syntax Class).
;; •
;;

;;----------------------------------------------;;
;;; Styling ------------------------------------;;
;;----------------------------------------------;;

(cl-defun mtg-propertize-card-name (text)

  "`propertize' TEXT as a Card Name.

Inputs:

• TEXT — a `stringp'.

Output:

• a `stringp'.
  TEXT, with different Text Properties.

Properties:

• Italics — Italicize some prefixes,
  like the “il” in  “il-Kor”.
"

  ())

;;----------------------------------------------;;

(cl-defun mtg-propertize-rules-text (text)

  "`propertize' TEXT as Rules Text.

Inputs:

• TEXT — a `stringp'.

Output:

• a `stringp'.
  TEXT, with different Text Properties."

  ())

;;----------------------------------------------;;

(cl-defun mtg-propertize-flavor-text (text)

  "`propertize' TEXT as Flavor Text.

Inputs:

• TEXT — a `stringp'.

Output:

• a `stringp'.
  TEXT, with different Text Properties."

  ())

;;----------------------------------------------;;
;;; Integration: ‘seq.el’ ----------------------;;
;;----------------------------------------------;;

;; ‘seq.el’ integration for ‘mtg’ types. i.e.:
;;
;; • the ‘cl-defmethod’s implements sequence-generic functions.
;;
;;
;;
;;----------------------------------------------;;

(cl-defmethod seqp ((_ ,mtg-card))

  t)

;;----------------------------------------------;;

(cl-defmethod seqp ((_ ,mtg-set))

  t)

;;----------------------------------------------;;

;; (cl-defmethod seq-elt ((stream stream) n)
;;   "Return the element of STREAM at index N."
;;   (while (> n 0)
;;     (setq stream (stream-rest stream))
;;     (setq n (1- n)))
;;   (stream-first stream))

;; (cl-defmethod seq-length ((stream stream))
;;   "Return the length of STREAM.
;; This function will eagerly consume the entire stream."
;;   (let ((len 0))
;;     (while (not (stream-empty-p stream))
;;       (setq len (1+ len))
;;       (setq stream (stream-rest stream)))
;;     len))

;; (cl-defmethod seq-subseq ((stream stream) start end)
;;   (seq-take (seq-drop stream start) (- end start)))

;; (cl-defmethod seq-into-sequence ((stream stream))
;;   "Convert STREAM into a sequence."
;;   (let ((list))
;;     (seq-doseq (elt stream)
;;       (push elt list))
;;     (nreverse list)))

;; (cl-defmethod seq-into ((stream stream) type)
;;   "Convert STREAM into a sequence of type TYPE."
;;   (seq-into (seq-into-sequence stream) type))

;; (cl-defmethod seq-into ((stream stream) (_type (eql stream)))
;;   stream)

;; (cl-defmethod seq-into ((seq sequence) (_type (eql stream)))
;;   (stream seq))

;; (cl-defmethod seq-take ((stream stream) n)
;;   "Return a stream of the first N elements of STREAM."
;;   (if (or (zerop n)
;;           (stream-empty-p stream))
;;       (stream-empty)
;;     (stream-cons
;;      (stream-first stream)
;;      (seq-take (stream-rest stream) (1- n)))))

;; (cl-defmethod seq-drop ((stream stream) n)
;;   "Return a stream of STREAM without its first N elements."
;;   (stream-make
;;    (while (not (or (stream-empty-p stream) (zerop n)))
;;      (setq n (1- n))
;;      (setq stream (stream-rest stream)))
;;    (unless (stream-empty-p stream)
;;      (cons (stream-first stream)
;;            (stream-rest stream)))))

;; (cl-defmethod seq-take-while (pred (stream stream))
;;   "Return a stream of the successive elements for which (PRED elt) is non-nil in STREAM."
;;   (stream-make
;;    (when (funcall pred (stream-first stream))
;;      (cons (stream-first stream)
;;            (seq-take-while pred (stream-rest stream))))))

;; (cl-defmethod seq-drop-while (pred (stream stream))
;;   "Return a stream from the first element for which (PRED elt) is nil in STREAM."
;;   (stream-make
;;    (while (not (or (stream-empty-p stream)
;;                    (funcall pred (stream-first stream))))
;;      (setq stream (stream-rest stream)))
;;    (unless (stream-empty-p stream)
;;      (cons (stream-first stream)
;;            (stream-rest stream)))))

;; (cl-defmethod seq-map (function (stream stream))
;;     "Return a stream representing the mapping of FUNCTION over STREAM.
;; The elements of the produced stream are the results of the
;; applications of FUNCTION on each element of STREAM in succession."
;;   (stream-make
;;    (when (not (stream-empty-p stream))
;;      (cons (funcall function (stream-first stream))
;;            (seq-map function (stream-rest stream))))))

;; (cl-defmethod seq-do (function (stream stream))
;;   "Evaluate FUNCTION for each element of STREAM eagerly, and return nil.

;; `seq-do' should never be used on infinite streams without some
;; kind of nonlocal exit."
;;   (while (not (stream-empty-p stream))
;;     (funcall function (stream-first stream))
;;     (setq stream (stream-rest stream))))

;; (cl-defmethod seq-filter (pred (stream stream))
;;   "Return a stream of the elements for which (PRED element) is non-nil in STREAM."
;;   (if (stream-empty-p stream)
;;       stream
;;     (stream-make
;;      (while (not (or (stream-empty-p stream)
;;                      (funcall pred (stream-first stream))))
;;        (setq stream (stream-rest stream)))
;;      (if (stream-empty-p stream)
;;          nil
;;        (cons (stream-first stream)
;;              (seq-filter pred (stream-rest stream)))))))

;;----------------------------------------------;;
;;; Integration: ‘map.el’ ----------------------;;
;;----------------------------------------------;;

;; ‘map.el’ integration for ‘mtg’ types. i.e.:
;;
;; • the ‘cl-defmethod’s implements mapping-generic functions.
;;
;;
;;
;;----------------------------------------------;;

(cl-defmethod mapp ((_ ,mtg-set))

  t)

;;----------------------------------------------;;

(cl-defmethod mapp ((_ ,mtg-))

  t)

;;----------------------------------------------;;

(cl-defmethod mapp ((_ ,mtg-))

  t)

;;----------------------------------------------;;

(cl-defmethod mapp ((_ ,mtg-))

  t)

;;----------------------------------------------;;

(cl-defmethod mapp ((_ ,mtg-))

  t)















;;----------------------------------------------;;
;;; ElDoc --------------------------------------;;
;;----------------------------------------------;;

(defun mtg-eldoc (&optional point)

  "`eldoc-function' for `mtg-mode'."

  (let* ((POINT (or point (point)))
         (NAME (mtg-card-name-at-point POINT))
         )

    ()))

;;----------------------------------------------;;

(defun mtg-eldoc/argument-case (string)

  "`eldoc-argument-case' for `mtg-mode'.

Inflect and fontify STRING for `eldoc-mode'.

=== Usage ===

    (setq-local eldoc-argument-case #'mtg-eldoc-argument-case)"

  (let* ((STRING (upcase string))
         )
    (propertize STRING 'face 'font-lock-variable-name-face)))

;;----------------------------------------------;;

(defun mtg-summarize (name)

  "Return a oneline summary of NAME.

Inputs:

• NAME — a `stringp'.
  A card name and/or edition name."

  (when-let* ((CARD (or (mtg-get-card-by-name name) (mtg-get-edition-by-name name)))


    ())))

;;----------------------------------------------;;
;;; Indentation --------------------------------;;
;;----------------------------------------------;;

(defsubst mtg/within-comment-p ()
  "Return non-nil if `point' is within a (oneline or multiline) comment"
  (nth 4 (syntax-ppss)))

;;----------------------------------------------;;
;;; Skeletons ----------------------------------;;
;;----------------------------------------------;;

(define-skeleton mtg-card

    "Prompt for a tag and insert it, optionally with attributes.
Completion and configuration are done according to `sgml-tag-alist'.
If you like tags and attributes in uppercase, customize
`sgml-transformation-function' to `upcase'."
  (funcall (or skeleton-transformation-function 'identity)
           (setq sgml-tag-last
		 (completing-read
		  (if (> (length sgml-tag-last) 0)
		      (format "Tag (default %s): " sgml-tag-last)
		    "Tag: ")
		  sgml-tag-alist nil nil nil 'sgml-tag-history sgml-tag-last)))
  ?< str |
  (("") -1 '(undo-boundary) (identity "&lt;")) |	; see comment above
  `(("") '(setq v2 (sgml-attributes ,str t)) ?>
    (cond
      ((string= "![" ,str)
       (backward-char)
       '(("") " [ " _ " ]]"))
      ((and (eq v2 t) sgml-xml-mode (member ,str sgml-empty-tags))
       '(("") -1 " />"))
      ((or (and (eq v2 t) (not sgml-xml-mode)) (string-match "^[/!?]" ,str))
       nil)
      ((symbolp v2)
       ;; Make sure we don't fall into an infinite loop.
       ;; For xhtml's `tr' tag, we should maybe use \n instead.
       (if (eq v2 t) (setq v2 nil))
       ;; We use `identity' to prevent skeleton from passing
       ;; `str' through `skeleton-transformation-function' a second time.
       '(("") v2 _ v2 "</" (identity ',str) ?> >))
      ((eq (car v2) t)
       (cons '("") (cdr v2)))
      (t
       (append '(("") (car v2))
               (cdr v2)
               '(resume: (car v2) _ "</" (identity ',str) ?> >))))))

;;----------------------------------------------;;
;;; Menu Bar -----------------------------------;;
;;----------------------------------------------;;

(defgroup mtg-menu nil

  "MTG Menu (in the Menu Bar)."

  :prefix 'mtg-menu
  :group 'mtg)

;;==============================================;;

(defun mtg-menu/customization-changed (variable value)

  "Function called when the MTG Menu customization has changed.
Set VARIABLE with VALUE, and force a rebuild of the MTG Menu."

  (if (and (featurep 'mtg) (mtg-menu/enabled-p))

      (progn ;; Unavailable until mtg has been loaded.

        (mtg-menu/hide)
        (set-default variable value)
        (mtg-menu/show))

    (set-default variable value)))

;;----------------------------------------------;;

(defun mtg-menu/enabled-p ()

  "Whether the MTG Menu is enabled.

Output:

• a `booleanp'."

  (or (bound-and-true-p mtg-mode)
      nil))

;;==============================================;;

(defcustom mtg-menu/menu-title "Open Recent"

  "Name of the MTG Menu."

  :type 'string
  :set #'mtg-menu/customization-changed

  :group 'mtg-menu)

;;----------------------------------------------;;

(defcustom mtg-menu/menu-path '("File")

  "Path where to add the MTG Menu.

If nil add it at top level (see also `easy-menu-add-item')."

  :type '(choice (const :tag "Top Level" nil)
          (sexp :tag "Menu Path"))
  :set 'mtg-menu/customization-changed

  :group 'mtg-menu)

;;----------------------------------------------;;

(defcustom mtg-menu/next-menu-title "Help"

  "Name of the menu before which the MTG Menu will be added.

If nil, add it at end of menu (see also `easy-menu-add-item')."

  :type '(choice (string :tag "Name")
          (const :tag "Last" nil))
  :set 'mtg-menu/customization-changed

  :group 'mtg-menu)

;;==============================================;;

(defun mtg-menu/show ()
  "Show the menu of recently opened files."
  (easy-menu-add-item
   (mtg-menu-bar) mtg-menu-path
   (list mtg-menu-title :filter 'mtg-make-menu-items)
   mtg-menu-before))

;;----------------------------------------------;;

(defun mtg-menu/hide ()
  "Hide the menu of recently opened files."
  (easy-menu-remove-item (mtg-menu--get-global-menubar) mtg-menu/menu-path
                         mtg-menu/menu-title))

;;==============================================;;

(defsubst mtg-menu--get-global-menubar ()

  "Return the `keymapp' of the global Menu Bar."

  (lookup-key global-map [menu-bar]))

;;----------------------------------------------;;
;;; Tool Bar -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;;; MTG Edit Mode -----------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; MTG Edit Mode: Syntax ----------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;;; MTG Search-Query Mode ---------------------;;
;;----------------------------------------------;;

(defgroup mtg-query nil

  "‘mtg-query-mode’ parses Search Queries."

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/mtg.el")
  :group 'mtg)

;;----------------------------------------------;;
;;; MTG Search-Query Mode: Syntax --------------;;
;;----------------------------------------------;;

;;;###autoload
(defvar mtg-query-mode-syntax-table

  (let ((TABLE (make-syntax-table))
        )

    ;; Prefix (Unary) Operators:
    ;;
    ;; (i.e. most are “Expression-Prefix Characters”)

    (modify-syntax-entry ?\` "'"   TABLE)
    (modify-syntax-entry ?\~ "'"   TABLE)

    (modify-syntax-entry ?\! "'"   TABLE)
    (modify-syntax-entry ?\@ "'"   TABLE)
    (modify-syntax-entry ?\# "'"   TABLE)
    (modify-syntax-entry ?\% "'"   TABLE)

    (modify-syntax-entry ?\& ". p" TABLE)
    ;; ^ « & » is also an Infix Operator (Conjunction).

    (modify-syntax-entry ?\| ". p" TABLE)
    ;; ^ « | » is also an Infix Operator (Disjunction).

    (modify-syntax-entry ?\* "'"   TABLE)

    (modify-syntax-entry ?\^ "_ p" TABLE)
    ;; ^ « ^ » is:
    ;;   • Semantically, a regexp (match the Beginning-of-Word).
    ;;   • Syntactically, a “Symbolic Character” (within symbols/words).
    ;;   • Syntactically, also an “Expression-Prefix Character”.
    ;;   
    ;;   

    (modify-syntax-entry ?\$ "_ p" TABLE)
    ;; ^ « $ », semantically a regexp (match the End-of-Word);
    ;;   syntactically, symbol within a words;
    ;;   is also an “expression-prefix”.

    (modify-syntax-entry ?\" "\"" TABLE)
    ;; ^ « " » is a string delimiter.

    ;; « - » is punctuation (as an operator),
    ;; but « -- » is a comment-starter:

    (modify-syntax-entry ?\- ". 123" TABLE)

    ;; Brackets (i.e. parens, curly braces, square braces):

    (modify-syntax-entry ?\( "()"    TABLE)
    (modify-syntax-entry ?\) ")("    TABLE)
    (modify-syntax-entry ?\[ "(]"    TABLE)
    (modify-syntax-entry ?\] ")["    TABLE)
    (modify-syntax-entry ?\{ "(}1nb" TABLE) ; « "n" » means: Multi-Line Coments can be nested.
    (modify-syntax-entry ?\} "){4nb" TABLE)

    ;; Infix (Binary) Operators:
    ;;
    ;; (i.e. most are “Symbolic Characters”)
    ;;

;; * `>`  — 
;; * `<`  — 
;; * `=`  — 
;; * `>=` — 
;; * `<=` — 
;; * `/=` — 
;; * `!=` — 

    ;; Whitespace (i.e. spaces, tabs, newlines) is conventional:

    (modify-syntax-entry ?\  " " TABLE)
    (modify-syntax-entry ?\t " " TABLE)
    (modify-syntax-entry ?\n ">" TABLE)
    ;; ^ « \n » is a “comment-ender”.

    ;;

    TABLE)

  "Syntax Table of MTG Query Mode.

a `syntax-table-p'.")

;; ^ Notes:
;; 
;; • e.g. in `emacs-lisp-mode', the apostrophe is *Syntactically-Classified* as an *Expression-Prefix Character*.
;;   “Expressions”, w.r.t “Expression-Prefix Character”, are TODO.
;;
;;   M-: (string (char-syntax ?\'))
;;     ↪ "'"
;;
;;   M-: (string (char-syntax ?\`))
;;     ↪ "'"
;;
;;   M-: (string (char-syntax ?\,))
;;     ↪ "'"
;;
;;   M-: (string (char-syntax ?\@))
;;     ↪ "_ p" TODO
;;
;;
;; • Syntax Table:
;;
;;   > ‘p’ identifies an additional prefix character for Lisp syntax. These characters are treated as whitespace when they appear between expressions. When they appear within an expression, they are handled according to their usual syntax classes.
;;   >
;;   > Expression prefixes: ‘'’
;;   > Characters used for syntactic operators that are considered as part of an expression if they appear next to one. In Lisp modes, these characters include the apostrophe, ‘'’ (used for quoting), the comma, ‘,’ (used in macros), and ‘#’ (used in the read syntax for certain data types).
;;
;;
;; • 
;;
;;

;;----------------------------------------------;;

;;;###autoload
(define-derived-mode mtg-query-mode fundamental-mode "MTG Queries"

  "Major Mode parses Search Queries.

‘mtg-query-mode’ derives from ‘fundamental-mode’."

  (progn

    

    ()))

;;----------------------------------------------;;
;;; MTG Search-Query Mode: Commands ------------;;
;;----------------------------------------------;;

;;;###autoload
(define-derived-mode mtg-table-mode tabulated-list-mode "MTG Results"

  "Major Mode renders Search Results.

‘mtg-table-mode’ derives from ‘tabulated-list-mode’."

  (progn

    (setq-local tabulated-list-format (mtg-table-list-format))

    (tabulated-list-init-header)

    ()))

;;----------------------------------------------;;
;;; MTG Search-Result Mode ---------------------;;
;;----------------------------------------------;;

(defgroup mtg-table nil

  "‘mtg-table-mode’ renders Search Results."

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/mtg.el")
  :group 'mtg)

;;==============================================;;

(defconst mtg-table-buffer-name "*MTG Cards*"

  "Default `buffer-name' for `mtg-table-mode'.

a `stringp'.")

;;----------------------------------------------;;

(defconst mtg-default-table-list-format

  (vector `("Name"      ,mtg-longest-card-name-length t)
          `("Cost"      nil t)
          `("Colors"    nil t)
          `("Type"      nil t)
          `("Body"      nil t)               ; both Power/Toughness and Loyalty.
          `("Rules"     nil t)

          `("Rarity"    nil t)
          `("Set"       nil t)
          `("Flavor"    nil t)
          `("Artist"    nil t)
          `("Frame"     nil t)
          `("Watermark" nil t)
          )

  "Default `tabulated-list-format' for `mtg-table-mode'.

a `vectorp' of `listp's of « (NAME WIDTH SORTER) » triplets.")

;;----------------------------------------------;;

(defcustom mtg-table-list-format-list

  nil

  "Table Format for `mtg-table-mode'.

a `listp' of columns (`symbolp's) and/or triplets (`listp's) with form:

    (NAME &optional WIDTH SORTER)

Ignored if nil, defaulting to `mtg-default-table-list-format'."

  :type '(repeat (choice (symbol :tag "Known Column Format")

                         (list (string :tag "Column Name")

                               (choice (integer   :tag "Minimum Column Width")
                                       (const nil :tag "Don't Pad Column"))

                               (choice (function  :tag "Sort Column by Comparator")
                                       (const t   :tag "Sort Column as String")
                                       (const nil :tag "Don't Sort Column")))))

  :safe #'listp
  :group 'mtg-table)

;;----------------------------------------------;;

(defun mtg-table/convert-to-tabulated-list-format (table-format)

  "Convert TABLE-FORMAT to conform to `tabulated-list-format'.

Notes:

• `mtg-table-list-format''s form, a `listp', is more customizeable.
• `mtg-default-table-list-format''s form, a `vectorp', is more efficient."

  (todo))

;;----------------------------------------------;;

(defun mtg-tabulated-list-entries ()

  "Return `tabulated-list-entries' for `mtg-table-mode'.

Table Entries:

• conform to `mtg-tabulated-list-format'.
• are distinguished by their “Multiverse ID”."

  ())

;;----------------------------------------------;;

(cl-defun mtg-table/clean-text (text &key width separator)

  "Return TEXT as a valid Table Entry (for `tabulated-list-mode').

Inputs:

• TEXT      — a `stringp'.

• WIDTH     — an `integerp' or nil.
  Defaults to nil (i.e. no maximum width).

• SEPARATOR — a `stringp' or nil.
  Defaults to “ | ”.

Transformations include:

• Replace newlines with “|” (i.e. a vertical bar).
• Truncate to a `string-width' of WIDTH, with “…” (i.e. ellipses)."

  (let* ((SEPARATOR (or separator " | "))      ;TODO (propertize " | " 'face 'mtg-table-text-separator)

         (TEXT-ONELINE
          (string-join (split-string (string-trim text) "[\f\n\r\v]+" :omit-nulls) SEPARATOR))

         (TEXT-TRUNCATED
          (if (and width (natnump width))
              (let ((PADDING  nil)
                    (ELLIPSIS "…")
                    (COLUMN-END width)
                    (COLUMN-BEG 0)
                    )
                (truncate-string-to-width TEXT-ONELINE COLUMN-END COLUMN-BEG PADDING ELLIPSIS))
            TEXT-ONELINE))
         )

    TEXT-TRUNCATED))

;; ^ Examples:
;;
;; M-: (mtg-table/clean-text " \nEnchant creature\n{G}: Regenerate enchanted creature.\n " :width nil)
;;   ⇒ "Enchant creature | {G}: Regenerate enchanted creature."
;;
;; M-: (mtg-table/clean-text " \nEnchant creature\n{G}: Regenerate enchanted creature.\n " :width 25)
;;   ⇒ "Enchant creature | {G}: …"
;;
;; M-: (mtg-table/clean-text " \nEnchant creature\n{G}: Regenerate enchanted creature.\n " :separator " ")
;;   ⇒ "Enchant creature {G}: Regenerate enchanted creature."
;;

;; ^ Notes:
;;
;; M-: (string-join (split-string (string-trim " \nEnchant creature\n{G}: Regenerate enchanted creature.\n ") "[\f\n\r\v]+" :omit-nulls) " | ")
;;   ⇒ "Enchant creature | {G}: Regenerate enchanted creature."
;;

;;----------------------------------------------;;

(defun mtg-table-list-format ()

  "Accessor for `mtg-table-list-format'.

Output:

• a Table Format.
  See `tabulated-list-format'.
  Defaults to `mtg-default-table-list-format'."

  (let* ((FORMAT-LIST   (bound-and-true-p mtg-table-list-format))
         (FORMAT-VECTOR (if FORMAT-LIST
                            (mtg-table/convert-to-tabulated-list-format FORMAT-LIST)
                          mtg-default-table-list-format))
         )

    FORMAT-VECTOR))

;;----------------------------------------------;;
;;; MTG Search-Query Mode: Syntax --------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; MTG Search-Query Mode: Commands ------------;;
;;----------------------------------------------;;

;;;###autoload
(define-derived-mode mtg-table-mode tabulated-list-mode "MTG Results"

  "Major Mode renders Search Results.

‘mtg-table-mode’ derives from ‘tabulated-list-mode’."

  (progn

    (setq-local tabulated-list-format (mtg-table-list-format))

    (tabulated-list-init-header)

    ()))

;;----------------------------------------------;;

;;;###autoload
(defun list-mtg-cards ()

  "List all MTG Cards.

Effects:

• Activate the “*MTG Cards*” buffer
  (creating it if necessary).
• Fetch/Munge the MTG Card data.
• Populate the that buffer with that data.
• Turn on `mtg-table-mode'."

  (interactive)

  (let* ((BUFFER (get-buffer-create mtg-table-buffer-name))
        )

  (progn

    (switch-to-buffer BUFFER)

    ()

    (tabulated-list-print ())

    (mtg-table-mode +1))))

;;----------------------------------------------;;
;;; Readers ------------------------------------;;
;;----------------------------------------------;;

;; “Readers” read/‘load’/deserialize/import MTG Types from different
;; formats or applications.
;;
;; 
;;

;;----------------------------------------------;;
;;; Readers: MTGJSON ---------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Readers: Scryfall --------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Readers: Magic Set Editor ------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Writers ------------------------------------;;
;;----------------------------------------------;;

;; “Writers” write/‘print’/render/export MTG Types into different
;; formats or applications.
;;
;; 
;;

;;----------------------------------------------;;
;;; Writers: MTGJSON ---------------------------;;
;;----------------------------------------------;;

;; e.g. JSON Syntax:
;;
;;     « {"mana": "{1}{U}{U}{G}" } »  »
;;

;;----------------------------------------------;;
;;; Writers: Magic Set Editor ------------------;;
;;----------------------------------------------;;

;; e.g. Conf Syntax:
;;
;;     « card:  »
;;

;;----------------------------------------------;;
;;; Writers: /r/custommagic --------------------;;
;;----------------------------------------------;;

;; e.g. Markdown Syntax:
;;
;;     « [1](/1)[U](/U)[U](/U)[G](/G) »
;;

;;----------------------------------------------;;
;;; Writers: MTG Salvation ---------------------;;
;;----------------------------------------------;;

;; e.g. BBCode Syntax:
;;
;;     « [mana]1UUG[/mana] »
;;

;;----------------------------------------------;;
;;; Writers: HTML ------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Writers: PS / PDF --------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; (Un/)Loading --------------------------------;;
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
;;; Effects ------------------------------------;;
;;----------------------------------------------;;

(when (bound-and-true-p mtg-setup-p)
  (mtg-setup))

;;----------------------------------------------;;
;;; Notes --------------------------------------;;
;;----------------------------------------------;;

;;
;;
;;

;; Syntax Table:
;;
;; > ‘p’ identifies an additional prefix character for Lisp syntax. These characters are treated as whitespace when they appear between expressions. When they appear within an expression, they are handled according to their usual syntax classes.
;;
;; > Expression prefixes: ‘'’
;; > Characters used for syntactic operators that are considered as part of an expression if they appear next to one. In Lisp modes, these characters include the apostrophe, ‘'’ (used for quoting), the comma, ‘,’ (used in macros), and ‘#’ (used in the read syntax for certain data types).
;;

;;----------------------------------------------;;
;;; EOF ----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg)

;;; mtg.el ends here