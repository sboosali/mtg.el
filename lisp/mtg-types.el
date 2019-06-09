;;; mtg-types.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/mtg.el
;; Keywords: local
;; Created: 15 May 2019
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

;; Types for “Magic: The Gathering”.
;;
;; Customizatin Groups:
;;
;; • group `mtg'
;;
;; Record Types (via `cl-defstruct'):
;;
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;;
;; Enum Types (via `cl-defstruct'):
;;
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;;
;; Constructors include:
;;
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;;
;; Accessors include:
;;
;; • `mtg-'
;; • `mtg-'
;; • `mtg-'
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin requirements:

(eval-when-compile
  (require 'rx)
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'json)
  (require 'seq))

;;==============================================;;

;; project requirements:

(progn)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct (mtg-card
               (:constructor mtg-card-create)
               (:copier      nil))

  (name          nil)
  (cost          nil)
  (types         nil)
  (supertypes    nil)
  (subtypes      nil)
  (colors        nil)
  (oracle        nil)
  (power         nil)
  (toughness     nil)
  (loyalty       nil)

  (cmc           0)
  (coloridentity nil)

  (image         nil)
  (flavor        nil)
  (frame         nil)
  (layout        nil)
  (border        nil)
  (rarity        nil)
  (set           nil)
  (typeline      nil)
  (language      nil)
  (artist        nil)

  (date          nil)
  (identifiers   nil)
  (rulings       nil)
  (legality      nil)
  (scryfall      nil)

  )

;; M-: (mtg-card-create :name "" :cost "" :types "" :supertypes "" :subtypes "" :colors "" :oracle "" :power "" :toughness "" :loyalty "" :cmc 1 :coloridentity "" :image "" :flavor "" :frame "" :layout "" :rarity "" :typeline "" :language "" :artist "" :rulings "" :legality "" :scryfall "")
;;  ⇒

;;TODO color cmc supertypes subtypes layout watermark collector language

;; TODO legality  'legal 'banned' 'restricted 'illegal
;; TODO color     'white 'blue 'black 'red 'green
;; TODO language  'en ...

;;----------------------------------------------;;

(cl-defstruct (mtg-language
               (:constructor mtg-language-create)
               (:copier      nil))

  language abbreviation endonym (flag nil))

;; M-: (mtg-language-create :language 'spanish :abbreviation 'es :endonym "Español")
;;  ⇒ #s(mtg-language spanish es "Español" nil)

;;----------------------------------------------;;

(cl-defstruct (mtg-rarity
               (:constructor mtg-rarity-create)
               (:copier      nil))

  rarity abbreviation color)

;; M-: (mtg-rarity-create :rarity 'rare :abbreviation 'r)
;;  ⇒ #s(mtg-rarity rare r nil)

;;----------------------------------------------;;

(cl-defstruct (mtg-symbol
               (:constructor mtg-symbol-create)
               (:copier      nil))

  symbol abbreviation (image nil) (char nil))

;; M-: (mtg-symbol-create :symbol 'tap :abbreviation 'T :image 'mtg-tap-symbol-svg-image :char 'mtg-tap-symbol-char)
;;  ⇒ #s(mtg-symbol tap T mtg-tap-symbol-svg-image mtg-tap-symbol-char)

;;----------------------------------------------;;

(cl-defstruct (mtg-edition
               (:constructor mtg-edition-create)
               (:copier      nil))

  abbreviation name (type 'expansion) (image nil))

;; M-: (mtg-edition-create :abbreviation 'abu :name "Alpha Beta Unlimited")
;;  ⇒ #s(mtg-edition abu "Alpha Beta Unlimited" nil)

;;----------------------------------------------;;

(cl-defstruct (mtg-block
               (:constructor mtg-block-create)
               (:copier      nil))

  abbreviation (name "") (editions '()))

;; M-: (mtg-block-create :abbreviation 'ia :name "Ice Age Block" :editions '())
;;  ⇒ #s(mtg-block ia "Ice Age Block" ())

;;----------------------------------------------;;
;; Types: Constructors -------------------------;;
;;----------------------------------------------;;

(cl-defun make-mtg-card (&key name cost types supertypes subtypes colors oracle power toughness loyalty cmc coloridentity image flavor frame layout rarity typeline language artist date rulings legality scryfall)

  "Make an `mtg-card'. 

A Smart Constructor — validation & defaulting.

Inputs:

• NAME          — a `stringp'.
• COST          — a `listp' of `stringp's, or `stringp'.
• TYPES         — a `listp' of `stringp's.
• SUPERTYPES    — a `listp' of `stringp's.
• SUBTYPES      — a `listp' of `stringp's.
• COLORS        — a `listp' of `stringp's.
• ORACLE        — a `stringp'.
• POWER         — a natural `integerp', or `stringp'.
• TOUGHNESS     — a natural `integerp', or `stringp'.
• LOYALTY       — a natural `integerp', or `stringp'.
• CMC           — a natural `integerp'.
• COLORIDENTITY — a `listp' of `stringp's.
• IMAGE         — a `stringp'.
• FLAVOR        — a `stringp'.
• FRAME         — a `stringp'.
• LAYOUT        — a `stringp'.
• RARITY        — a `stringp'.
• TYPELINE      — a `stringp'.
• LANGUAGE      — a `stringp'.
• ARTIST        — a `stringp'.
• RULINGS       — a `stringp'.
• LEGALITY      — a `stringp'.
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

• `mtg-card-create'"

  (let* ((NAME          name)
         (COST          cost)
         (TYPES         types)
         (SUPERTYPES    supertypes)
         (SUBTYPES      subtypes)
         (COLORS        colors)
         (ORACLE        oracle)
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
         (SET           set)
         (TYPELINE      typeline)
         (LANGUAGE      language)
         (ARTIST        artist)
         (DATE          date)
         (IDENTIFIERS   identifiers)
         (RULINGS       rulings)
         (LEGALITY      legality)
         (SCRYFALL      scryfall)
         )

    (mtg-card-create :name NAME :cost COST :types TYPES :supertypes SUPERTYPES :subtypes SUBTYPES :colors COLORS :oracle ORACLE :power POWER :toughness TOUGHNESS :loyalty LOYALTY :cmc CMC :coloridentity COLORIDENTITY :image IMAGE :flavor FLAVOR :frame FRAME :layout LAYOUT :border BORDER :rarity RARITY :set SET :typeline TYPELINE :language LANGUAGE :artist ARTIST :date DATE :identifiers IDENTIFIERS :rulings RULINGS :legality LEGALITY :scryfall SCRYFALL)))

;;----------------------------------------------;;

(cl-defun make-mtg-symbol (&key symbol abbreviation (image nil) (char nil))

  "Make an `mtg-symbol'. 

A Smart Constructor — validation & defaulting.

Inputs:

• SYMBOL       — a `symbolp'.
• ABBREVIATION — a `symbolp'.
• IMAGE        — an optional `imagep', or `symbolp' thereof.
• CHAR         — an optional `characterp', or `symbolp' thereof.

Examples:

• M-: (make-mtg-symbol :symbol 'blue-mana :abbreviation 'u :char ?🌢)
   ⇒ #s(mtg-symbol blue-mana u nil 127778)"

  (let* ((SYMBOL       symbol)
         (ABBREVIATION abbreviation)
         (IMAGE        image)
         (CHAR         char)
         )

  (mtg-symbol-create :symbol       SYMBOL
                     :abbreviation ABBREVIATION
                     :image        IMAGE
                     :char         CHAR)))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup mtg

  nil

  "Customize “Magic: The Gathering”."

  :link (url-link :tag "GitHub" "https://github.com/sboosali/mtg.el")
 
  )

;;----------------------------------------------;;

(defcustom mboo-mtg-color-list

  '(white
    blue
    black
    red
    green)

  "All MTG colors."

  :type '(repeat (symbol :tag "Color"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-color-abbreviations-alist

  '(
    ( ?w . white)
    ( ?u . blue)
    ( ?b . black)
    ( ?r . red)
    ( ?g . green)
   )

  "Color abbreviations.

Associates characters with the colors they abbreviate."

  :type '(alist :key-type   (string :tag "Color Abbreviation")
                :value-type (string :tag "Color Name"))

  :safe #'listp
  :group 'mtg)

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

  :type '(repeat (symbol :tag "Color"))

  :safe #'listp
  :group 'mtg)

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

each. The less commonly used are the wedges, which involve two enemy color combinations: they're derived from the dragons of Planar Chaos, the volver cycle from Apocalypse, or more recently the five clans from Khans of Tarkir. Having only one allied color pair (the two enemies of a single color will be allied) limits deck cohesion, making their use infrequent.

More typically, enemy three color decks are not fully fleshed out in the colors. You're more likely to have an "Izzet splashed with green" deck than a "Ceta" deck.

 Red + green + black = Jund
 White + green + blue = Bant
 Black + red + blue = Grixis
 Green + white + red = Naya
 Blue + white + black = Esper
 Blue + red + white = Jeskai (clan on Tarkir), Numot (dragon from Apocalypse) or Raka (from Rakavolver)
 Red + white + black = Mardu (clan on Tarkir), Oros (dragon from Apocalypse) or Dega (from Degavolver)
 Black + green + blue = Sultai (clan on Tarkir), Vorosh (dragon from Apocalypse) or Ana (from Anavolver)
 Green + blue + red = Temur (clan on Tarkir), Intet (dragon from Apocalypse) or Ceta (from Cetavolver)
 White + black + green = Abzan (clan on Tarkir) Teneb (dragon from Apocalypse) Necra (from Necravolver), Junk citation, or Doran citation
Informal usages:

Red + white + black = Borzhov
Red + white + blue = USA/American/Patriot
(although note that Team America is actually black + blue + green)
Red + green + blue = Grizzet
(although it's usually Simic splashing red)
In addition, it's especially common for red + blue + green and black + blue + green to be called by their abbreviations — "RUG" and "BUG" — because these are names that are easy to remember and pronounce.

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

;;----------------------------------------------;;

(defcustom mtg-super-types-list

  '(basic
    legendary
    snow
    ongoing
    world)

  "Known Super-Types.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Supertype"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-types-list

  '(instant        ; ?🗲
    sorcery
    land
    artifact
    enchantment
    creature
    planeswalker
    conspiracy)

  "Known Card-Types.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Card type"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

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

  '(
    (spell        . mtg-spell-subtypes-list)
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

;;----------------------------------------------;;

(defcustom mtg-rules-keywords-list

  '(
    absorb
    affinity
    afflict
    aftermath
    amplify
    annihilator
    ascend
    aura-swap
    awaken
    banding
    battle-cry
    bestow
    bloodthirst
    bushido
    buyback
    cascade
    champion
    changeling
    cipher
    conspire
    convoke
    crew
    cumulative-upkeep
    cycling
    dash
    deathtouch
    defender
    delve
    dethrone
    devoid
    devour
    double-strike
    dredge
    echo
    embalm
    emerge
    enchant
    entwine
    epic
    equip
    escalate
    eternalize
    evoke
    evolve
    exalted
    exploit
    extort
    fabricate
    fading
    fear
    first-strike
    flanking
    flash
    flashback
    flying
    forecast
    fortify
    frenzy
    fuse
    graft
    gravestorm
    haste
    haunt
    hexproof
    hidden-agenda
    hideaway
    horsemanship
    improvise
    indestructible
    infect
    ingest
    intimidate
    kicker
    landwalk
    level-up
    lifelink
    living-weapon
    madness
    melee
    menace
    miracle
    modular
    morph
    myriad
    ninjutsu
    offering
    outlast
    overload
    partner
    persist
    phasing
    poisonous
    protection
    provoke
    prowess
    prowl
    rampage
    reach
    rebound
    recover
    reinforce
    renown
    replicate
    retrace
    ripple
    scavenge
    shadow
    shroud
    skulk
    soulbond
    soulshift
    splice
    split-second
    storm
    sunburst
    surge
    suspend
    totem-armor
    trample
    transfigure
    transmute
    tribute
    undaunted
    undying
    unearth
    unleash
    vanishing
    vigilance
    wither
    )

  "Known Keywords (in rules text).

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Keyword"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-symbol-list

  '(
   )

  "MTG Symbols.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "MTG Symbol"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-symbol-alist

  `(
    (tap . ,(mtg-symbol-create :symbol 'tap :abbreviation 'T :image 'mtg-tap-symbol-svg-image :char 'mtg-tap-symbol-char)
   )

  "Symbol metadata (abbreviations and endonyms).

`listp' of `mtg-symbol-p's:

• each ‘:symbol’ should be in `mtg-symbol-list'."

  :type '(repeat (symbol :tag "Symbol Info"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-card-border-color-list

  '(black
    white
    silver)

  "Known Border Colors.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Border Color"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-frame-list

  '(old
    new
    timeshifted
    future)

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

(defcustom mtg-language-list

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
    korean
    )

  "Language names.

`listp' of `symbolp's.

Languages into which cards have been translated."

  :type '(repeat (symbol :tag "Language Name"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-language-alist

  `(
    (english    . ,(mtg-language-create :language 'english    :abbreviation 'en :endonym "English"   :flag "🇺🇸"))
    (german     . ,(mtg-language-create :language 'german     :abbreviation 'de :endonym "Deutsch"   :flag ""))
    (french     . ,(mtg-language-create :language 'french     :abbreviation 'fr :endonym "Français"  :flag ""))
    (italian    . ,(mtg-language-create :language 'italian    :abbreviation 'it :endonym "Italiano"  :flag ""))
    (spanish    . ,(mtg-language-create :language 'spanish    :abbreviation 'es :endonym "Español"   :flag ""))
    (portuguese . ,(mtg-language-create :language 'portuguese :abbreviation 'pt :endonym "Português" :flag ""))
    (japanese   . ,(mtg-language-create :language 'japanese   :abbreviation 'jp :endonym "日本語"    :flag ""))
    (chinese    . ,(mtg-language-create :language 'chinese    :abbreviation 'cn :endonym "简体中文"  :flag ""))
    (russian    . ,(mtg-language-create :language 'russian    :abbreviation 'ru :endonym "Русский"   :flag ""))
    (taiwanese  . ,(mtg-language-create :language 'taiwanese  :abbreviation 'tw :endonym "繁體中文"  :flag ""))
    (korean     . ,(mtg-language-create :language 'korean     :abbreviation 'ko :endonym "한국어"    :flag ""))
    )

  "Language metadata (abbreviations and endonyms).

`listp' of `mtg-language-p's:

• each ‘:language’ should be in `mtg-language-list'."

  :type '(repeat (symbol :tag "Language Info"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-rarity-list

  '(common
    uncommon
    rare
    mythic)

  "Rarity names.

`listp' of `symbolp's.

Raritys into which cards have been translated."

  :type '(repeat (symbol :tag "Rarity Name"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-rarity-alist

  `(
    (common   . ,(mtg-rarity-create :rarity 'common   :abbreviation 'C :color "black"))
    (uncommon . ,(mtg-rarity-create :rarity 'uncommon :abbreviation 'U :color "silver"))
    (rare     . ,(mtg-rarity-create :rarity 'rare     :abbreviation 'R :color "gold"))
    (mythic   . ,(mtg-rarity-create :rarity 'mythic   :abbreviation 'M :color "bronze"))
    )

  "Rarity metadata (abbreviations and endonyms).

`listp' of `mtg-rarity-p's:

• each ‘:rarity’ should be in `mtg-rarity-list'."

  :type '(alist :key-type   (symbol     :tag "Rarity")
                :value-type (mtg-rarity :tag "Rarity Info"))

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

;;----------------------------------------------;;

(defcustom mtg-symbol-list

  '(common
    uncommon
    rare
    mythic)

  "Symbol names.

`listp' of `symbolp's.

Symbols into which cards have been translated."

  :type '(repeat (symbol :tag "Symbol Name"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-symbol-alist

  `(

    (tap                    . ,(make-mtg-symbol :symbol 'tap                    :abbreviation 'T   :char ?Ⓣ))
    (untap                  . ,(make-mtg-symbol :symbol 'untap                  :abbreviation 'Q   :char ?🅤))

    (white-mana             . ,(make-mtg-symbol :symbol 'white-mana             :abbreviation 'W   :char ?🌞))
    (blue-mana              . ,(make-mtg-symbol :symbol 'blue-mana              :abbreviation 'U   :char ?🌢))
    (black-mana             . ,(make-mtg-symbol :symbol 'black-mana             :abbreviation 'B   :char ?💀))
    (red-mana               . ,(make-mtg-symbol :symbol 'red-mana               :abbreviation 'R   :char ?⛰))
    (green-mana             . ,(make-mtg-symbol :symbol 'green-mana             :abbreviation 'G   :char ?🌲))

    (colorless-mana         . ,(make-mtg-symbol :symbol 'colorless-mana         :abbreviation 'C   :char ?◇))
    (snow-mana              . ,(make-mtg-symbol :symbol 'snow-mana              :abbreviation 'S   :char ?❄))
    (energy-mana            . ,(make-mtg-symbol :symbol 'energy-mana            :abbreviation 'E   :char ?⚡))
    (variable-X-mana        . ,(make-mtg-symbol :symbol 'variable-X-mana        :abbreviation 'X   :char ?X))
    (variable-Y-mana        . ,(make-mtg-symbol :symbol 'variable-Y-mana        :abbreviation 'Y   :char ?Y))
    (variable-Z-mana        . ,(make-mtg-symbol :symbol 'variable-Z-mana        :abbreviation 'Z   :char ?Z))

    (phyrexian-white-mana   . ,(make-mtg-symbol :symbol 'phyrexian-white-mana   :abbreviation 'P/W :char ?ϕ))
    (phyrexian-blue-mana    . ,(make-mtg-symbol :symbol 'phyrexian-blue-mana    :abbreviation 'P/U :char ?ϕ))
    (phyrexian-black-mana   . ,(make-mtg-symbol :symbol 'phyrexian-black-mana   :abbreviation 'P/B :char ?ϕ))
    (phyrexian-red-mana     . ,(make-mtg-symbol :symbol 'phyrexian-red-mana     :abbreviation 'P/R :char ?ϕ))
    (phyrexian-green-mana   . ,(make-mtg-symbol :symbol 'phyrexian-green-mana   :abbreviation 'P/G :char ?ϕ))

    (monohybrid-white-mana  . ,(make-mtg-symbol :symbol 'monohybrid-white-mana  :abbreviation '2/W :char ?🌞))
    (monohybrid-blue-mana   . ,(make-mtg-symbol :symbol 'monohybrid-blue-mana   :abbreviation '2/U :char ?🌢))
    (monohybrid-black-mana  . ,(make-mtg-symbol :symbol 'monohybrid-black-mana  :abbreviation '2/B :char ?💀))
    (monohybrid-red-mana    . ,(make-mtg-symbol :symbol 'monohybrid-red-mana    :abbreviation '2/R :char ?⛰))
    (monohybrid-green-mana  . ,(make-mtg-symbol :symbol 'monohybrid-green-mana  :abbreviation '2/G :char ?🌲))

    (zero-generic-mana      . ,(make-mtg-symbol :symbol 'zero-generic-mana      :abbreviation '0   :char ?⓪))
    (one-generic-mana       . ,(make-mtg-symbol :symbol 'one-generic-mana       :abbreviation '1   :char ?⓵))
    (two-generic-mana       . ,(make-mtg-symbol :symbol 'two-generic-mana       :abbreviation '2   :char ?⓶))
    (three-generic-mana     . ,(make-mtg-symbol :symbol 'three-generic-mana     :abbreviation '3   :char ?⓷))
    (four-generic-mana      . ,(make-mtg-symbol :symbol 'four-generic-mana      :abbreviation '4   :char ?⓸))
    (five-generic-mana      . ,(make-mtg-symbol :symbol 'five-generic-mana      :abbreviation '5   :char ?⓹))
    (six-generic-mana       . ,(make-mtg-symbol :symbol 'six-generic-mana       :abbreviation '6   :char ?⓺))
    (seven-generic-mana     . ,(make-mtg-symbol :symbol 'seven-generic-mana     :abbreviation '7   :char ?⓻))
    (eight-generic-mana     . ,(make-mtg-symbol :symbol 'eight-generic-mana     :abbreviation '8   :char ?⓼))
    (nine-generic-mana      . ,(make-mtg-symbol :symbol 'nine-generic-mana      :abbreviation '9   :char ?⓽))

    )

  "Symbol metadata (abbreviations and endonyms).

`listp' of `mtg-symbol-p's:

• each ‘:symbol’ should be in `mtg-symbol-list'."

  :type '(alist :key-type   (symbol     :tag "Symbol")
                :value-type (mtg-symbol :tag "Symbol Info"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-edition-type-list

  '(expansion
    core
    reprint
    box
    un
    from the vault
    premium deck
    duel deck
    starter
    commander
    planechase
    archenemy
    promo
    vanguard
    masters
    conspiracy
    masterpiece)

  "Known Types of MTG Editions.

a `listp' of `symbolp's."

  :type '(repeat (symbol :tag "Edition Type"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-edition-alist

  `(
    
   )

  "Known MTG Editions.

a `listp' of `mtg-edition-p's."

  :type '(repeat (mtg-edition :tag "Edition"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-edition-name-list

  '(
    al
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
    ts
    pc
    fut
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

(defcustom mtg-block-list

  (list (mtg-block-create :abbreviation 'antediluvian   :name "Antediluvian Sets"      :editions '())
        (mtg-block-create :abbreviation 'ordinal        :name "Ordinal Core Sets"      :editions '())
        (mtg-block-create :abbreviation 'cardinal       :name "Cardinal Core Sets"     :editions '())
        (mtg-block-create :abbreviation 'mirage         :name "Mirage"                 :editions '())
        (mtg-block-create :abbreviation 'rath           :name "The Rath Cycle"         :editions '())
        (mtg-block-create :abbreviation 'urza           :name "The Urza Cycle"         :editions '())
        (mtg-block-create :abbreviation 'masques        :name "Masques"                :editions '())
        (mtg-block-create :abbreviation 'invasion       :name "Invasion"               :editions '())
        (mtg-block-create :abbreviation 'odyssey        :name "Odyssey"                :editions '())
        (mtg-block-create :abbreviation 'onslaught      :name "Onslaught"              :editions '())
        (mtg-block-create :abbreviation 'mirrodin       :name "Mirrodin"               :editions '())
        (mtg-block-create :abbreviation 'kamigawa       :name "Kamigawa"               :editions '())
        (mtg-block-create :abbreviation 'ravnica        :name "Ravnica"                :editions '())
        (mtg-block-create :abbreviation 'iceage         :name "Ice Age"                :editions '())
        (mtg-block-create :abbreviation 'timespiral     :name "Time Spiral"            :editions '())
        (mtg-block-create :abbreviation 'lorwyn         :name "Lorwyn"                 :editions '())
        (mtg-block-create :abbreviation 'shadowmoor     :name "Shadowmoor"             :editions '())
        (mtg-block-create :abbreviation 'alara          :name "Shards Of Alara"        :editions '())
        (mtg-block-create :abbreviation 'zendikar       :name "Zendikar"               :editions '())
        (mtg-block-create :abbreviation 'scars          :name "Scars Of Mirrodin"      :editions '())
        (mtg-block-create :abbreviation 'innistrad      :name "Innistrad"              :editions '())
        (mtg-block-create :abbreviation 'ravnica2       :name "Return To Ravnica"      :editions '())
        (mtg-block-create :abbreviation 'theros         :name "Theros"                 :editions '())
        (mtg-block-create :abbreviation 'khans          :name "Khans Of Tarkir"        :editions '())
        (mtg-block-create :abbreviation 'zendikar2      :name "Battle For Zendikar"    :editions '())
        (mtg-block-create :abbreviation 'shadows        :name "Shadows Over Innistrad" :editions '())
        (mtg-block-create :abbreviation 'kaladesh       :name "Kaladesh"               :editions '())
        (mtg-block-create :abbreviation 'amonkhet       :name "Amonkhet"               :editions '())
        (mtg-block-create :abbreviation 'ixalan         :name "Ixalan"                 :editions '())
        (mtg-block-create :abbreviation 'ravnica3       :name "Guilds of Ravnica"      :editions '())
        (mtg-block-create :abbreviation 'war            :name "War of the Spark"       :editions '())
        )

  "Known MTG Blocks (of MTG Editions).

a `listp' of `mtg-block-p's."

  :type '(repeat (mtg-block :tag "Block"))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-block-names-list

  (mapcar #'mtg-block-abbreviation mtg-block-list)

  "Known MTG Blocks (of MTG Editions).

a `listp' of `mtg-block-p's."

  :type '(repeat (symbol :tag "Block"))

  :safe #'listp
  :group 'mtg)

;;==============================================;;

(defcustom mtg-preferred-formats-set

  '(vintage
   )

  "Set of preferred formats.

`listp' of `symbolp's

Customization:

• via checklist widget."

  :type '(set (const block)
              (const classic)
              (const commander)
              (const extended)
              (const legacy)
              (const modern)
              (const standard)
              (const vintage))

  :safe #'listp
  :group 'mtg)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg-types)

;;; mtg-types.el ends here