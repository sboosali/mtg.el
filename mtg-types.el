;;; mtg.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
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

;; Completion for writing custom MagicTheGathering cards.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase)
  (require 'cl-lib))

(progn
  (require 'json)
  (require 'seq))

;;----------------------------------------------;;

;; Internal:

(require 'mtg-data)

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

;; e.g. (mtg-card-create :name "" :cost "" :types "" :supertypes "" :subtypes "" :colors "" :oracle "" :power "" :toughness "" :loyalty "" :cmc 1 :coloridentity "" :image "" :flavor "" :frame "" :layout "" :rarity "" :typeline "" :language "" :artist "" :rulings "" :legality "" :scryfall "")

;;TODO color cmc supertypes subtypes layout watermark collector language

;; TODO legality  'legal 'banned' 'restricted 'illegal
;; TODO color     'white 'blue 'black 'red 'green
;; TODO language  'en ...



;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup mtg

    nil

  "."

  ;;:link (url-link "")

  )

;;----------------------------------------------;;

(defcustom mboo-mtg-color-list

  '(white blue black red green)

  "All MTG colors."

  :type '(repeat (symbol :tag "Color"))

  :safe #'listp
  :group 'mtg)
;;----------------------------------------------;;

(defcustom sboo-mtg-color-abbreviations-alist

  '(
    ( ?w . white)
    ( ?u . blue)
    ( ?b . black)
    ( ?r . red)
    ( ?g . green)
   )

  "Color abbreviations.

Associates characters with the colors they abbreviate."

  :type '(alist :key-type   (string :tag "Abbreviation")
                :value-type (string :tag "Color"))

  :safe #'listp
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-mtg-guild-list 

  '(azorius dimir rakdos gruul selesnya orzhov izzet golgari boros simic)

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

(defcustom mtg-known-card-types-list

  '(
    "Creature"
   )

  "Known card types."

  :type '(repeat (choice (const nil)
                           (string :tag "Card type")))

  :safe t
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-known-super-types-list

  '(
    "Legendary"
   )

  "Known super types."

  :type '(repeat (choice (const nil)
                           (string :tag "Supertype")))

  :safe t
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-known-sub-types-alist

  '(
    ""
   )

  "Known sub-types, by card-type."

  :type '(alist :key-type   (string :tag "Card type")
                :value-type (choice (const nil)
                                    (string :tag "Subtype")))

  :safe t
  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-max-length-of-card-name

  0

  "The maximum length among card names.

Used for vertically-aligning annotations (among other uses). 

(a natural.)"

  :type '(choice (const nil :tag "Unknown")
                 (integer :tag "Natural"))

  :safe #'integerp
  :group 'mtg)

;;----------------------------------------------;;

(defcustom sboo-mtg-preferred-formats-set

  '(
    vintage
   )

  "Set of formats. 

(Customize via checklist widget)."

  :type '(set (const nil)
              (const vintage)
              (const legacy)
              (const modern)
              )

  :safe #'listp
  :group 'sboo)




;;==============================================;;

blackBorder :: Border
blackBorder = "black"

whiteBorder :: Border
whiteBorder = "white"

silverBorder :: Border
silverBorder = "silver"






instantType :: Cardtype
instantType = "Instant"

sorceryType :: Cardtype
sorceryType = "Sorcery"

landType :: Cardtype
landType = "Land"

artifactType :: Cardtype
artifactType = "Artifact"

enchantmentType :: Cardtype
enchantmentType = "Enchantment"

creatureType :: Cardtype
creatureType = "Creature"

planeswalkerType :: Cardtype
planeswalkerType = "Planeswalker"

conspiracyType :: Cardtype
conspiracyType = "Conspiracy"
 



basicSupertype :: Supertype
basicSupertype = "Basic"

legendarySupertype :: Supertype
legendarySupertype = "Legendary"

snowSupertype ::Supertype
snowSupertype = "Snow"

ongoingSupertype :: Supertype
ongoingSupertype = "Ongoing"

worldSupertype :: Supertype
worldSupertype = "World"





standardFormat :: Format
standardFormat = "standard"

blockFormat :: Format
blockFormat = "block"

extendedFormat :: Format
extendedFormat = "extended"

vintageFormat :: Format
vintageFormat = "vintage"

classicFormat :: Format
classicFormat = "classic"

legacyFormat :: Format
legacyFormat = "legacy"

modernFormat :: Format
modernFormat = "modern"

commanderFormat :: Format
commanderFormat = "commander"








oldFrame :: Frame
oldFrame = "old"

timeshiftedFrame :: Frame
timeshiftedFrame = "timeshifted"

newFrame :: Frame
newFrame = "new"

futureFrame :: Frame
futureFrame = "future"









;;

whiteSymbol :: ManaSymbol
whiteSymbol = "W"

blueSymbol :: ManaSymbol
blueSymbol = "U"

blackSymbol :: ManaSymbol
blackSymbol = "B"

redSymbol :: ManaSymbol
redSymbol = "R"

greenSymbol :: ManaSymbol
greenSymbol = "G"

;;

colorlessSymbol :: ManaSymbol
colorlessSymbol = "C"

snowSymbol :: ManaSymbol
snowSymbol = "S"

energySymbol :: ManaSymbol
energySymbol = "E"

variableSymbol :: ManaSymbol
variableSymbol = "X"

;;

phyrexian :: ManaSymbol -> ManaSymbol
phyrexian (ManaSymbol s) = ManaSymbol ("{P" <> s <> "}")

phyrexianWhite :: ManaSymbol
phyrexianWhite = phyrexian whiteSymbol

phyrexianBlue :: ManaSymbol
phyrexianBlue = phyrexian blueSymbol

phyrexianBlack :: ManaSymbol
phyrexianBlack = phyrexian blackSymbol

phyrexianRed :: ManaSymbol
phyrexianRed = phyrexian redSymbol

phyrexianGreen :: ManaSymbol
phyrexianGreen = phyrexian greenSymbol

;;

monohybrid :: ManaSymbol -> ManaSymbol
monohybrid (ManaSymbol s) = ManaSymbol ("{2/" <> s <> "}")

monohybridWhite :: ManaSymbol
monohybridWhite = monohybrid whiteSymbol

monohybridBlue :: ManaSymbol
monohybridBlue = monohybrid blueSymbol

monohybridBlack :: ManaSymbol
monohybridBlack = monohybrid blackSymbol

monohybridRed :: ManaSymbol
monohybridRed = monohybrid redSymbol

monohybridGreen :: ManaSymbol
monohybridGreen = monohybrid greenSymbol

;;

genericSymbol :: Natural -> ManaSymbol
genericSymbol n = ManaSymbol $ show' n

zeroSymbol :: ManaSymbol
zeroSymbol = genericSymbol 0

oneSymbol :: ManaSymbol
oneSymbol = genericSymbol 1

twoSymbol :: ManaSymbol
twoSymbol = genericSymbol 2

threeSymbol :: ManaSymbol
threeSymbol = genericSymbol 3

fourSymbol :: ManaSymbol
fourSymbol = genericSymbol 4

fiveSymbol :: ManaSymbol
fiveSymbol = genericSymbol 5

sixSymbol :: ManaSymbol
sixSymbol = genericSymbol 6

sevenSymbol :: ManaSymbol
sevenSymbol = genericSymbol 7

eightSymbol :: ManaSymbol
eightSymbol = genericSymbol 8

nineSymbol :: ManaSymbol
nineSymbol = genericSymbol 9

tenSymbol :: ManaSymbol
tenSymbol = genericSymbol 10

elevenSymbol :: ManaSymbol
elevenSymbol = genericSymbol 11

twelveSymbol :: ManaSymbol
twelveSymbol = genericSymbol 12

thirteenSymbol :: ManaSymbol
thirteenSymbol = genericSymbol 13

fourteenSymbol :: ManaSymbol
fourteenSymbol = genericSymbol 14

fifteenSymbol :: ManaSymbol
fifteenSymbol = genericSymbol 15

sixteenSymbol :: ManaSymbol
sixteenSymbol = genericSymbol 16

seventeenSymbol :: ManaSymbol
seventeenSymbol = genericSymbol 17

eighteenSymbol :: ManaSymbol
eighteenSymbol = genericSymbol 18

nineteenSymbol :: ManaSymbol
nineteenSymbol = genericSymbol 19

twentySymbol :: ManaSymbol
twentySymbol = genericSymbol 20










_Deathtouch :: Keyword
_Deathtouch = "Deathtouch"

_Defender :: Keyword
_Defender = "Defender"

_DoubleStrike :: Keyword
_DoubleStrike = "Double Strike"

_Enchant :: Keyword
_Enchant = "Enchant"

_Equip :: Keyword
_Equip = "Equip"

_FirstStrike :: Keyword
_FirstStrike = "First Strike"

_Flash :: Keyword
_Flash = "Flash"

_Flying :: Keyword
_Flying = "Flying"

_Haste :: Keyword
_Haste = "Haste"

_Hexproof :: Keyword
_Hexproof = "Hexproof"

_Indestructible :: Keyword
_Indestructible = "Indestructible"

_Intimidate :: Keyword
_Intimidate = "Intimidate"

_Landwalk :: Keyword
_Landwalk = "Landwalk"

_Lifelink :: Keyword
_Lifelink = "Lifelink"

_Protection :: Keyword
_Protection = "Protection"

_Reach :: Keyword
_Reach = "Reach"

_Shroud :: Keyword
_Shroud = "Shroud"

_Trample :: Keyword
_Trample = "Trample"

_Vigilance :: Keyword
_Vigilance = "Vigilance"

_Banding :: Keyword
_Banding = "Banding"

_Rampage :: Keyword
_Rampage = "Rampage"

_CumulativeUpkeep :: Keyword
_CumulativeUpkeep = "Cumulative Upkeep"

_Flanking :: Keyword
_Flanking = "Flanking"

_Phasing :: Keyword
_Phasing = "Phasing"

_Buyback :: Keyword
_Buyback = "Buyback"

_Shadow :: Keyword
_Shadow = "Shadow"

_Cycling :: Keyword
_Cycling = "Cycling"

_Echo :: Keyword
_Echo = "Echo"

_Horsemanship :: Keyword
_Horsemanship = "Horsemanship"

_Fading :: Keyword
_Fading = "Fading"

_Kicker :: Keyword
_Kicker = "Kicker"

_Flashback :: Keyword
_Flashback = "Flashback"

_Madness :: Keyword
_Madness = "Madness"

_Fear :: Keyword
_Fear = "Fear"

_Morph :: Keyword
_Morph = "Morph"

_Amplify :: Keyword
_Amplify = "Amplify"

_Provoke :: Keyword
_Provoke = "Provoke"

_Storm :: Keyword
_Storm = "Storm"

_Affinity :: Keyword
_Affinity = "Affinity"

_Entwine :: Keyword
_Entwine = "Entwine"

_Modular :: Keyword
_Modular = "Modular"

_Sunburst :: Keyword
_Sunburst = "Sunburst"

_Bushido :: Keyword
_Bushido = "Bushido"

_Soulshift :: Keyword
_Soulshift = "Soulshift"

_Splice :: Keyword
_Splice = "Splice"

_Offering :: Keyword
_Offering = "Offering"

_Ninjutsu :: Keyword
_Ninjutsu = "Ninjutsu"

_Epic :: Keyword
_Epic = "Epic"

_Convoke :: Keyword
_Convoke = "Convoke"

_Dredge :: Keyword
_Dredge = "Dredge"

_Transmute :: Keyword
_Transmute = "Transmute"

_Bloodthirst :: Keyword
_Bloodthirst = "Bloodthirst"

_Haunt :: Keyword
_Haunt = "Haunt"

_Replicate :: Keyword
_Replicate = "Replicate"

_Forecast :: Keyword
_Forecast = "Forecast"

_Graft :: Keyword
_Graft = "Graft"

_Recover :: Keyword
_Recover = "Recover"

_Ripple :: Keyword
_Ripple = "Ripple"

_SplitSecond :: Keyword
_SplitSecond = "Split Second"

_Suspend :: Keyword
_Suspend = "Suspend"

_Vanishing :: Keyword
_Vanishing = "Vanishing"

_Absorb :: Keyword
_Absorb = "Absorb"

_AuraSwap :: Keyword
_AuraSwap = "Aura Swap"

_Delve :: Keyword
_Delve = "Delve"

_Fortify :: Keyword
_Fortify = "Fortify"

_Frenzy :: Keyword
_Frenzy = "Frenzy"

_Gravestorm :: Keyword
_Gravestorm = "Gravestorm"

_Poisonous :: Keyword
_Poisonous = "Poisonous"

_Transfigure :: Keyword
_Transfigure = "Transfigure"

_Champion :: Keyword
_Champion = "Champion"

_Changeling :: Keyword
_Changeling = "Changeling"

_Evoke :: Keyword
_Evoke = "Evoke"

_Hideaway :: Keyword
_Hideaway = "Hideaway"

_Prowl :: Keyword
_Prowl = "Prowl"

_Reinforce :: Keyword
_Reinforce = "Reinforce"

_Conspire :: Keyword
_Conspire = "Conspire"

_Persist :: Keyword
_Persist = "Persist"

_Wither :: Keyword
_Wither = "Wither"

_Retrace :: Keyword
_Retrace = "Retrace"

_Devour :: Keyword
_Devour = "Devour"

_Exalted :: Keyword
_Exalted = "Exalted"

_Unearth :: Keyword
_Unearth = "Unearth"

_Cascade :: Keyword
_Cascade = "Cascade"

_Annihilator :: Keyword
_Annihilator = "Annihilator"

_LevelUp :: Keyword
_LevelUp = "Level Up"

_Rebound :: Keyword
_Rebound = "Rebound"

_TotemArmor :: Keyword
_TotemArmor = "Totem Armor"

_Infect :: Keyword
_Infect = "Infect"

_BattleCry :: Keyword
_BattleCry = "Battle Cry"

_LivingWeapon :: Keyword
_LivingWeapon = "Living Weapon"

_Undying :: Keyword
_Undying = "Undying"

_Miracle :: Keyword
_Miracle = "Miracle"

_Soulbond :: Keyword
_Soulbond = "Soulbond"

_Overload :: Keyword
_Overload = "Overload"

_Scavenge :: Keyword
_Scavenge = "Scavenge"

_Unleash :: Keyword
_Unleash = "Unleash"

_Cipher :: Keyword
_Cipher = "Cipher"

_Evolve :: Keyword
_Evolve = "Evolve"

_Extort :: Keyword
_Extort = "Extort"

_Fuse :: Keyword
_Fuse = "Fuse"

_Bestow :: Keyword
_Bestow = "Bestow"

_Tribute :: Keyword
_Tribute = "Tribute"

_Dethrone :: Keyword
_Dethrone = "Dethrone"

_HiddenAgenda :: Keyword
_HiddenAgenda = "Hidden Agenda"

_Outlast :: Keyword
_Outlast = "Outlast"

_Prowess :: Keyword
_Prowess = "Prowess"

_Dash :: Keyword
_Dash = "Dash"

_Exploit :: Keyword
_Exploit = "Exploit"

_Menace :: Keyword
_Menace = "Menace"

_Renown :: Keyword
_Renown = "Renown"

_Awaken :: Keyword
_Awaken = "Awaken"

_Devoid :: Keyword
_Devoid = "Devoid"

_Ingest :: Keyword
_Ingest = "Ingest"

_Myriad :: Keyword
_Myriad = "Myriad"

_Surge :: Keyword
_Surge = "Surge"

_Skulk :: Keyword
_Skulk = "Skulk"

_Emerge :: Keyword
_Emerge = "Emerge"

_Escalate :: Keyword
_Escalate = "Escalate"

_Melee :: Keyword
_Melee = "Melee"

_Crew :: Keyword
_Crew = "Crew"

_Fabricate :: Keyword
_Fabricate = "Fabricate"

_Partner :: Keyword
_Partner = "Partner"

_Undaunted :: Keyword
_Undaunted = "Undaunted"

_Improvise :: Keyword
_Improvise = "Improvise"

_Aftermath :: Keyword
_Aftermath = "Aftermath"

_Embalm :: Keyword
_Embalm = "Embalm"

_Eternalize :: Keyword
_Eternalize = "Eternalize"

_Afflict :: Keyword
_Afflict = "Afflict"

_Ascend :: Keyword
_Ascend = "Ascend"











;;

english :: Language
english = "English"

german :: Language
german = "German"

french :: Language
french = "French"

italian :: Language
italian = "Italian"

spanish :: Language
spanish = "Spanish"

portuguese :: Language
portuguese = "Portuguese"

japanese :: Language
japanese = "Japanese"

chinese :: Language
chinese = "Chinese"

russian :: Language
russian = "Russian"

taiwanese :: Language
taiwanese = "Taiwanese"

korean :: Language
korean = "Korean"

;;

englishAbbreviation :: Text
englishAbbreviation = "en"

germanAbbreviation :: Text
germanAbbreviation = "de"

frenchAbbreviation :: Text
frenchAbbreviation = "fr"

italianAbbreviation :: Text
italianAbbreviation = "it"

spanishAbbreviation :: Text
spanishAbbreviation = "es"

portugueseAbbreviation :: Text
portugueseAbbreviation = "pt"

japaneseAbbreviation :: Text
japaneseAbbreviation = "jp"

chineseAbbreviation :: Text
chineseAbbreviation = "cn"

russianAbbreviation :: Text
russianAbbreviation = "ru"

taiwaneseAbbreviation :: Text
taiwaneseAbbreviation = "tw"

koreanAbbreviation :: Text
koreanAbbreviation = "ko"

;;

englishEndonym :: Text
englishEndonym = "English"

germanEndonym :: Text
germanEndonym = "Deutsch"

frenchEndonym :: Text
frenchEndonym = "Français"

italianEndonym :: Text
italianEndonym = "Italiano"

spanishEndonym :: Text
spanishEndonym = "Español"

portugueseEndonym :: Text
portugueseEndonym = "Português"

japaneseEndonym :: Text
japaneseEndonym = "日本語"

chineseEndonym :: Text
chineseEndonym = "简体中文"

russianEndonym :: Text
russianEndonym = "Русский"

taiwaneseEndonym :: Text
taiwaneseEndonym = "繁體中文"

koreanEndonym :: Text
koreanEndonym = "한국어"














{-|
-}
aftermathLayout :: Layout
aftermathLayout = "aftermath"

{-|
-}
doubleFacedLayout :: Layout
doubleFacedLayout = "double-faced"

{-|
-}
flipLayout :: Layout
flipLayout = "flip"

{-|
-}
levelerLayout :: Layout
levelerLayout = "leveler"

{-|
-}
meldLayout :: Layout
meldLayout = "meld"

{-|
-}
normalLayout :: Layout
normalLayout = "normal"

{-|
-}
planeLayout :: Layout
planeLayout = "plane"

{-|
-}
phenomenonLayout :: Layout
phenomenonLayout = "phenomenon"

{-|
-}
schemeLayout :: Layout
schemeLayout = "scheme"

{-|
-}
splitLayout :: Layout
splitLayout = "split"

{-|
-}
tokenLayout :: Layout
tokenLayout = "token"

{-|
-}
vanguardLayout :: Layout
vanguardLayout = "vanguard"












mythic :: Rarity
mythic = "Mythic"

rare :: Rarity
rare = "Rare"

uncommon :: Rarity
uncommon = "Uncommon"

common :: Rarity
common = "Common"

;;

mythicAbbreviation :: Char
mythicAbbreviation = 'M'

rareAbbreviation :: Char
rareAbbreviation = 'R'

uncommonAbbreviation :: Char
uncommonAbbreviation = 'U'

commonAbbreviation :: Char
commonAbbreviation = 'C'

;;


tapSymbol :: Symbol
tapSymbol = "T"

untapSymbol :: Symbol
untapSymbol = "Q"























mirageBlock :: BlockName
mirageBlock = "Mirage"

rathBlock :: BlockName
rathBlock = "Rath"

urzaBlock :: BlockName
urzaBlock = "Urza"

masquesBlock :: BlockName
masquesBlock = "Masques"

invasionBlock :: BlockName
invasionBlock = "Invasion"

odysseyBlock :: BlockName
odysseyBlock = "Odyssey"

onslaughtBlock :: BlockName
onslaughtBlock = "Onslaught"

mirrodinBlock :: BlockName
mirrodinBlock = "Mirrodin"

kamigawaBlock :: BlockName
kamigawaBlock = "Kamigawa"

ravnicaBlock :: BlockName
ravnicaBlock = "Ravnica"

iceageBlock :: BlockName
iceageBlock = "Ice Age"

timespiralBlock :: BlockName
timespiralBlock = "Time Spiral"

lorwynBlock :: BlockName
lorwynBlock = "Lorwyn"

shadowmoorBlock :: BlockName
shadowmoorBlock = "Shadowmoor"

alaraBlock :: BlockName
alaraBlock = "Shards Of Alara"

zendikarBlock :: BlockName
zendikarBlock = "Zendikar"

scarsBlock :: BlockName
scarsBlock = "Scars Of Mirrodin"

innistradBlock :: BlockName
innistradBlock = "Innistrad"

ravnicaReturnBlock :: BlockName
ravnicaReturnBlock = "Return To Ravnica"

therosBlock :: BlockName
therosBlock = "Theros"

khansBlock :: BlockName
khansBlock = "Khans Of Tarkir"

zendikarReturnBlock :: BlockName
zendikarReturnBlock = "Battle For Zendikar"

shadowsBlock :: BlockName
shadowsBlock = "Shadows Over Innistrad"

kaladeshBlock :: BlockName
kaladeshBlock = "Kaladesh"

amonkhetBlock :: BlockName
amonkhetBlock = "Amonkhet"

ixalanBlock :: BlockName
ixalanBlock = "Ixalan"

;;

antediluvianPseudoBlock :: BlockName
antediluvianPseudoBlock = "Antediluvian Sets"

ordinalPseudoBlock :: BlockName
ordinalPseudoBlock = "Ordinal Core Sets"

cardinalPseudoBlock :: BlockName
cardinalPseudoBlock = "Cardinal Core Sets"

;;

coreEdition :: EditionType
coreEdition = "core"

expansionEdition :: EditionType
expansionEdition = "expansion"

reprintEdition :: EditionType
reprintEdition = "reprint"

boxEdition :: EditionType
boxEdition = "box"

unEdition :: EditionType
unEdition = "un"

vaultEdition :: EditionType
vaultEdition = "from the vault"

premiumEdition :: EditionType
premiumEdition = "premium deck"

duelEdition :: EditionType
duelEdition = "duel deck"

starterEdition :: EditionType
starterEdition = "starter"

commanderEdition :: EditionType
commanderEdition = "commander"

planechaseEdition :: EditionType
planechaseEdition = "planechase"

archenemyEdition :: EditionType
archenemyEdition = "archenemy"

promoEdition :: EditionType
promoEdition = "promo"

vanguardEdition :: EditionType
vanguardEdition = "vanguard"

mastersEdition :: EditionType
mastersEdition = "masters"

conspiracyEdition :: EditionType
conspiracyEdition = "conspiracy"

masterpieceEdition :: EditionType
masterpieceEdition = "masterpiece"

;;

eAL :: EditionCode
eAL = "AL"

eBE :: EditionCode
eBE = "BE"

eUN :: EditionCode
eUN = "UN"

eRV :: EditionCode
eRV = "RV"

eSUMMER :: EditionCode
eSUMMER = "SUMMER"

eE4 :: EditionCode
eE4 = "E4"

eE5 :: EditionCode
eE5 = "E5"

eE6 :: EditionCode
eE6 = "E6"

eE7 :: EditionCode
eE7 = "E7"

eE8 :: EditionCode
eE8 = "E8"

eE9 :: EditionCode
eE9 = "E9"

eE10 :: EditionCode
eE10 = "E10"

eM10 :: EditionCode
eM10 = "M10"

eM11 :: EditionCode
eM11 = "M11"

eM12 :: EditionCode
eM12 = "M12"

eM13 :: EditionCode
eM13 = "M13"

eM14 :: EditionCode
eM14 = "M14"

eM15 :: EditionCode
eM15 = "M15"

eORI :: EditionCode
eORI = "ORI"

eAN :: EditionCode
eAN = "AN"

eAQ :: EditionCode
eAQ = "AQ"

eLG :: EditionCode
eLG = "LG"

eDK :: EditionCode
eDK = "DK"

eFE :: EditionCode
eFE = "FE"

eHL :: EditionCode
eHL = "HL"

eMR :: EditionCode
eMR = "MR"

eVI :: EditionCode
eVI = "VI"

eWL :: EditionCode
eWL = "WL"

eTP :: EditionCode
eTP = "TP"

eSH :: EditionCode
eSH = "SH"

eEX :: EditionCode
eEX = "EX"

eUS :: EditionCode
eUS = "US"

eUL :: EditionCode
eUL = "UL"

eUD :: EditionCode
eUD = "UD"

eMM :: EditionCode
eMM = "MM"

eNE :: EditionCode
eNE = "NE"

ePR :: EditionCode
ePR = "PR"

eIN :: EditionCode
eIN = "IN"

ePS :: EditionCode
ePS = "PS"

eAP :: EditionCode
eAP = "AP"

eOD :: EditionCode
eOD = "OD"

eTR :: EditionCode
eTR = "TR"

eJU :: EditionCode
eJU = "JU"

eON :: EditionCode
eON = "ON"

eLE :: EditionCode
eLE = "LE"

eSC :: EditionCode
eSC = "SC"

eMI :: EditionCode
eMI = "MI"

eDS :: EditionCode
eDS = "DS"

eDN5 :: EditionCode
eDN5 = "DN5"

eCHK :: EditionCode
eCHK = "CHK"

eBOK :: EditionCode
eBOK = "BOK"

eSOK :: EditionCode
eSOK = "SOK"

eRAV :: EditionCode
eRAV = "RAV"

eGP :: EditionCode
eGP = "GP"

eDI :: EditionCode
eDI = "DI"

eIA :: EditionCode
eIA = "IA"

eAI :: EditionCode
eAI = "AI"

eCS :: EditionCode
eCS = "CS"

eTSTS :: EditionCode
eTSTS = "TSTS"

eTS :: EditionCode
eTS = "TS"

ePC :: EditionCode
ePC = "PC"

eFUT :: EditionCode
eFUT = "FUT"

eLW :: EditionCode
eLW = "LW"

eMT :: EditionCode
eMT = "MT"

eSHM :: EditionCode
eSHM = "SHM"

eEVE :: EditionCode
eEVE = "EVE"

eALA :: EditionCode
eALA = "ALA"

eCFX :: EditionCode
eCFX = "CFX"

eARB :: EditionCode
eARB = "ARB"

eZEN :: EditionCode
eZEN = "ZEN"

eWWK :: EditionCode
eWWK = "WWK"

eROE :: EditionCode
eROE = "ROE"

eSOM :: EditionCode
eSOM = "SOM"

eMBS :: EditionCode
eMBS = "MBS"

eNPH :: EditionCode
eNPH = "NPH"

eISD :: EditionCode
eISD = "ISD"

eDKA :: EditionCode
eDKA = "DKA"

eAVR :: EditionCode
eAVR = "AVR"

eRTR :: EditionCode
eRTR = "RTR"

eGTC :: EditionCode
eGTC = "GTC"

eDGM :: EditionCode
eDGM = "DGM"

eTHS :: EditionCode
eTHS = "THS"

eBNG :: EditionCode
eBNG = "BNG"

eJOU :: EditionCode
eJOU = "JOU"

eKTK :: EditionCode
eKTK = "KTK"

eFRF :: EditionCode
eFRF = "FRF"

eDTK :: EditionCode
eDTK = "DTK"

eBFZ :: EditionCode
eBFZ = "BFZ"

eOGW :: EditionCode
eOGW = "OGW"

eSOI :: EditionCode
eSOI = "SOI"

eEMN :: EditionCode
eEMN = "EMN"

eKLD :: EditionCode
eKLD = "KLD"

eAER :: EditionCode
eAER = "AER"

eAKH :: EditionCode
eAKH = "AKH"

eHOU :: EditionCode
eHOU = "HOU"

eXLN :: EditionCode
eXLN = "XLN"

eRIX :: EditionCode
eRIX = "RIX"

;;




colorWatermarks :: Set Watermark
colorWatermarks =
  [ whiteWatermark
  , blueWatermark
  , blackWatermark
  , redWatermark
  , greenWatermark
  ]

guildWatermarks :: Set Watermark
guildWatermarks =
  [ azoriusWatermark
  , dimirWatermark
  , rakdosWatermark
  , gruulWatermark
  , selesnyaWatermark
  , orzhovWatermark
  , izzetWatermark
  , golgariWatermark
  , borosWatermark
  , simicWatermark
  ]

wedgeWatermarks :: Set Watermark
wedgeWatermarks =
  [ marduWatermark
  , temurWatermark
  , abzanWatermark
  , jeskaiWatermark
  , sultaiWatermark
  ]

dragonlordWatermarks :: Set Watermark
dragonlordWatermarks =
  [ ojutaiWatermark
  , silumgarWatermark
  , kolaghanWatermark
  , atarkaWatermark
  , dromokaWatermark
  ]

beseigedWatermarks :: Set Watermark
beseigedWatermarks =
  [ mirranWatermark
  , phyrexianWatermark
  ]

unstableWatermarks :: Set Watermark
unstableWatermarks =
  [ agentsOfSNEAKWatermark
  , crossbreedLabsWatermark
  , goblinExplosioneersWatermark
  , leagueOfDastardlyDoomWatermark
  ]

miscellaneousWatermarks :: Set Watermark
miscellaneousWatermarks =
  [ planeswalkerWatermark
  , colorlessWatermark
  ]

;;

abzanWatermark :: Watermark
abzanWatermark = "Abzan"

agentsOfSNEAKWatermark :: Watermark
agentsOfSNEAKWatermark = "Agents of S.N.E.A.K."

atarkaWatermark :: Watermark
atarkaWatermark = "Atarka"

azoriusWatermark :: Watermark
azoriusWatermark = "Azorius"

blackWatermark :: Watermark
blackWatermark = "Black"

blueWatermark :: Watermark
blueWatermark = "Blue"

borosWatermark :: Watermark
borosWatermark = "Boros"

colorlessWatermark :: Watermark
colorlessWatermark = "Colorless"

crossbreedLabsWatermark :: Watermark
crossbreedLabsWatermark = "Crossbreed Labs"

dimirWatermark :: Watermark
dimirWatermark = "Dimir"

dromokaWatermark :: Watermark
dromokaWatermark = "Dromoka"

goblinExplosioneersWatermark :: Watermark
goblinExplosioneersWatermark = "Goblin Explosioneers"

golgariWatermark :: Watermark
golgariWatermark = "Golgari"

greenWatermark :: Watermark
greenWatermark = "Green"

gruulWatermark :: Watermark
gruulWatermark = "Gruul"

izzetWatermark :: Watermark
izzetWatermark = "Izzet"

jeskaiWatermark :: Watermark
jeskaiWatermark = "Jeskai"

kolaghanWatermark :: Watermark
kolaghanWatermark = "Kolaghan"

leagueOfDastardlyDoomWatermark :: Watermark
leagueOfDastardlyDoomWatermark = "League of Dastardly Doom"

marduWatermark :: Watermark
marduWatermark = "Mardu"

mirranWatermark :: Watermark
mirranWatermark = "Mirran"

ojutaiWatermark :: Watermark
ojutaiWatermark = "Ojutai"

orderOfTheWidgetWatermark :: Watermark
orderOfTheWidgetWatermark = "Order of the Widget"

orzhovWatermark :: Watermark
orzhovWatermark = "Orzhov"

phyrexianWatermark :: Watermark
phyrexianWatermark = "Phyrexian"

planeswalkerWatermark :: Watermark
planeswalkerWatermark = "Planeswalker"

rakdosWatermark :: Watermark
rakdosWatermark = "Rakdos"

redWatermark :: Watermark
redWatermark = "Red"

selesnyaWatermark :: Watermark
selesnyaWatermark = "Selesnya"

silumgarWatermark :: Watermark
silumgarWatermark = "Silumgar"

simicWatermark :: Watermark
simicWatermark = "Simic"

sultaiWatermark :: Watermark
sultaiWatermark = "Sultai"

temurWatermark :: Watermark
temurWatermark = "Temur"

whiteWatermark :: Watermark
whiteWatermark = "White"

;;




















;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun make-mtg-card (&key name cost types supertypes subtypes colors oracle power toughness loyalty cmc coloridentity image flavor frame layout rarity typeline language artist date rulings legality scryfall)

  "Make an `mtg-card'. Smart constructor with validation plus defaulting.

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

(cl-defun mtg-parse-json-cards (&key json-file json-string)

  "Parse a JSON-STRING or JSON-FILE with schema `mtg-card's.

Inputs:

• JSON-FILE — a « .json » file.
• JSON-STRING — a json string.

Output:

• a `hash-table-p' of `mtg-card-p's (indexed by `mtg-card-name').

Example:

• M-: (mtg-parse-json-cards :json-string \"[{\\n  \\\"object\\\": \\\"card\\\",\\n  \\\"id\\\": \\\"46b0a5c2-ac85-448e-9e87-12fc74fd4147\\\",\\n  \\\"oracle_id\\\": \\\"550c74d4-1fcb-406a-b02a-639a760a4380\\\",\\n  \\\"multiverse_ids\\\": [\\n    390\\n  ],\\n  \\\"tcgplayer_id\\\": 8671,\\n  \\\"name\\\": \\\"Ancestral Recall\\\",\\n  \\\"lang\\\": \\\"en\\\",\\n  \\\"released_at\\\": \\\"1993-10-04\\\",\\n  \\\"uri\\\": \\\"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147\\\",\\n  \\\"scryfall_uri\\\": \\\"https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api\\\",\\n  \\\"layout\\\": \\\"normal\\\",\\n  \\\"highres_image\\\": true,\\n  \\\"image_uris\\\": {\\n    \\\"small\\\": \\\"https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970\\\",\\n    \\\"normal\\\": \\\"https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970\\\",\\n    \\\"large\\\": \\\"https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970\\\",\\n    \\\"png\\\": \\\"https://img.scryfall.com/cards/png/en/leb/48.png?1525122970\\\",\\n    \\\"art_crop\\\": \\\"https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970\\\",\\n    \\\"border_crop\\\": \\\"https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970\\\"\\n  },\\n  \\\"mana_cost\\\": \\\"{U}\\\",\\n  \\\"cmc\\\": 1.0,\\n  \\\"type_line\\\": \\\"Instant\\\",\\n  \\\"oracle_text\\\": \\\"Target player draws three cards.\\\",\\n  \\\"colors\\\": [\\n    \\\"U\\\"\\n  ],\\n  \\\"color_identity\\\": [\\n    \\\"U\\\"\\n  ],\\n  \\\"legalities\\\": {\\n    \\\"standard\\\": \\\"not_legal\\\",\\n    \\\"future\\\": \\\"not_legal\\\",\\n    \\\"frontier\\\": \\\"not_legal\\\",\\n    \\\"modern\\\": \\\"not_legal\\\",\\n    \\\"legacy\\\": \\\"banned\\\",\\n    \\\"pauper\\\": \\\"not_legal\\\",\\n    \\\"vintage\\\": \\\"restricted\\\",\\n    \\\"penny\\\": \\\"not_legal\\\",\\n    \\\"commander\\\": \\\"banned\\\",\\n    \\\"duel\\\": \\\"banned\\\",\\n    \\\"oldschool\\\": \\\"restricted\\\"\\n  },\\n  \\\"games\\\": [\\n    \\\"paper\\\"\\n  ],\\n  \\\"reserved\\\": true,\\n  \\\"foil\\\": false,\\n  \\\"nonfoil\\\": true,\\n  \\\"oversized\\\": false,\\n  \\\"promo\\\": false,\\n  \\\"reprint\\\": true,\\n  \\\"set\\\": \\\"leb\\\",\\n  \\\"set_name\\\": \\\"Limited Edition Beta\\\",\\n  \\\"set_uri\\\": \\\"https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302\\\",\\n  \\\"set_search_uri\\\": \\\"https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints\\\",\\n  \\\"scryfall_set_uri\\\": \\\"https://scryfall.com/sets/leb?utm_source=api\\\",\\n  \\\"rulings_uri\\\": \\\"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings\\\",\\n  \\\"prints_search_uri\\\": \\\"https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints\\\",\\n  \\\"collector_number\\\": \\\"48\\\",\\n  \\\"digital\\\": false,\\n  \\\"rarity\\\": \\\"rare\\\",\\n  \\\"illustration_id\\\": \\\"d20eda7b-a902-4c00-bdab-601059e417b5\\\",\\n  \\\"artist\\\": \\\"Mark Poole\\\",\\n  \\\"border_color\\\": \\\"black\\\",\\n  \\\"frame\\\": \\\"1993\\\",\\n  \\\"frame_effect\\\": \\\"\\\",\\n  \\\"full_art\\\": false,\\n  \\\"story_spotlight\\\": false,\\n  \\\"edhrec_rank\\\": 16999,\\n  \\\"related_uris\\\": {\\n    \\\"gatherer\\\": \\\"http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390\\\",\\n    \\\"tcgplayer_decks\\\": \\\"https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall\\\",\\n    \\\"edhrec\\\": \\\"http://edhrec.com/route/?cc=Ancestral+Recall\\\",\\n    \\\"mtgtop8\\\": \\\"http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall\\\"\\n  }\\n}]\"
    ⇒ 

Links:

• URL `'

Related:

• `'"

  (let* ((json-array-type  'vector)
         (json-object-type 'plist)
         (json-false        nil)
         (json-null         nil)
       ;;(json-key-type    'keyword)
         )

    (let* ((ARRAY (cond

                     ((stringp json-string)

                      (json-read-from-string json-string)
                      )

                     ((and (stringp json-file) (file-exists-p json-file))

                      (read json-file)
                      )

                     (t (throw 'mtg-parse-json-cards-error ))))

             (SIZE (length ARRAY))

             (TABLE (make-hash-table :test #'equal :size SIZE))

             )
    (progn

      (dolist (PLIST ARRAY)
        (let* ((CARD (mtg-card-from-scryfall-plist PLIST))
               (NAME (mtg-card-name       CARD))
               )
          (puthash NAME CARD TABLE)))

      TABLE))))

;; M-: (mtg-parse-json-cards :json-file   "./scryfall-default-cards.json")
;; M-: (mtg-parse-json-cards :json-string "[{\n  \"object\": \"card\",\n  \"id\": \"46b0a5c2-ac85-448e-9e87-12fc74fd4147\",\n  \"oracle_id\": \"550c74d4-1fcb-406a-b02a-639a760a4380\",\n  \"multiverse_ids\": [\n    390\n  ],\n  \"tcgplayer_id\": 8671,\n  \"name\": \"Ancestral Recall\",\n  \"lang\": \"en\",\n  \"released_at\": \"1993-10-04\",\n  \"uri\": \"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147\",\n  \"scryfall_uri\": \"https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api\",\n  \"layout\": \"normal\",\n  \"highres_image\": true,\n  \"image_uris\": {\n    \"small\": \"https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970\",\n    \"normal\": \"https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970\",\n    \"large\": \"https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970\",\n    \"png\": \"https://img.scryfall.com/cards/png/en/leb/48.png?1525122970\",\n    \"art_crop\": \"https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970\",\n    \"border_crop\": \"https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970\"\n  },\n  \"mana_cost\": \"{U}\",\n  \"cmc\": 1.0,\n  \"type_line\": \"Instant\",\n  \"oracle_text\": \"Target player draws three cards.\",\n  \"colors\": [\n    \"U\"\n  ],\n  \"color_identity\": [\n    \"U\"\n  ],\n  \"legalities\": {\n    \"standard\": \"not_legal\",\n    \"future\": \"not_legal\",\n    \"frontier\": \"not_legal\",\n    \"modern\": \"not_legal\",\n    \"legacy\": \"banned\",\n    \"pauper\": \"not_legal\",\n    \"vintage\": \"restricted\",\n    \"penny\": \"not_legal\",\n    \"commander\": \"banned\",\n    \"duel\": \"banned\",\n    \"oldschool\": \"restricted\"\n  },\n  \"games\": [\n    \"paper\"\n  ],\n  \"reserved\": true,\n  \"foil\": false,\n  \"nonfoil\": true,\n  \"oversized\": false,\n  \"promo\": false,\n  \"reprint\": true,\n  \"set\": \"leb\",\n  \"set_name\": \"Limited Edition Beta\",\n  \"set_uri\": \"https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302\",\n  \"set_search_uri\": \"https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints\",\n  \"scryfall_set_uri\": \"https://scryfall.com/sets/leb?utm_source=api\",\n  \"rulings_uri\": \"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings\",\n  \"prints_search_uri\": \"https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints\",\n  \"collector_number\": \"48\",\n  \"digital\": false,\n  \"rarity\": \"rare\",\n  \"illustration_id\": \"d20eda7b-a902-4c00-bdab-601059e417b5\",\n  \"artist\": \"Mark Poole\",\n  \"border_color\": \"black\",\n  \"frame\": \"1993\",\n  \"frame_effect\": \"\",\n  \"full_art\": false,\n  \"story_spotlight\": false,\n  \"edhrec_rank\": 16999,\n  \"related_uris\": {\n    \"gatherer\": \"http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390\",\n    \"tcgplayer_decks\": \"https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall\",\n    \"edhrec\": \"http://edhrec.com/route/?cc=Ancestral+Recall\",\n    \"mtgtop8\": \"http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall\"\n  }\n}]")

;;----------------------------------------------;;

(defun mtg-card-from-scryfall-plist (properties)

  "Parse Scryfall PROPERTIES into an MTG Card.

Inputs:

• PROPERTIES — a `plist'.

Output:

• an `mtg-card-p'.

Example:

• M-: (mtg-card-from-scryfall-plist '(:object \"card\" :id \"46b0a5c2-ac85-448e-9e87-12fc74fd4147\" :oracle_id \"550c74d4-1fcb-406a-b02a-639a760a4380\" :multiverse_ids [390] :tcgplayer_id 8671 :name \"Ancestral Recall\" :lang \"en\" :released_at \"1993-10-04\" :uri \"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147\" :scryfall_uri \"https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api\" :layout \"normal\" :highres_image t :mana_cost \"{U}\" :cmc 1.0 :type_line \"Instant\" :oracle_text \"Target player draws three cards.\" :colors [\"U\"] :color_identity [\"U\"] :games [\"paper\"] :reserved t :foil nil :nonfoil t :oversized nil :promo nil :reprint t :set \"leb\" :set_name \"Limited Edition Beta\" :set_uri \"https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302\" :set_search_uri \"https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints\" :scryfall_set_uri \"https://scryfall.com/sets/leb?utm_source=api\" :rulings_uri \"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings\" :prints_search_uri \"https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints\" :collector_number \"48\" :digital nil :rarity \"rare\" :illustration_id \"d20eda7b-a902-4c00-bdab-601059e417b5\" :artist \"Mark Poole\" :border_color \"black\" :frame \"1993\" :frame_effect \"\" :full_art nil :story_spotlight nil :edhrec_rank 16999 :legalities (:standard \"not_legal\" :future \"not_legal\" :frontier \"not_legal\" :modern \"not_legal\" :legacy \"banned\" :pauper \"not_legal\" :vintage \"restricted\" :penny \"not_legal\" :commander \"banned\" :duel \"banned\" :oldschool \"restricted\") :image_uris (:small \"https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970\" :normal \"https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970\" :large \"https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970\" :png \"https://img.scryfall.com/cards/png/en/leb/48.png?1525122970\" :art_crop \"https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970\" :border_crop \"https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970\") :related_uris (:gatherer \"http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390\" :tcgplayer_decks \"https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall\" :edhrec \"http://edhrec.com/route/?cc=Ancestral+Recall\" :mtgtop8 \"http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall\")))
    ⇒ 

Related:

• `make-mtg-card'"

  ;; 

  (let* ((NAME          (plist-get properties :name))
         (COST          (plist-get properties :mana_cost))
         (COLORS        (plist-get properties :colors))
         (ORACLE        (plist-get properties :oracle_text))
         (POWER         (plist-get properties :power))
         (TOUGHNESS     (plist-get properties :toughness))
         (LOYALTY       (plist-get properties :loyalty))
         (CMC           (plist-get properties :cmc))
         (COLORIDENTITY (plist-get properties :color_identity))
         (FLAVOR        (plist-get properties :flavor_text))
         (FRAME         (plist-get properties :frame))
         (BORDER        (plist-get properties :border_color))
         (LAYOUT        (plist-get properties :layout))
         (RARITY        (plist-get properties :rarity))
         (SET           (plist-get properties :set))
         (TYPELINE      (plist-get properties :type_line))
         (LANGUAGE      (plist-get properties :lang))
         (ARTIST        (plist-get properties :artist))
         (DATE          (plist-get properties :released_at))
         (IDENTIFIERS   (plist-get properties :multiverse_ids))
         (RULINGS       (plist-get properties :rulings))
         (LEGALITY      (plist-get properties :legalities))
         (SCRYFALL      (plist-get properties :scryfall_uri))

         (TYPE-PLIST    (mtg-parse-typeline TYPELINE))
         (TYPES         (plist-get TYPE-PLIST :types))
         (SUPERTYPES    (plist-get TYPE-PLIST :supertypes))
         (SUBTYPES      (plist-get TYPE-PLIST :subtypes))

         (IMAGE         (plist-get (plist-get properties :image_uris) :png))
         )

    (make-mtg-card :name NAME :cost COST :types TYPES :supertypes SUPERTYPES :subtypes SUBTYPES :colors COLORS :oracle ORACLE :power POWER :toughness TOUGHNESS :loyalty LOYALTY :cmc CMC :coloridentity COLORIDENTITY :image IMAGE :flavor FLAVOR :frame FRAME :layout LAYOUT :border BORDER :rarity RARITY :set SET :typeline TYPELINE :language LANGUAGE :artist ARTIST :date DATE :identifiers IDENTIFIERS :rulings RULINGS :legality LEGALITY :scryfall SCRYFALL)))

;; M-: (mtg-card-from-scryfall-plist '(:object "card" :id "46b0a5c2-ac85-448e-9e87-12fc74fd4147" :oracle_id "550c74d4-1fcb-406a-b02a-639a760a4380" :multiverse_ids [390] :tcgplayer_id 8671 :name "Ancestral Recall" :lang "en" :released_at "1993-10-04" :uri "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147" :scryfall_uri "https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api" :layout "normal" :highres_image t :mana_cost "{U}" :cmc 1.0 :type_line "Instant" :oracle_text "Target player draws three cards." :colors ["U"] :color_identity ["U"] :games ["paper"] :reserved t :foil nil :nonfoil t :oversized nil :promo nil :reprint t :set "leb" :set_name "Limited Edition Beta" :set_uri "https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302" :set_search_uri "https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints" :scryfall_set_uri "https://scryfall.com/sets/leb?utm_source=api" :rulings_uri "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings" :prints_search_uri "https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints" :collector_number "48" :digital nil :rarity "rare" :illustration_id "d20eda7b-a902-4c00-bdab-601059e417b5" :artist "Mark Poole" :border_color "black" :frame "1993" :frame_effect "" :full_art nil :story_spotlight nil :edhrec_rank 16999 :legalities (:standard "not_legal" :future "not_legal" :frontier "not_legal" :modern "not_legal" :legacy "banned" :pauper "not_legal" :vintage "restricted" :penny "not_legal" :commander "banned" :duel "banned" :oldschool "restricted") :image_uris (:small "https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970" :normal "https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970" :large "https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970" :png "https://img.scryfall.com/cards/png/en/leb/48.png?1525122970" :art_crop "https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970" :border_crop "https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970") :related_uris (:gatherer "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390" :tcgplayer_decks "https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall" :edhrec "http://edhrec.com/route/?cc=Ancestral+Recall" :mtgtop8 "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall")))

;; NOTES
;;
;; M-: (plist-get '(:object "card" :id "46b0a5c2-ac85-448e-9e87-12fc74fd4147" :oracle_id "550c74d4-1fcb-406a-b02a-639a760a4380" :multiverse_ids [390] :tcgplayer_id 8671 :name "Ancestral Recall" :lang "en" :released_at "1993-10-04" :uri "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147" :scryfall_uri "https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api" :layout "normal" :highres_image t :mana_cost "{U}" :cmc 1.0 :type_line "Instant" :oracle_text "Target player draws three cards." :colors ["U"] :color_identity ["U"] :games ["paper"] :reserved t :foil nil :nonfoil t :oversized nil :promo nil :reprint t :set "leb" :set_name "Limited Edition Beta" :set_uri "https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302" :set_search_uri "https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints" :scryfall_set_uri "https://scryfall.com/sets/leb?utm_source=api" :rulings_uri "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings" :prints_search_uri "https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints" :collector_number "48" :digital nil :rarity "rare" :illustration_id "d20eda7b-a902-4c00-bdab-601059e417b5" :artist "Mark Poole" :border_color "black" :frame "1993" :frame_effect "" :full_art nil :story_spotlight nil :edhrec_rank 16999 :legalities (:standard "not_legal" :future "not_legal" :frontier "not_legal" :modern "not_legal" :legacy "banned" :pauper "not_legal" :vintage "restricted" :penny "not_legal" :commander "banned" :duel "banned" :oldschool "restricted") :image_uris (:small "https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970" :normal "https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970" :large "https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970" :png "https://img.scryfall.com/cards/png/en/leb/48.png?1525122970" :art_crop "https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970" :border_crop "https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970") :related_uris (:gatherer "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390" :tcgplayer_decks "https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall" :edhrec "http://edhrec.com/route/?cc=Ancestral+Recall" :mtgtop8 "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall")) :name)
;;    ⇒ "Ancestral Recall"

;; TODO
;;
;; (:gatherer "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390")
;; (format "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s" (car (mtg-card-identifiers CARD)))

;; TODO
;;
;; {"type_line":"Creature \342\200\224 Horror"}
;;
;; M-: (mtg-parse-typeline "Legendary Creature — Elf Druid")
;;     '(:supertypes ("Legendary") :types ("Creature") :subtypes ("Elf" "Druid"))
;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun mtg-insert-card (card)

  "Insert an MTG card name, from `mtg-card-names'."

  (interactive (list (mtg-read-card-name)))

  (insert card))                        ;TODO different styles, versions (old, new), name-only or full-text or image, surround with backticks, etc. 

;;----------------------------------------------;;

(cl-defun sboo-mtg-read-color (&key prompt)

  "Read a color via `sboo-mtg-colors-list'."

  (interactive)

  (let ((PROMPT        (or prompt "Color: "))
        (COLLECTION    sboo-mtg-colors-list)
        (REQUIRE-MATCH t)
        )

    (read PROMPT CHOLLECTION nil REQUIRE-MATCH)))

;;----------------------------------------------;;

(cl-defun sboo-mtg-read-color-char (&key prompt)

  "Read a color via `sboo-mtg-color-abbreviations-alist'."

  (interactive)

  (let ((PROMPT  (or prompt "Color: "))
        (CHOICES (mapcar (lambda (KV)
                           (let* ((K (car KV))
                                  (V (cdr KV))
                                  )
                             (list K (symbol-name V))))
                         sboo-mtg-color-abbreviations-alist))
        )

    (read-multiple-choice PROMPT CHOICES)))

;;----------------------------------------------;;

(cl-defun sboo-mtg-read-guild (&key prompt)

  ""

  (interactive)

  (let ((PROMPT        (or prompt "Guild: "))
        (COLLECTION    sboo-mtg-guild-list)
        (REQUIRE-MATCH t)
        )

    (completing-read PROMPT COLLECTION nil REQUIRE-MATCH)))

;;----------------------------------------------;;

(cl-defun mtg-read-card-name (&key )

  "Read an MTG card name, from `mtg-card-names'."

  (interactive)

  (when (require 'mtg-data nil :no-error)

    (let ((PROMPT (format "%s: "
                          "Card name"))

          (PREDICATE     nil)
          (REQUIRE-MATCH t)
          (DEFAULT       nil)

          (CANDIDATES (mtg-card-names))
          )

      (let* ((STRING
              (let ((helm-mode-fuzzy-match nil))
                (completing-read PROMPT CANDIDATES PREDICATE REQUIRE-MATCH nil nil DEFAULT)))
             )

        STRING))))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun mtg-get-card-name-max-length (cards)

  "Get `mtg-max-length-of-card-name'.

Inputs:

• CARDS — a `sequencep' of `mtg-card's.

Output:

• an `integerp'. 
  a natural (i.e. non-negative number).
  length is measured by `string-width'.

Example:

• M-: (mtg-get-card-name-max-length )
    ⇒ 

Related:

• `mtg-max-length-of-card-name'"

  (let* ((MAX-LENGTH (seq-reduce
                     (lambda (old-length new-card)
                       (let* ((new-length (string-width (mtg-card-name new-card)))
                              )
                         (max old-length new-length)))
                      cards
                      0)))

    MAX-LENGTH))

;; M-: (mtg-get-card-name-max-length (mtg-cards))

;; M-: (mtg-get-card-name-max-length (list (mtg-card-create :name "four") (mtg-card-create :name "sixsix") (mtg-card-create :name "1")))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 

;; M-: (progn (require 'json) (message "%S" (json-read-file "./colornames.json")))

;;==============================================;;

;; e.g. « Ancestral Recall » (one-line):
;;
;; {"object":"card","id":"46b0a5c2-ac85-448e-9e87-12fc74fd4147","oracle_id":"550c74d4-1fcb-406a-b02a-639a760a4380","multiverse_ids":[390],"tcgplayer_id":8671,"name":"Ancestral Recall","lang":"en","released_at":"1993-10-04","uri":"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147","scryfall_uri":"https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api","layout":"normal","highres_image":true,"image_uris":{"small":"https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970","normal":"https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970","large":"https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970","png":"https://img.scryfall.com/cards/png/en/leb/48.png?1525122970","art_crop":"https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970","border_crop":"https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970"},"mana_cost":"{U}","cmc":1.0,"type_line":"Instant","oracle_text":"Target player draws three cards.","colors":["U"],"color_identity":["U"],"legalities":{"standard":"not_legal","future":"not_legal","frontier":"not_legal","modern":"not_legal","legacy":"banned","pauper":"not_legal","vintage":"restricted","penny":"not_legal","commander":"banned","duel":"banned","oldschool":"restricted"},"games":["paper"],"reserved":true,"foil":false,"nonfoil":true,"oversized":false,"promo":false,"reprint":true,"set":"leb","set_name":"Limited Edition Beta","set_uri":"https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302","set_search_uri":"https://api.scryfall.com/cards/search?order=set\u0026q=e%3Aleb\u0026unique=prints","scryfall_set_uri":"https://scryfall.com/sets/leb?utm_source=api","rulings_uri":"https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings","prints_search_uri":"https://api.scryfall.com/cards/search?order=released\u0026q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380\u0026unique=prints","collector_number":"48","digital":false,"rarity":"rare","illustration_id":"d20eda7b-a902-4c00-bdab-601059e417b5","artist":"Mark Poole","border_color":"black","frame":"1993","frame_effect":"","full_art":false,"story_spotlight":false,"edhrec_rank":16999,"related_uris":{"gatherer":"http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390","tcgplayer_decks":"https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall\u0026page=1\u0026partner=Scryfall\u0026utm_campaign=affiliate\u0026utm_medium=scryfall\u0026utm_source=scryfall","edhrec":"http://edhrec.com/route/?cc=Ancestral+Recall","mtgtop8":"http://mtgtop8.com/search?MD_check=1\u0026SB_check=1\u0026cards=Ancestral+Recall"}}

;;----------------------------------------------;;

;; e.g. « Ancestral Recall » (pretty-printed):
;;
;; {
;;   "object": "card",
;;   "id": "46b0a5c2-ac85-448e-9e87-12fc74fd4147",
;;   "oracle_id": "550c74d4-1fcb-406a-b02a-639a760a4380",
;;   "multiverse_ids": [
;;     390
;;   ],
;;   "tcgplayer_id": 8671,
;;   "name": "Ancestral Recall",
;;   "lang": "en",
;;   "released_at": "1993-10-04",
;;   "uri": "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147",
;;   "scryfall_uri": "https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api",
;;   "layout": "normal",
;;   "highres_image": true,
;;   "image_uris": {
;;     "small": "https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970",
;;     "normal": "https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970",
;;     "large": "https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970",
;;     "png": "https://img.scryfall.com/cards/png/en/leb/48.png?1525122970",
;;     "art_crop": "https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970",
;;     "border_crop": "https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970"
;;   },
;;   "mana_cost": "{U}",
;;   "cmc": 1.0,
;;   "type_line": "Instant",
;;   "oracle_text": "Target player draws three cards.",
;;   "colors": [
;;     "U"
;;   ],
;;   "color_identity": [
;;     "U"
;;   ],
;;   "legalities": {
;;     "standard": "not_legal",
;;     "future": "not_legal",
;;     "frontier": "not_legal",
;;     "modern": "not_legal",
;;     "legacy": "banned",
;;     "pauper": "not_legal",
;;     "vintage": "restricted",
;;     "penny": "not_legal",
;;     "commander": "banned",
;;     "duel": "banned",
;;     "oldschool": "restricted"
;;   },
;;   "games": [
;;     "paper"
;;   ],
;;   "reserved": true,
;;   "foil": false,
;;   "nonfoil": true,
;;   "oversized": false,
;;   "promo": false,
;;   "reprint": true,
;;   "set": "leb",
;;   "set_name": "Limited Edition Beta",
;;   "set_uri": "https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302",
;;   "set_search_uri": "https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints",
;;   "scryfall_set_uri": "https://scryfall.com/sets/leb?utm_source=api",
;;   "rulings_uri": "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings",
;;   "prints_search_uri": "https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints",
;;   "collector_number": "48",
;;   "digital": false,
;;   "rarity": "rare",
;;   "illustration_id": "d20eda7b-a902-4c00-bdab-601059e417b5",
;;   "artist": "Mark Poole",
;;   "border_color": "black",
;;   "frame": "1993",
;;   "frame_effect": "",
;;   "full_art": false,
;;   "story_spotlight": false,
;;   "edhrec_rank": 16999,
;;   "related_uris": {
;;     "gatherer": "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390",
;;     "tcgplayer_decks": "https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall",
;;     "edhrec": "http://edhrec.com/route/?cc=Ancestral+Recall",
;;     "mtgtop8": "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall"
;;   }
;; }

;;----------------------------------------------;;

;; e.g. « Ancestral Recall » (parsed by `json-read' with a « plist » `json-object-type'):
;;
;; M-: (let* ((json-array-type  'vector) (json-object-type 'plist)) (json-read-file "./scryfall-default-cards.json"))
;;
;; '(
;;   :object             "card"
;;   :id                 "46b0a5c2-ac85-448e-9e87-12fc74fd4147"
;;   :oracle_id          "550c74d4-1fcb-406a-b02a-639a760a4380"
;;   :multiverse_ids     [390]
;;   :tcgplayer_id       8671
;;   :name               "Ancestral Recall"
;;   :lang               "en"
;;   :released_at        "1993-10-04"
;;   :uri                "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147"
;;   :scryfall_uri       "https://scryfall.com/card/leb/48/ancestral-recall?utm_source=api"
;;   :layout             "normal"
;;   :highres_image      t
;;   :mana_cost          "{U}"
;;   :cmc                1.0
;;   :type_line          "Instant"
;;   :oracle_text        "Target player draws three cards."
;;   :colors             ["U"]
;;   :color_identity     ["U"]
;;   :games              ["paper"]
;;   :reserved           t
;;   :foil               nil
;;   :nonfoil            t
;;   :oversized          nil
;;   :promo              nil
;;   :reprint            t
;;   :set                "leb"
;;   :set_name           "Limited Edition Beta"
;;   :set_uri            "https://api.scryfall.com/sets/5307bd88-637c-4a5c-9801-a0d887715302"
;;   :set_search_uri     "https://api.scryfall.com/cards/search?order=set&q=e%3Aleb&unique=prints"
;;   :scryfall_set_uri   "https://scryfall.com/sets/leb?utm_source=api"
;;   :rulings_uri        "https://api.scryfall.com/cards/46b0a5c2-ac85-448e-9e87-12fc74fd4147/rulings"
;;   :prints_search_uri  "https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints"
;;   :collector_number   "48"
;;   :digital            nil
;;   :rarity             "rare"
;;   :illustration_id    "d20eda7b-a902-4c00-bdab-601059e417b5"
;;   :artist             "Mark Poole"
;;   :border_color       "black"
;;   :frame              "1993"
;;   :frame_effect       ""
;;   :full_art           nil
;;   :story_spotlight    nil
;;   :edhrec_rank        16999
;;   :legalities          ( :standard "not_legal"
;;                          :future "not_legal"
;;                          :frontier "not_legal"
;;                          :modern "not_legal"
;;                          :legacy "banned"
;;                          :pauper "not_legal"
;;                          :vintage "restricted"
;;                          :penny "not_legal"
;;                          :commander "banned"
;;                          :duel "banned"
;;                          :oldschool "restricted"
;;                          )
;;   :image_uris ( :small "https://img.scryfall.com/cards/small/en/leb/48.jpg?1525122970"
;;                 :normal "https://img.scryfall.com/cards/normal/en/leb/48.jpg?1525122970"
;;                 :large "https://img.scryfall.com/cards/large/en/leb/48.jpg?1525122970"
;;                 :png "https://img.scryfall.com/cards/png/en/leb/48.png?1525122970"
;;                 :art_crop "https://img.scryfall.com/cards/art_crop/en/leb/48.jpg?1525122970"
;;                 :border_crop "https://img.scryfall.com/cards/border_crop/en/leb/48.jpg?1525122970"
;;                 )
;;   :related_uris ( :gatherer "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=390"
;;                   :tcgplayer_decks "https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall"
;;                   :edhrec "http://edhrec.com/route/?cc=Ancestral+Recall"
;;                   :mtgtop8 "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall"
;;                   )
;;   )

;;----------------------------------------------;;

;; e.g. « Ancestral Recall » (loaded into an `mtg-card'):
;;
;; 

;;==============================================;;

;;TODO....
;;    "name"              : "Ancestral Recall",

;;     "lang"              : "en",

;;     "scryfall_uri"      : "https://scryfall.com/card/ovnt/2018/ancestral-recall?utm_source=api",

;; image_uris.png             : "https://img.scryfall.com/cards/png/en/ovnt/2018.png?1523193176",
;; image_uris.art_crop        : "https://img.scryfall.com/cards/art_crop/en/ovnt/2018.jpg?1523193176",

;;     "mana_cost"         : "{U}",

;;     "cmc"               : 1,

;;     "type_line"         : "Instant",

;;     "oracle_text"       : "Target player draws three cards.",

;;     "colors"            : [ "U" ],

;;     "color_identity"    : [ "U" ],

;;       "legalities.standard"        : "not_legal",
;;       "legalities.vintage"         : "restricted",

;;     "rulings_uri"       : "https://api.scryfall.com/cards/d392392e-6c36-44e4-a037-2b788a088891/rulings",

;;     "rarity"            : "mythic",

;;     "artist"            : "Raoul Vitale",

;;     "border_color"      : "black",

;;     "frame"             : "2015",

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg)

;;; mtg.el ends here