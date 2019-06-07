;;; mtg-json.el --- -*- coding: utf-8; lexical-binding: t -*-

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

;; MTG JSON — parse ‘mtg.json’.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin requirements:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'json)
  (require 'seq)
  (require 'cl-lib))

;;==============================================;;

;; project requirements:

(progn
  (require 'mtg-types))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
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

(cl-defun mtg-read-color (&key prompt)

  "Read a color via `mtg-colors-list'."

  (interactive)

  (let ((PROMPT        (or prompt "Color: "))
        (COLLECTION    mtg-colors-list)
        (REQUIRE-MATCH t)
        )

    (read PROMPT CHOLLECTION nil REQUIRE-MATCH)))

;;----------------------------------------------;;

(cl-defun mtg-read-color-char (&key prompt)

  "Read a color via `mtg-color-abbreviations-alist'."

  (interactive)

  (let ((PROMPT  (or prompt "Color: "))
        (CHOICES (mapcar (lambda (KV)
                           (let* ((K (car KV))
                                  (V (cdr KV))
                                  )
                             (list K (symbol-name V))))
                         mtg-color-abbreviations-alist))
        )

    (read-multiple-choice PROMPT CHOICES)))

;;----------------------------------------------;;

(cl-defun mtg-read-guild (&key prompt)

  ""

  (interactive)

  (let ((PROMPT        (or prompt "Guild: "))
        (COLLECTION    mtg-guild-list)
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
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'mtg-json)

;;; mtg-json.el ends here