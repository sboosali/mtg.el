________________


BoF
________________




;;


;; define-mtg-keyword / define-mtg-subtype / etc from ‘https://mtgjson.com/api/v5/EnumValues.json’




;;


(defun mtg-card- (card)
  " CARD.
e.g. “”:
    M-: (mtg-creature- \"\")
     → '()"
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


(require 'thread)


(defun mtg-text/ (text)
  "Replace TEXT ."
  (thread-last text
   (replace-regexp-in-string "" "")
    (replace-regexp-in-string "" "")
    (replace-regexp-in-string "" "")
  ))


________________




-MTG-ETC


;;


mtg-:






;;


mtg-:






;;


mtg-:






;;


________________




mtg-cfg / mtg-bnf:


;;; Code:


;; 
;; • syntax vs semantics — e.g. left-types & right-types (syntactic) versus super-types & card-types & sub-types (semantic).
;; 


;;; Code — Parser:


(defconst mtg-cfg/grammar/
  '(


     (())


     (()


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/
  '(


     (())


     (()


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/
  '(


     (())


     (()


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/keyword-abilities
  '(keyword-abilities


     ((keyword-ability))


     ((keyword-abilities "," keyword-ability)
      (append $1 (list $2)))  ; nconc


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/keyword-ability
  '(keyword-ability


     (()
      (progn $1))


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/
  '(


     (())


     (()


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/
  '(


     (())


     (()


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar/
  '(


     (())


     (()


     )
  "Non-Terminal for .")


(defconst mtg-cfg/grammar
  `(


     ,mtg-cfg/grammar/
     ,mtg-cfg/grammar/
     ,mtg-cfg/grammar/
     ,mtg-cfg/grammar/
     ,mtg-cfg/grammar/keyword-abilities
     ,mtg-cfg/grammar/keyword-ability
     ,mtg-cfg/grammar/
     ,mtg-cfg/grammar/
     ,mtg-cfg/grammar/


   )
  "Context-Free Grammar for (most) M:tG cards.")


;;; Code — Lexer:


(define-lex-simple-regex-analyzer mtg-cfg/lexer/
  "Lex  tokens."
  (rx ) 'NUM)


(define-lex-simple-regex-analyzer mtg-cfg/lexer/
  "Lex  tokens."
  (rx ) 'NUM)


(define-lex mtg-cfg/lexer
  "Lexical Analyzer for (most) M:tG cards."
  mtg-cfg/lexer/
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  mtg-cfg/lexer/number
  mtg-cfg/lexer/punctuation
  semantic-lex-default-action)


;;


(define-lex-simple-regex-analyzer mtg-bnf/lexer/number
  "Lex  tokens."
  (rx ) 'NUM)
(define-lex-simple-regex-analyzer wisent-calc-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUM)
(define-lex-simple-regex-analyzer wisent-calc-lex-punctuation
  "Detect and create punctuation tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)" (char-after))


(define-lex wisent-calc-lexer
  "Calc lexical analyzer."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  wisent-calc-lex-number
  wisent-calc-lex-punctuation
  semantic-lex-default-action
  )




________________




;;


mtg-deck / mtg-results:


^ Type
^^ Subtype
^ Color


;;


mtg-query:


illusory "When ~ becomes the target of a spell [or ability] [an opponent controls], sacrifice it."
temporary,ephemeral "At the beginning of (the|your) next end step, (sacrifice|exile) ~."
virtual "When ~ dies, exile it."


proactive vs reactive


;;


mtg-results:


;;


(defconst mtg-results/sort-prints-by/default
  '((english t) (edition ) (archun rarity booster-slot) (color color-identity color-personality) (layout frame keyword t) ( t) ( t)
color color-identity color-personality card-type card-supertype card-subtype mana-value body-size layout frame keyword rules-text-alphabetically name
  ) "")


(defconst mtg-results/sort-cards-by/default
  '(color color-identity color-personality card-type card-supertype card-subtype mana-value body-size layout frame keyword rules-text-alphabetically name)
 "")


(defconst mtg-results/sort-editions-by/default
  '((black-bordered t) (offline t) (expansion primary t) release-date))


"archenemy", "box", "commander", "core", "draft_innovation", "duel_deck", "expansion", "from_the_vault", "funny", "masterpiece", "masters", "memorabilia", "planechase", "premium_deck", "promo", "spellbook", "starter", "token", "treasure_chest", "vanguard".


"black", "borderless", "gold", "silver", "white".


keyruneCode


;;n.b.
;; group the faces of a multi-faced card next to each other.
;; 


;;


sort-prints-by


sort-cards-by


sort-editions-by


;;
;; sort editions by:
;; • date
;; • name
;; • kind (border, reprints, etc)
;; • size
;; • 
;; • 
;; 
;; e.g. (let ((sort-prints-by '(edition ))) …)
;;
;; e.g. (let ((sort-editions-by '(edition ))) …)
;;
;; 


;;


mtg-eval:


Dragon's Fire
{1}{R} Instant
As an additional cost to cast this spell, you may reveal a Dragon card from your hand or choose a Dragon you control.
~ deals 3 damage to target creature or planeswalker. If you revealed a Dragon card or chose a Dragon as you cast this spell, ~ deals damage equal to the power of that card or creature instead.
↓
{1}{R} + ~ deals 3 damage to target creature or planeswalker.
|
{1}{R} + Reveal a Dragon card from your hand. + ~ deals X damage to target creature or planeswalker, where X is the revealed card's power.
|
{1}{R} + Choose a Dragon you control. + ~ deals X damage to target creature or planeswalker, where X is the chosen creature's power.


Lightning Axe
{R} Instant
As an additional cost to cast this spell, discard a card or pay {5}.
↓
{5}{R} + ~ deals 5 damage to target creature.
|
{R} + Discard a card. + ~ deals 5 damage to target creature.


Eaten Alive
{B} Sorcery
As an additional cost to cast this spell, sacrifice a creature or pay {3}{B}.
Exile target creature or planeswalker.
↓
{3}{B}{B} + Exile target creature or planeswalker.
|
{B} + Sacrifice a creature. + Exile target creature or planeswalker.


Silumgar's Scorn
{U}{U} Instant
As an additional cost to cast this spell, you may reveal a Dragon card from your hand. Counter target spell unless its controller pays {1}.
If you revealed a Dragon card or controlled a Dragon as you cast this spell, counter that spell instead.
↓
{U}{U} + Counter target spell unless its controller pays {1}.
|
{U}{U} + Reveal a Dragon card from your hand. + Counter target spell.


Daring Buccaneer
{R} 2/2 Pirate
As an additional cost to cast this spell, reveal a Pirate card from your hand or pay {2}.
↓
{2}{R} 2/2 Pirate
|
{R} 2/2 Pirate + Reveal a Pirate card from your hand.


↓
|




↓
|




↓
|


Crashing Tide
{2}{U} Sorcery
This spell has flash as long as you control a Merfolk.
Return target creature to its owner’s hand.
Draw a card.


Bloodbraid Marauder
Delirium — This spell has cascade as long as there are four or more card types among cards in your graveyard.


Torgaar, Famine Incarnate
{6}{B}{B} | {4}{B}{B} & Sacrifice a creature. | {2}{B}{B} & Sacrifice two creatures. | {B}{B} & Sacrifice three creatures. 


kids of chart:
- bar charts (histograms): for . with stats (mean, median, mode variance, etc). 
- pie charts (percentages): for . 


chart :
- mana value (bar)
- colors (pie)
- types (bar)
- mana vs non-mana (pie): lands, moxen, vs land-ramp, dorks, rocks, treasure-makers, rituals, vs non-mana
-  (bar)
-  (bar)
-  (bar)
-  (bar)


stats:
- mean: for (e.g. mean-mana-value for Dark Confidant), . 
- median: for (e.g. median- for ), . 
- mode: for (e.g. mode- for Predict?), . 
- variance: for (e.g. ), . 
- : for . 
- : for . 
- : for . 


;;


-mtg-parse


Bartered Cow
"When ~ dies or when you discard it, create a Food token."
'(when (or (dies this) (discarded this)) (create food))


-mtg-rx


(rx-define mtg-d20-range (char ?- ?– ?—))
(rx-define mtg-d20-guard (char ?|))


-mtg-eval


(defun mtg-eval/roll-d20 ()
  "Return a uniformly-random ‘natnump’ between 1 and 20 (inclusive)."
  (1+ (random 20)))


(defun mtg-eval/roll-d6 ()
  "Return a uniformly-random ‘natnump’ between 1 and 6 (inclusive)."
  (1+ (random 6)))


(defun mtg-eval/roll-d2 ()
  "Return a uniformly-random ‘natnump’ between 1 and 2 (inclusive)."
  (1+ (random 2)))


(defun mtg-eval/flip-coin ()
  "Return a uniformly-random ‘symbolp’ from either 'heads or 'tails."
  (aref (random 2) [heads tails]))


(defun mtg-eval/draw-card (deck)
  "Return a random ‘mtg-card-p’ from DECK (“after shuffling”)."
  (let* ((CARDS (mtg-deck-cards deck))
         (N-CARDS (length CARDS)))
    (aref (random N-CARDS) CARDS)))


-mtg-math


e.g. “Delina, Wild Mage”:
Whenever ~ attacks, choose target creature you control, then roll a d20.
1-14 | Create a tapped and attacking token that's a copy of that creature, except it's not legendary and it has "Exile this creature at end of combat."
15-20 | Create one of those tokens. You may roll again.


nondeterminism ≥ { randomness uncertainty }
randomness ≥ { draw roll choose-at-random }
uncertainty ≥ { hidden-information they-choose }
 ≥ { }


;;


(and (cost (<= 5)) (draw (>= 2)))
↓
: {3}{U}{U} Draw three cards.
&
Think Twice: {1}{U} Flashback {2}{U} Draw a card.
&
: {U} Investigate twice.
&


(activated (cost (<= 4)) (draw (== 1)))
↓
{3}{U}: Draw a card.
&
{1}{U}: Investigate.
&


;;


Serra Avenger
You can’t cast this spell during your first, second, or third turns of the game.


Rock Jockey
You can’t cast this spell if you’ve played a land this turn.


Rakdos, Lord of Riots
You can’t cast this spell unless an opponent lost life this turn.


Asmoranomardicadaistinaculdacar {}
As long as you’ve discarded a card this turn, you may pay {B/R} to cast this spell.


;;


Imperiosaur
Spend only mana produced by basic lands to cast this spell.


Myr Superion
Spend only mana produced by creatures to cast this spell.


Hogaak, Arisen Necropolis
{5}{B/G}{B/G} Creature
You can’t spend mana to cast this spell.
Convoke, delve (Each creature you tap while casting this spell pays for {1} or one mana of that creature’s color. Each card you exile from your graveyard pays for {1}.)
You may cast ~ from your graveyard.


;;


Memory Deluge
{2}{U}{U} Instant
Look at the top X cards of your library, where X is the amount of mana spent to cast this spell. Put two of them into your hand and the rest on the bottom of your library in a random order.
Flashback {5}{U}{U}
⟨↓⟩
{2}{U}{U} (Hand)
Look at the top four cards of your library. Put two of them into your hand and the rest on the bottom of your library in a random order.
⟨|⟩
{5}{U}{U} (Graveyard)
Look at the top seven cards of your library. Put two of them into your hand and the rest on the bottom of your library in a random order.


;;^ Flashback + (when-cast-from-graveyard | when-cast-for-its-flashback-cost | where-x-is-the-amount-of-mana-spent-to-cast-this-spell | …) ≈≤ Aftermath


Sundering Stroke
{6}{R} Sorcery
~ deals 7 damage divided as you choose among one, two, or three targets. If at least seven red mana was spent to cast this spell, instead ~ deals 7 damage to each of those permanents and/or players.
↓
{6}{R} ; ~ deals 7 damage divided as you choose among one, two, or three targets. If at least seven red mana was spent to cast this spell.
|
{R}{R}{R}{R}{R}{R}{R} ; ~ deals 7 damage to each of one, two, or three targets.


Plumb the Forbidden
{1}{B} Instant
As an additional cost to cast this spell, you may sacrifice one or more creatures. When you do, copy this spell for each creature sacrificed this way.
You draw a card and you lose 1 life.


Skyshroud Condor
{1}{U} 2/2 Bird
Cast this spell only if you’ve cast another spell this turn.
Flying


Massacre
{2}{B}{B} Sorcery
If an opponent controls a Plains and you control a Swamp, you may cast this spell without paying its mana cost.
All creatures get -2/-2 until end of turn.


Seedtime
{1}{G} Instant
Cast this spell only during your turn.
Take an extra turn after this one if an opponent cast a blue spell this turn.


Savage Beating
{3}{R}{R} Instant
Cast this spell only during your turn and only during combat.
Choose one —
• Creatures you control gain double strike until end of turn.
• Untap all creatures you control. After this phase, there is an additional combat phase.
Entwine {1}{R} (Choose both if you pay the entwine cost.)


Arterial Alchemy
When ~ enters the battlefield, create a Blood token for each opponent you have.
Blood tokens you control are Equipment in addition to their other types and have “Equipped creature gets +2/+0” and equip {2}.
↓≥
When ~ enters the battlefield, create a colorless Blood Equipment artifact token with “{1}, {T}, Discard a card, Sacrifice this artifact: Draw a card.”, “Equipped creature gets +2/+0”, and equip {2}.


Shallow Grave
{1}{B} Instant
Return the top creature card of your graveyard to the battlefield. That creature gains haste until end of turn. Exile it at the beginning of the next end step.
↓
%cares-about-graveyard-order ("top creature card of your graveyard")


Circling Vultures
You may discard ~ any time you could cast an instant.
At the beginning of your upkeep, sacrifice ~ unless you exile the top creature card of your graveyard.
↓
"Discard ~:." ≈ "You may discard ~ any time you could cast an instant."


"…<K>… . The same is true for <KLIST>."
Thunderous Orator
Whenever ~ attacks, it gains flying until end of turn if you control a creature with flying. The same is true for first strike, double strike, deathtouch, indestructible, lifelink, menace, and trample.
↓
Whenever ~ attacks, it gains flying until end of turn if you control a creature with flying. 
Whenever ~ attacks, it gains first strike until end of turn if you control a creature with first strike.
Whenever ~ attacks, it gains double strike until end of turn if you control a creature with double strike.
Whenever ~ attacks, it gains deathtouch until end of turn if you control a creature with deathtouch.
Whenever ~ attacks, it gains indestructible until end of turn if you control a creature with indestructible.
Whenever ~ attacks, it gains lifelink until end of turn if you control a creature with lifelink.
Whenever ~ attacks, it gains menace until end of turn if you control a creature with menace.
Whenever ~ attacks, it gains trample until end of turn if you control a creature with trample.


Solar Tide
{4}{W}{W} Sorcery
Choose one —
• Destroy all creatures with power 2 or less.
• Destroy all creatures with power 3 or greater.
Entwine—Sacrifice two lands. (Choose both if you pay the entwine cost.)
↓
|
{4}{W}{W}, Sacrifice two lands: Destroy all creatures.
* Choose two — • Destroy all creatures with power 2 or less. • Destroy all creatures with power 3 or greater. → Destroy all creatures with power 2 or less. Destroy all creatures with power 3 or greater. → Destroy all creatures with power 2 or less or with power 3 or greater. → Destroy all creatures.


Borrowed Grace
{2}{W} Instant
Escalate {1}{W} (Pay this cost for each mode chosen beyond the first.)
Choose one or both —
• Creatures you control get +2/+0 until end of turn.
• Creatures you control get +0/+2 until end of turn.
↓


↓


Collective Brutality
{1}{B} Sorcery
Escalate—Discard a card.
Choose one or more —
• Target opponent reveals their hand. You choose an instant or sorcery card from it. That player discards that card.
• Target creature gets -2/-2 until end of turn.
• Target opponent loses 2 life and you gain 2 life.
↓
{1}{B}: Choose one — • Target opponent reveals their hand. You choose an instant or sorcery card from it. That player discards that card. • Target creature gets -2/-2 until end of turn. • Target opponent loses 2 life and you gain 2 life.
|
{1}{B}, Discard a card: Choose two — • Target opponent reveals their hand. You choose an instant or sorcery card from it. That player discards that card. • Target creature gets -2/-2 until end of turn. • Target opponent loses 2 life and you gain 2 life.
|
{1}{B}, Discard two cards: Choose two — • Target opponent reveals their hand. You choose an instant or sorcery card from it. That player discards that card. • Target creature gets -2/-2 until end of turn. • Target opponent loses 2 life and you gain 2 life.


Twisted Reflection
{1}{U} Instant
Choose one —
• Target creature gets -6/-0 until end of turn.
• Switch target creature’s power and toughness until end of turn.
Entwine {B}
↓
{1}{U}{B} Instant
Choose two — Target creature gets -6/-0 until end of turn. Switch target creature’s power and toughness until end of turn.
≥
Target creature gets -6/-0 until end of turn. Switch target creature’s power and toughness until end of turn.
≥
Target creature gets -6/-0 until end of turn. Switch its power and toughness until end of turn.
≈
Target creature gets -0/-6 until end of turn.


Unbounded Potential
{1}{W} Instant
Choose one —
• Put a +1/+1 counter on each of up to two target creatures.
• Proliferate. (Choose any number of permanents and/or players, then give each another counter of each kind already there.)
Entwine {3}{W}
↓
{4}{W}{W}: Choose one — • Put a +1/+1 counter on each of up to two target creatures. • Proliferate. (Choose any number of permanents and/or players, then give each another counter of each kind already there.)
≥
Put two +1/+1 counters on each of up to two target creatures.


Tooth and Nail
{5}{G}{G} Sorcery
Choose one —
• Search your library for up to two creature cards, reveal them, put them into your hand, then shuffle.
• Put up to two creature cards from your hand onto the battlefield.
Entwine {2}


Journey of Discovery
{2}{G} Sorcery
Choose one —
• Search your library for up to two basic land cards, reveal them, put them into your hand, then shuffle.
• You may play up to two additional lands this turn.
Entwine {2}{G}


Blinding Beam
{2}{W} Instant
Choose one —
• Tap two target creatures.
• Creatures don’t untap during target player’s next untap step.
Entwine {1}
↓


Blinding Beam
{2}{W} Instant
Choose one —
• Tap two target creatures.
• Creatures don’t untap during target player’s next untap step.
Entwine {1}
↓


Grab the Reins
{3}{R} Instant
Choose one —
• Until end of turn, you gain control of target creature and it gains haste.
• Sacrifice a creature. Grab the Reins deals damage equal to that creature’s power to any target.
Entwine {2}{R}
↓


↓


;;


mana:
snow-mana: mana produced by a snow permanent. e.g. Graven Lore. e.g. Tundra Fumarole.
basic-mana: mana produced by a basic land. e.g. Imperiosaur.
dork-mana: mana produced by . e.g. Myr Superion.
rock-mana: mana produced by artifacts. e.g. .
treasure-mana: mana produced by treasure permanents (i.e. mostly by sacrificing a treasure token). e.g. Price of Loyalty.
-mana: mana produced by . e.g. .
-mana: mana produced by . e.g. .
-mana: mana produced by . e.g. .
-mana: mana produced by . e.g. .
-mana: mana produced by . e.g. .


;;








;;


________________




;; -MTG-PROP


draw-ish:
^ Draw
 Cycling
^ discover
^ tutor
 Transmute
^ wish
 Learn
^ “quickdraw” (i.e. quickdraw-from-deck)
^ “slowdraw” (i.e. upload-from-deck)
^ “theftdraw” (i.e. expropriate-from-deck)
^ mill+regrow
^ create-draw-token
 Investigate
^ manifest-from-deck
^ -from-deck
^ -from-deck


mana-ish (a.k.a. -ish):
add-mana
reduce-casting-cost
give-alternate-casting-cost
create-treasure (create-gold), create-eldrazi-spawn (create-eldrazi-scion), create-, &c
fetch-lands, fetch-mana-rocks, fetch-mana-dorks, &c
freecast ("you may cast that card without paying its mana cost")


pick (a.k.a. modalesque, a.k.a.):
"Choose one" &al, Entwine, Escalate, 
Split-cards, Fuse, Adventure-cards, 
{X}-spells
Cycling &al, Landcycling, 
" _" (e.g. ; e.g. )


kicker-ish:
Kicker &al, Multikicker, 
Morph &al, Megamorph, 
Evoke, Channel, Bloodrush, Dash, Cycling+DiscardTrigger, 
Bestow, 


creature-ish:
Creature
t:Vehicle
token creation (e.g. ; e.g. )
noncreature animation (e.g. ; e.g. )
self-animation (e.g. Celestial Colonnade; e.g. Phyrexian Totem)
EtB:[creature token] (e.g. Ancestral Blade)
 (e.g. )


aura-ish:
t:Aura (/ Enchant)
Equipment + EtB:equip (e.g. Embercleave; e.g. Ancestral Blade)
Bestow
Disturb front + Aura back
t:Licids


equipment-ish:
t:Equipment (/ Equip)
Aura + LtB:Regrow (e.g. Rancor)
EtB:[fetch Equipment]


shroud-ish:
Shroud
Hexproof
Ward _ (e.g. Rimeshield Frost Giant; e.g. Westgate Regent; e.g. Owlin Shieldmage)
Ward-ish (e.g. Frost Titan; e.g. Reality Smasher; e.g. Kira, Great Glass-Spinner & Glyph Keeper)
tax to target (e.g. Icefall Regent; e.g. )
hurt if target (e.g. Thunderbreak Regent & Retromancer; e.g. Warden of the Woods & Leovold, Emissary of Trest)


affinity-ish:
affinity-for-permanents
pseudo-affinity-for-spells (e.g. Cryptic Serpent)
"This spell costs <m> less to cast [if … | for each …]. [It also costs <m> less to cast [if … | for each …].]" (e.g. Bone Picker; e.g. Geistlight Snare; e.g. Cryptic Serpent; e.g. Embercleave; e.g. Blasphemous Act; e.g. the Avatar cycle; e.g. Metalwork Colossus; e.g. Ghalta, Primal Hunger)


convoke-ish:
convoke
improvise
tap-untapped-white-creature-you-control (&al)
" _" (e.g. ; e.g. )


emerge-ish:
emerge
offering


delve-ish:
delve


gravecast (a.k.a. flashback-ish):
flashback
aftermath
jump-start
disturb
embalm
eternalize
unearth
cremate-activation
"" (e.g. )


excast:
strict-excast (e.g. Moonhollow Griffin)
Forecast
Adventure


deckcast:
strict-deckcast (e.g. Panglacial Wurm)


counterspell-ish:
counter, counter-spell (counter-spell-exiling-it, counter-spell-bouncing-it, counter-spell-tucking-it, counter-spell-stealing-it, counter-spell-with-compensation, )
exile-spell (c.f. counter-spell-exiling-it, e.g. Dissipate)
bounce-spell (vs counter-spell-bouncing-it, e.g. Remand)
tuck-spell (vs counter-spell-tucking-it, e.g. )
steal-spell (vs counter-spell-stealing-it, e.g. Spelljack?)
manifest-spell: vs counter-spell-with-compensation (e.g. Swan Song); 
-spell (c.f. counter-spell-ing-it, e.g. )
-spell (c.f. counter-spell-ing-it, e.g. )


damage-ish:
damage
shrink-tou (c.f. damage)
put-minus-counters (c.f. damage)
kill-low-tou, kill-low-pow, kill-low-mv (e.g. Strangling Soot)
fight, bite (e.g. ; e.g. )


madness-ish:
madness
when-discarded-trigger (e.g. Bartered Cow; e.g. Edgar's Awakening; e.g. )


;;e.g. (madness-ish $0) matches both Basking Rootwalla and Bartered Cow.


random:
draw
roll, flip
choose-at-random
they-choose
hidden-information


transform-ish:
transform
meld
flip
monstrous
adapt
sacrifice-to-make-permanents (e.g. Giant Caterpillar, "Sacrifice ~: Create a token."; e.g. Sakura-Tribe Elder and Misty Rainforest, "Sacrifice ~: Fetch a land."; e.g. )
make-permanents-when-dies (e.g. Doomed Traveller, "When ~ dies, create a token."; e.g. )
permanent-with-cremate-to-make-permanents (e.g. Dauntless Cathar, "Cremate ~: Create a token."; e.g. )


(self-)vertical-growth:
put-+1/+1-counter
 graft
 mentor
put-+1/+1-counter-on-self
 evolve
 train
set-base-powtou-high
 e.g. Mirror Entity
set-self-base-powtou-high
 eternalize
aura
equipment
anthem
grant-double-strike


(self-)horizontal-growth:
create-creature-token
 populate
 o:storm + t:permanent


regenerate-ish:
regenerate
undying, persist
grant-temporary-indestructible
heal-damage
grant-temporary-protection, grant-temporary-damage-prevention
grant-temporary-shroud, grant-temporary-hexproof
flicker, rescue


threshold-ish:
threshold
delirium
undergrowth


 (e.g. )


tutor-ish:
tutor
 transmute
 landcycling
wish
 learn
dig (a.k.a. discover-from-top-many)
 n.b. (dig ∞) = (tutor)
 e.g. Dig Through Time


(self-)fork-ish (≤ cast-trigger):
replicate
storm
conspire
ripple
gravestorm
cascade
"When you cast this spell, …" (e.g. )


bounded-vs-unbounded effects:
- i.e. not burn-them (e.g. Grapeshot; e.g. Tendrils of Agony) or mill-them (e.g. Brainstorm) or attacker-creation or even attacker-pumping (e.g. Empty the Warrens; e.g. Haze of Rage).
- anti-permanent: bounce (e.g. Temporal Fissure; e.g. Scattershot); murder/edict (e.g. Wing Shards); counterspell (e.g. Flusterstorm).
- non-deck-draw: (e.g. Reaping the Graves).
- animation: (c.f. creation; e.g. Siege of Towers)


reactive-vs-proactive effects:
- (a.k.a. interrupts-vs-instants)
- e.g. countertithes vs taxes
-  


uncounterable(-ish):
`cant-be-countered` (e.g. ) ("This spell can't be countered.")
`split-second` (e.g. Sudden Shock)
`storm` (e.g. Grapeshot)
`(activated-ability…)`:
 `channel` (e.g. Ghost-Lit Raider)
 `cycling-trigger` (e.g. Slice and Dice)


Slice and Dice
When you cycle ~, you may have it deal 1 damage to each creature.
Cycling {2}{R} ({2}{R}, Discard this card: Draw a card.)


Ghost-Lit Raider
Channel — {3}{R}, Discard ~: It deals 4 damage to target creature.


devotion:
`devotion`


devotion-like:
`devotion` (e.g. Evangel of Heliod; e.g. Blight-Breath Catoblepas)
`chroma: among-mana-costs-of-your-permanents` (e.g. Springjack Shepherd, c.f. Evangel of Heliod; e.g. Outrage Shaman, c.f. Blight-Breath Catoblepas)


devotion-ish:
`devotion` (n.b. `(devotion COLOR)` ≈ `(devotion-ish COLOR your-battlefield)`)
`chroma:global` (e.g. Umbra Stalker has “graveyard-devotion-to-black” i.e. `(devotion-ish black your-graveyard)`; e.g. Primalcrux has “(battlefield-)devotion-to-green” i.e. `(devotion-ish green your-battlefield)`)


Chroma — ~’s power and toughness are each equal to the number of black mana symbols in the mana costs of cards in your graveyard.


Primalcrux
Chroma — ~’s power and toughness are each equal to the number of green mana symbols in the mana costs of permanents you control.


Phosphorescent Feast
Chroma — Reveal any number of cards in your hand. You gain 2 life for each green mana symbol in those cards’ mana costs.


Sanity Grinding
Chroma — Reveal the top ten cards of your library. For each blue mana symbol in the mana costs of the revealed cards, target opponent mills a card. Then put the cards you revealed this way on the bottom of your library in any order.


craft (`(craft MIN CARD ZONE)`):
`metalcraft` (n.b. `(metalcraft)` ≈ `(craft 3 artifact your-battlefield)`)
`spell-mastery` (n.b. `(spell-mastery)` ≈ `(craft 2 spell your-graveyard)`)
`threshold` (n.b. `(threshold)` ≈ `(craft 1 t your-graveyard)`)
`` (n.b. `()` ≈ `(craft   your-)`)
`` (n.b. `()` ≈ `(craft   your-)`)
`` (n.b. `()` ≈ `(craft   your-)`)


imprint-ish
imprint
exiled-with-this ("exiled with ~")


straight-ish (n.b. custom):
- for ordered properties (lists).
^ straight (what):
^^ mana-value-straight
^^ powtouloy-straight
^^ devotion-straight
^^ -straight
^^ -straight
^ straight (how):
^^ longest-straight (e.g. "When ~ enters the battlefield, it deals X damage to any target, where X is your longest nonland straight. (X is the maximum number of nonland permanents you control with consecutive unique mana values.)")
^^ choose-straight (e.g. "Choose target permanent straight. Put a +1/+1 on each creature in that straight. (Permanent straights are one or more permanents on the battlefield with consecutive unique mana values.)")
^ (e.g. )


flush-ish (n.b. custom):
- for unordered properties (sets).
^ flush (what):
^^ color-flush (& exact-colors-flush)
^^ type-flush (& exact-types-flush)
^^ keyword-flush (& exact-keywords-flush)
^^ exact-mana-cost-flush
^^ -flush (& exact--flush)
^^ -flush (& exact--flush)
^ flush (how):
^^ longest-flush (e.g. "When ~ enters the battlefield, it deals X damage to any target, where X is your longest nonland color flush. (X is the maximum number of permanents you control with the exact same colors. For example, colorless, blue, green-blue, blue-red, and green-blue-red are all different.)")
^^ choose-flush (e.g. "Choose target colored flush. Put a +1/+1 on each creature in that straight. (.)")
^ (e.g. )


untap-ish:
untap
blink
 (e.g. )


tap-ish:
tap
 twiddle
blink-tapped (e.g. Eldrazi Displacer)
 (e.g. )


blink-ish:
blink
¿wink?
rescue+refreeplay
sacrifice+reanimate


has-ability-ish:
has-keyword-ability (i.e. in the keyword-ability-list)
gives-self-keyword-abilty (e.g. Slivers: "Creature — Sliver" + "Slivers [you control] have <K>." → "<K>")
gives-self-abilty (e.g. Slivers: "Creature — Sliver" + "Slivers [you control] have “<A>.”" → "<A>.")
can-gain-abilty (e.g. Mystic Visionary: "As long as there are seven or more cards in your graveyard, ~ has <K>."; e.g. Arrogant Poet: "Whenever ~ attacks, you may pay 2 life. If you do, it gains <K> until end of turn."; e.g. : "Discard a card: ~ gains <K> until end of turn."; e.g. : "", etc)


pitch (a.k.a. discard-ish):
discard (e.g.)
exile-from-hand (e.g.)
process-expropriated
pitch-quickdrawn
pitch-uploaded
pitch-sideboard-lesson
pitch-


pitch-cost (a.k.a. discard-cost-ish):
discard-cost (e.g. Foil)
exile-from-hand-cost (e.g. Force of Will)
process-expropriated-cost (e.g. )
-quickdrawn-cost (e.g. )


roundly:
self-tap-activation
per-round-trigger
(e.g. : "")


turnly:


damage:


(e.g. : "")


counterspell:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


-ish:


(e.g. : "")


upgrades:
N < N+1
regrow < reanimate
tap, exert < freeze
colorless mana < colored mana < rainbow mana
ritual < treasure < land (&al)
 < 
 < 
 < 
 < 
 < 
 < 
 < 
 < 
 < 


;;


________________




;; -MTG-SEARCH


(defcustom mtg-results/select-by/default-quotient)


;; ^ each result is selected by:
;;
;; • prints — by default, all black-bordered card-prints are searched, and the query results are the matching cards' (1) english-language (2) recentest-printing. 
;; • cards —
;; • editions — . for example, you can match assist each cards, then match sets with at least one matching card.
;; • text — e.g. 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; 


(defcustom mtg-results/sort-by/default-order)


;; ^ sort&group the results by (one or more of):
;;
;; • date — original print date.
;; • name — 
;; • color — 
;; • mana-value — 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; •  — 
;; • strength — how strong the card is (cost, effect, etc; bans, price, etc).
;; • …
;; • number-of-reprints — 
;;


(defcustom mtg-results/group-by/default-order)


;;


(defcustom mtg-cardprint-results/)


;;


(defcustom mtg-card-results/)


;;


(defvar mtg-edition-results/-alist
  )


;; 


(defcustom mtg--results/)


;; 


(defcustom mtg-query/select-by/char ?)


;; ^
;; e.g. “…”
;;


(defcustom mtg-query/sort-by/char ?)


;; ^
;; e.g. “…”
;;


;;n.b.
;; “@…” a.k.a. “@<…” (text-has) vs “@=…” (text-is)
;; 


;;


(defcustom mtg-query-nullary-prediate-alist


(square . (eql POW TOU))


(vanilla        . (null TEXT))
(french-vanilla . ( TEXT))
; (pcase TEXT (`(keywords ,@...) t) (_ nil))
; (and (eql 1 (length TEXT)) (eq 'keywords (car TEXT)))


(colorless  . (eql 0 (length COLORS)))
(monocolor  . (eql 1 (length COLORS)))
(bicolor    . (eql 2 (length COLORS)))
(tricolor   . (eql 3 (length COLORS)))
(four-color . (eql 4 (length COLORS)))
(five-color . (eql 5 (length COLORS)))
(all-color  . (eq t COLORS))


( . ())
( . ())
( . ())
( . ())
( . ())


)


;;


(defcustom mtg-query-unary-prediate-alist


( . ())
( . ())


)


;;


;; “#$%…” — Devotion (mnemonic: “#” counts, “$” costs, “%” colors). e.g. #$%g>=2 (cards with a devotion-of-green of at least, a.k.a. cards that cost at least {G}{G}).


;;


(defun mtg- ())


;;


(defun mtg-query-token ())


;; search for tokens as cards. e.g. the query `(is token) (has "Sacrifice ~:")` matches the token made by `Create a colorless Treasure artifact token with “Sacrifice this artifact: Add one mana of any color.”`.


(defun mtg- ())


;; “~~” . (or "~" "this spell")


;;


:


- `mtg-with-buffer` (`(mtg-with-buffer t …)`, (`(mtg-with-buffer 'rules-text …)`, `(mtg-with-buffer 'card-name …)`, `(mtg-with-buffer 'type-line …)`, `(mtg-with-buffer 'flavor-text …)`, `(mtg-with-buffer '(artist flavor-text) …)`, etc) — call predicate procedure within a temporary buffer whose contents are some text from the card. e.g. `(mtg-card-with-rules-text-buffer ())` → `(mtg-with-buffer 'text ())` → `(with-temp-buffer (insert TEXT) ())`. 


:
- `(text (string P))` — call predicate `P` on a card's rules text as a string. e.g. `(text (string (knickname "Tymaret"))` → ``. e.g. `(text (string ( ))` → `(string-prefix-p "" TEXT)`. 
- `(text (buffer P))` — call predicate `P` in a buffer with a card's rules text. e.g. `(text (buffer ))` → `(with-temp-buffer (insert TEXT) ())`. 


;;


________________




;; -MTG-CFG


;; https://github.com/Soothsilver/mtg-grammar/blob/master/mtg.g4






;; https://github.com/rmmilewi/mtgcompiler/blob/master/mtgcompiler/frontend/compilers/LarkMtgJson/grammar.py


(player . ("you" "they" "player" "opponent" "controller" "owner teammate" "team"))






;;


;; https://hudecekpetr.cz/a-formal-grammar-for-magic-the-gathering/


;;> Flower // Flourish: reads “… a basic Forest or Plains card, …”. The parser interprets it as “a basic Forest” or “Plains card”, as opposed to the correct “a basic card that’s a Forest or that’s a Plains”. This happens because it considers the words “basic Forest” and “Plains” to be of the same kind, when in fact one’s already a compound of a supertype and a subtype and the other is a subtype only.
;;> Aurelia, Exemplar of Justice: parses, but incorrectly. It reads, in part, “that creature gets +2/+0, gains trample if it’s red, and gains vigilance if it’s white.” Which the parser interprets as “if it’s white, it gains vigilance” and “if it’s white and red, it gains trample and +2/+0” as opposed to the correct “+2/+0 always”, “vigilance if white”, and “trample if red”.
;;> Pelt Collector: reads “Whenever …, if that creature’s power is greater than Pelt Collector’s, …” which makes sense in English but is difficult to parse because the adjective-like ‘Peltr Collector’s’ doesn’t qualify anything. It would parse correctly if it said “greater than Pelt Collector’s power”.
;;> Chance for Glory: I thought that the template “[object] gains [abilities] until [event]” would work for all ability-gaining abilities, but Chance for Glory reads, “Creatures you control gain indestructible.” There’s no “until”, not after, not in front. They gain indestructible indefinitely.
;;> Gruesome Menagerie: reads “Choose a creature card with mana value 1 in your graveyard, then do the same for creature cards with mana value 2 and 3. …”, because the effect is very unique so I wrote a term that can only ever apply to this card.
;;> : 
;;> : 
;;> : 
;;> : 
;;> : 
;;> : 
;;> : 
;;> : 
;;> : 
;;> : 


;;


;; pre-parse:
;; • card name to "~".
;; • ".”" to "”.".
;; • 
;; • 


;;


________________




;; -MTG-CARD


;;


;;
;; • '(* )
;; • '(let (()) ())
;; • '()
;; • '()
;; • '()
;; • '()
;; • '()
;; • '()
;; • '()
;; • '()
;;


;;


'(* 3 r) → [r r r]


'(let ((colorless-artifact-mana (c :only '(artifact))) (* 3 colorless-artifact-mana))


;;


________________




;; -MTG-QUERY


;;










;;
;; • $$… — real mana cost. e.g. “$$0” (a.k.a. “$$>=[{0}]”) means “really costs no mana”, matches Force of Will, , etc. e.g. “#$0 $$<=[Discard a card]” means “”. 
;; • #$$… — real mana value. e.g. “#$$<=1 #$%==0” (a.k.a. “$$1”) means “can cost one generic mana or less”; matches Dismember, Frogmite, , etc. 
;; • … — 
;;
;; • @… and @[…] 
;; • @"…"
;; • @(rx …)
;; • @(bnf …)
;; 
;; • @… a.k.a. @>=…
;; • @==…
;; 




;;


"#$2"
↓
(#$ 2)
↓
(mv (= _ 2))
↓
(= MANA-VALUE 2)
↓
(λ (CARD) (= (mtg-mana-value CARD) 2))


"#$2-"
↓
(#$ (t . 2))
↓
(mv (<= _ 2))
↓
(<= MANA-VALUE 2)
↓
(λ (CARD) (<= (mtg-mana-value CARD) 2))


"#$1,2,3"
↓
(#$ (, 1 2 3))
↓
(mv (or (= _ 1) (= _ 2) (= _ 3)))
↓
(or (= 1 MANA-VALUE) (= 2 MANA-VALUE) (= 3 MANA-VALUE))
↓
(λ (CARD) (and (<= 1 MANA-VALUE) (<= MANA-VALUE 3)))


;;


(defvar mtg-edition-query/predicate-alist
  '(
   ) "")


;; (set EDITION-Q [CARD-Q])
;;
;; the ‘set’ query: keep only the cards of an edition that CARD-Q matches (keep all if CARD-Q is committed), then keep only EDITION-Q that EDITION-Q matches.
;;
;; n.b. (set _) ≈ (set _ t)
;;
;; 


;; e.g. sets with at least 100  cards:


(set (>= SIZE 100))


;; e.g. sets with at least 100  original cards:


(set (>= SIZE 100) (not (is reprint)))


;;


________________




(set-cards (λ (CARDS) EDITION-Q) CARD-Q)


;; e.g. sets with Colossal Dreadmaw:


(set CARDS (name "Colossal Dreadmaw"))


;; e.g. sets with :


()




;; e.g. sets whose cards are at least 10% reprints:


(set (>= (/ (length CARDS) (length (cards SET))) 0.10) (is reprint))




;; e.g. sets that originally-print at least two cards that were then reprinted:






;; e.g. sets that reprint a card at a lower-rarity:






;; e.g. sets :


()






;; 


________________




-TBS-CG


{G} Instant
Choose X, where X is the number of spell's you cast this turn. You may choose the same mode more than once.
• Untap target land you control.
• Put two +1/+1 counters on target land you control. Unless it's a creature, it becomes a 0/0 creature until end of turn that's still a land. 


:
- c.f.:
Rude Awakening
{4}{G} Sorcery
Choose one —
• Untap all lands you control.
• Until end of turn, lands you control become 2/2 creatures that are still lands.
Entwine {2}{G} (Choose both if you pay the entwine cost.)




what are PlayPoints?


* Glyphs: that you write/draw/etc.
   * different shapes: 
   * different sizes: 
   * * 





________________




-MTG-SQL


(defconst mtg-sqlite-prints-db-file "AllPrintings.sqlite")


(defconst mtg-sqlite-prints-q-file "all-printings.sql")


(defun mtg-sqlite-config ()
  (add-hook 'sql-mode-hook #'mtg-turn-on-sqlite-mode))


(defun mtg-turn-on-sqlite-mode ()
  (cond
    ((let ((RX (rx-to-string `(: "/" ,mtg-sqlite-prints-q-file eos))))
       (string-match-p RX buffer-file-name))
     (let ((DB (expand-file-name mtg-sqlite-prints-db-file)))
       (setq-local sql-database DB))
    (()
     ())))


(defun mtg-interactive-sqlite-mode (&optional prompt-file-p)
  (interactive "P")
  (sql-connect ' "**")
  )


(with-eval-after-load 'sql
  (mtg-sqlite-config))


;;


(use-package sql
  :config  
  (setq sql-sqlite-program "sqlite3")
  (add-to-list 'same-window-buffer-names "*SQL*")
  (defalias 'sql-get-login #'ignore)
;;(setq sql-set-product 'sqlite)
  ())


;; ^
;; “.sql”
;; ‘sql-send-region’ (<C-c C-r> in ‘sql-mode-map’).
;


(use-package sql-upcase
  :config
  (setq sql-upcase-mixed-case t)
  (dolist (HOOK '(sql-mode-hook sql-interactive-mode-hook))
    (add-hook HOOK #'sql-upcase-mode))
  ())


;; ^




;; 


________________




-MTG-JSON


(defun mtg-interactive-json-mode (&optional prompt-file-p)
  (interactive "P")
  )


;;


________________




-MTG-SCRYFALL


;; 
o:/exile.*return/
This ensures both that the words are in the right order and in a single ability,
`o:/…/`
Cards that reference “graveyard” and then “library” in an effect in that order:
o:/graveyard.*library/
Cards that reference “black creature” excluding those that only reference “nonblack creature”:
o:/(?<!non)black creature/
Creatures with “partner” but not “partner with”:
o:/partner(?! with)/
Cards that create tokens when they enter the battlefield:
o:/when ~ enters.*create.*token/
Triggered abilities that create tokens:
o:/(\bat\b|\bwhen).*,.*create.*token/
Generic mana cost reduction effects:
o:/costs \{\d+\} less to cast/
“Fetch lands” in the broadest sense:
t:land o:/sacrifice.*search.*battlefield/
All Cards with “Cycling 1“:
o:/\bcycling \{1\}(?!{)/
Many cards that remove creatures:
o:/(destroy|exile) target .*(creature|permanent)/
Cards that remove creatures, excluding 'blink' effects that then 'return' the card to play:
o:/(destroy|exile) target (creature|permanent)(?!.*return)/
Cards that deal specific, double digit amounts of damage:
o:/deals \d\d+/
Activated abilities that grant certain keywords:
o:/:.*gain.*(flying|vigilance|haste|trample|indestructible|double strike)/
Activated abilities that cost discarding a card:
o:/discard.*:/
Cards with flashback, excluding some that reference flashback:
o:/^flashback/
Activated abilities that cost only sacrificing a creature:
o:/^sacrifice a creature.*:/
Activated abilities that only cost sacrificing this card:
o:/^sacrifice ~:/
Activated abilities that remove a +1/+1 counter in the cost:
o:/^remove.*\+1\/\+1.*counter.*:/
Include a blue mana symbol:
o:/{u}/
Include a blue mana symbol in an activated abilities cost:
o:/{u}.*:/
...cost only a blue mana:
o:/^{u}:/
...cost any amount of blue but no other cost:
o:/^({u})+:/
...cost a blue, no other mana cost, but may have other costs:
o:/(?<!\})({u})+(?!\{).*:/
The “swords of x and y” cycle, excluding other swords:
name:/sword of.*and/
Alternate win, and loss, conditions:
o:/(wins?|loses?) the game/
Cards that roll dice and care about dice rolling:
o:/\broll(ed)?\b.*\b(d\d+|die|dice)\b/
Land cycling cards:
o:/(plains|island|swamp|mountain|forest|basic land)cycling/


________________




-MTG-MATH


(defun mtg-math/median (list &optional key)
  "Return the median-average of (nonempty) LIST.
e.g. :
    M-: (mtg-math/median 0 1 1 2 3 5 8)
     → 2
    M-: (mtg-math/median 0 1 1 2 3 5 8 13)
     → 2.5"
  (declare (pure t) (side-effect-free t))
  (let* ((KEY (or key #'identity))
         (LIST (sort-by list KEY))
         (COUNT (length list)))
    (cond
      ((= 0   COUNT)
       nil)


      ((oddp  COUNT)
       (let* ((I (/ (1- COUNT) 2)))
         (apply KEY ( list I)))))


      ((evenp COUNT)
       (let* ((I (/ COUNT 2)) (J (1- I)))
         (/ (+ (apply KEY ( list I)
               (apply KEY ( list J)))
            2))
      )))


(defun mtg-math/mean (list &optional key)
  "Return the mean-average of (nonempty) LIST.
e.g. :
    M-: (mtg-math/mean 0 1 1 2 3 5 8)
     → (/ 20 7)
     → 2.85714285714
    M-: (mtg-math/mean 0 1 1 2 3 5 8 13)
     → (/ 33 8)
     → 4.125"
  (declare (pure t) (side-effect-free t))
    (cl-loop for ITEM in list
      sum (apply KEY ITEM) into SUM
      count t into COUNT
      finally return (/ SUM COUNT))))


;;


(defun mtg-deck-median- (deck))






(defun mtg-edition-median- (edition))






________________




-MTG-GAME


(defun mtg-creature-combat-damage (creature &optional pow)
  "How much damage attacking CREATURE could do.
e.g. “”:
    M-: (mtg-creature-combat-damage (make-mtg-creature-card :pow 2 :has '(double-strike)))
     → 4"
  (declare (pure t) (side-effect-free t))
  (let* ((POW (or pow (mtg-creature-card-pow creature)))
         (ABILITIES (mtg-creature-card-abilities creature)))
         (DOUBLE-STRIKE-P (memq 'double-strike ABILITIES))
         (-P )
         (POW        (if -P TOU POW))
         (POW-BONUS  (if -P 0 0))
         (POW-FACTOR (if DOUBLE-STRIKE-P 2 1))
         ()
         )
    (* (+ POW POW-BONUS) POW-FACTOR)))


;;
;; - double-strike → pow × 2
;; - while-attacking-effect → …
;; - during-your-turn-effect → …
;; - precombat-trigger → …
;; - beginning-of-combat-trigger → …
;; - attack-trigger → …
;; -  → 
;;
;; Doran, the Siege Tower
;; 
;;


;;


(defun mtg-text-tildify (text &optional name)
  "Return TEXT with NAME as “~”.
e.g. “Tymaret, the Murder King”:
    M-: (mapcar #'mtg-text-tildify '(\"{1}{R}, Sacrifice another creature: Tymaret, the Murder King deals 2 damage to target player or planeswalker.\" \"{1}{B}, Sacrifice a creature: Return Tymaret from your graveyard to your hand.\"))
     → '(\"{1}{R}, Sacrifice another creature: ~ deals 2 damage to target player or planeswalker.\" \"{1}{B}, Sacrifice a creature: Return ~ from your graveyard to your hand.\")"
  (declare (pure t) (side-effect-free t))
    (let* ((NAME (or name "CARDNAME"))
           (NICKNAME (or (mtg-card-knickname-from NAME) NAME))
           (NAME-RX (rx-to-string `(or ,NAME ,NICKNAME))))
      (replace-regexp-in-string NAME-RX "~" text)))


(defun mtg-card-knickname-from (name)
  " NAME.
M-: (mtg-card-knickname-from \"Tymaret, the Murder King\")
 → \"Tymaret\"
M-: (mtg-card-knickname-from \"Servant of Tymaret\")
 → nil"
  (when-let* ((COMMA-INDEX (save-match-data (string-match name (rx mtg-comma)) (match-beginning 0))))
    (substring 0 COMMA-INDEX name)))


;;
;;>Tymaret, the Murder King
;;>{1}{R}, Sacrifice another creature: Tymaret, the Murder King deals 2 damage to target player or planeswalker.
;;>{1}{B}, Sacrifice a creature: Return Tymaret from your graveyard to your hand.
;;
;;>Tymaret, Chosen from Death
;;>Tymaret’s toughness is equal to your devotion to black.
;;>


________________




-MTG-KWDS






________________




-MTG-RX




________________




-MTG-LINT


;;(dishonest-ability-word :wrong #' :right #' )
(dishonest-metalcraft :wrong "" :right "" :desc "")


(inapplicable-continuous-effect :wrong #' :right #')
(layer-6-4 :wrong "Creatures you control with <ABILITY> have <TYPE>." :right '("" "") :desc "")


;; >613. Interaction of Continuous Effects…
;; >613.1a. Layer 1: Rules and effects that modify copiable values are applied.
;; >613.1b. Layer 2: Control-changing effects are applied.
;; >613.1c. Layer 3: Text-changing effects are applied. See rule 612, "Text-Changing Effects."
;; >613.1d. Layer 4: Type-changing effects are applied. These include effects that change an object's card type, subtype, and/or supertype.
;; >613.1e. Layer 5: Color-changing effects are applied.
;; >613.1f. Layer 6: Ability-adding effects, keyword counters, ability-removing effects, and effects that say an object can't have an ability are applied.
;; >613.1g. Layer 7: Power- and/or toughness-changing effects are applied.
;; >
;; >
;; >
;; >


(defun mtg-parse/ability-word (text)
  "Parse TEXT as an ‘mtg-ability-word-p’ paragraph."
  )


(mtg-parse/ability-word-name)
(rx (mtg-ability-word (group-n 1 mtg-any-name)))


(mtg-parse/ability-word-effect)
(rx mtg-sentences)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ability-word "")
;;      → '() 
;;


(defun mtg-text/flatten-choose-text (text)
  "Return a “Choose <N> — …” TEXT as the equivalent (but longer) “Choose one — …”.
e.g. “Choose two — • A. • B. • C. • D.” → “Choose one — • A. B. • A. C. • A. D. • B. C. • B. D. • C. D.”"
  )


;;
;; e.g. “Ogre Savant”:
;;     M-: (mtg-parse/card-text "When ~ enters the battlefield, if {U} was spent to cast it, return target creature to its owner’s hand." "{4}{R}")
;;      → '(triggered-ability :trigger etb :if (spent [u]) :effect (bounce creature)))
;;
;; e.g. “Unexplained Vision”:
;;     M-: (mtg-parse/card-text "Draw three cards. If at least three blue mana was spent to cast this spell, scry 3.")
;;      → '((draw 3) (if (spent [u u u]) (scry 3)))
;;
;; e.g. “Clockwork Servant”:
;;     M-: (mtg-parse/card-text "When ~ enters the battlefield, if at least three mana of the same color was spent to cast it, draw a card.")
;;      → '((if (spent (same-color 3)) (draw 1)))
;;


________________




-MTG-PARSE


;;


mtg-parse-rules-text
merge bulleted onto same line for searching (multiple lines for displaying)


mtg-split-lines


(replace-regexp-in-string (rx mtg-newline (group mtg-point)) " \\1" text)


;;OLD (string-split (rx (: mtg-newline (not mtg-point))) text)


mtg-merge-lines


(
  (cl-loop for LINE in LINES
    while (string-match (rx bos mtg-point) LINE)
    concat (concat " " LINE)
    )
)


;;


(defun mtg-parse/choose (line)
  "Parse “Choose … — • …” LINE.
TODO “Choose <X>(–<Y>) — (• …)+”
"
  )


;;
;; - “Choose one [or (both|more)] —\n”
;; - “Choose [up to] <N> —\n”
;; - “Choose <N>. You may choose the same mode more than once.\n”
;;
;; e.g. “Cryptic Command”:
;;     M-: (mtg-parse/choose "Choose two — • Counter target spell. • Return target permanent to its owner's hand. • Tap all creatures your opponents control. • Draw a card.")
;;      → '(choose 2 nil ("Counter target spell." "Return target permanent to its owner's hand." "Tap all creatures your opponents control." "Draw a card."))
;;
;; e.g. “Mystic Confluence”:
;;     M-: (mtg-parse/choose "Choose three. You may choose the same mode more than once. • Counter target spell unless its controller pays {3}. • Return target creature to its owner's hand. • Draw a card.")
;;      → '(choose 3 t ("Counter target spell unless its controller pays {3}." "Return target creature to its owner's hand." "Draw a card."))
;;
;; e.g. “”:
;;     M-: (mtg-parse/choose "Choose one or both — ")
;;      → '(choose (1 2) nil ("" "" "" ""))
;;
;; e.g. “”:
;;     M-: (mtg-parse/choose "Choose one or more — ")
;;      → '(choose (1 t) nil ("" "" "" ""))
;;
;; e.g. “”:
;;     M-: (mtg-parse/choose "Choose up to two — ")
;;      → '(choose (0 2) nil ("" "" "" ""))
;;
;; e.g. “”:
;;     M-: (mtg-parse/choose "Choose ")
;;      → '(choose (1 2) nil ("" "" "" ""))
;;


(mtg-parse/triggered-ability)


 … 


;;
;; - “When[ever] …, ….”
;; - “At …, ….”
;; - “When … [or at …], ….”
;;
;; e.g. “”:
;;     M-: (mtg-parse/triggered-ability "When ~ enters the battlefield, .")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “”:
;;     M-: (mtg-parse/triggered-ability "At the beginning of your upkeep, .")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “”:
;;     M-: (mtg-parse/triggered-ability "When you cast this spell, .")
;;      → '(activated-ability :trigger (cast this) :effect "…") 
;;
;; e.g. “Inferno Titan”:
;;     M-: (mtg-parse/triggered-ability "Whenever ~ enters the battlefield or attacks, .")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “Old Rutstein”:
;;     M-: (mtg-parse/triggered-ability "When ~ enters the battlefield or at the beginning of your upkeep, .")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “Mogg War Marshal”:
;;     M-: (mtg-parse/triggered-ability "When ~ enters the battlefield or dies, ….")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “Efreet Weaponmaster”:
;;     M-: (mtg-parse/triggered-ability "When ~ enters the battlefield or is turned face up, ….")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “Blind Hunter”:
;;     M-: (mtg-parse/triggered-ability "When ~ enters the battlefield or the creature it haunts dies, ….")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “Ichor Wellspring”:
;;     M-: (mtg-parse/triggered-ability "When ~ enters the battlefield or is put into a graveyard from the battlefield, draw a card.")
;;      → (mtg-make-triggered-ability :trigger '(or etb dies) :effect ((draw 1)))
;;
;; e.g. “Tomebound Lich”:
;;     M-: (mtg-parse/triggered-ability "Whenever ~ enters the battlefield or deals combat damage to a player, draw a card, then discard a card.")
;;      → (mtg-make-triggered-ability :trigger '(or etb (hits player combat-damage) :effect ((loot 1))) 
;;
;; e.g. “Brine Comber”:
;;     M-: (mtg-parse/triggered-ability "Whenever ~ enters the battlefield or becomes the target of an Aura spell, ….")
;;      → (mtg-make-triggered-ability :trigger '(or etb (targets aura this)) :effect "…") 
;;
;; e.g. “Jerren, Corrupted Bishop // Ormendahl, the Corrupter”:
;;     M-: (mtg-parse/triggered-ability "Whenever ~ enters the battlefield or another nontoken Human you control dies, ….")
;;      → (mtg-make-triggered-ability :trigger '(or etb (dies (and another nontoken Human yours))) :effect "…") 
;;
;; e.g. “”:
;;     M-: (mtg-parse/triggered-ability ", ….")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “”:
;;     M-: (mtg-parse/triggered-ability ", ….")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;; e.g. “”:
;;     M-: (mtg-parse/triggered-ability ", ….")
;;      → (mtg-make-triggered-ability :trigger '(or ) :effect "…") 
;;
;;


(mtg-parse/activated-ability)


{T}: … Activate only as a sorcery.


;;
;; - “<…>: <…>. [Activate [this ability] <…>.]”
;; - “<…>: [<…>.] Choose <…>. [Activate <…>.] [ • <…>]+”
;; - “<…>: [<…>.] Roll <…>. [Activate <…>.] [ | <…>]+”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/activated-ability "{W}: Gain 1 life.")
;;      → (mtg-make-activated-ability :cost '([w]) :timing 'slow :effect (gain-life 1))
;;
;; e.g. “”:
;;     M-: (mtg-parse/activated-ability "{T}: ~ deals 1 damage to any target. Activate only as a sorcery.")
;;      → (mtg-make-activated-ability :cost '([t]) :timing 'slow :effect '(damage 1 t)) 
;;
;; e.g. “”:
;;     M-: (mtg-parse/activated-ability "{X}: Gain X life. Activate only once each turn.")
;;      → (mtg-make-activated-ability :cost '([x]) :timing '(once (turn any)) :effect '(gain-life x))
;;
;; e.g. “Tetzimoc, Primal Death”:
;;     M-: (mtg-parse/activated-ability "{B}, Reveal ~ from your hand: Put a prey counter on target creature. Activate only during your turn.")
;;      → (mtg-make-activated-ability :cost '([b] (reveal-this hand)) :timing '(during (turn yours)) :effect '())
;;
;; e.g. “”:
;;     M-: (mtg-parse/activated-ability ": .")
;;      → (mtg-make-activated-ability :cost '() :timing 'fast :effect '())
;;
;;
;; e.g. “”:
;;     M-: (mtg-parse/activated-ability ": .")
;;      → (mtg-make-activated-ability :cost '() :timing 'fast :effect '())
;;
;;


(mtg-parse/spell-ability)


(or (mtg-parse/spell-timing) (mtg-parse/spell-casting) (mtg-parse/spell-additional-cost))


;;


(mtg-parse/spell-timing)


"Flash (You may cast this spell any time you could cast an instant.)"
"Cast this spell only during combat."
"Cast this spell only before combat or during combat before blockers are declared."


;;
;; - “You may cast this spell …” (“Flash”)
;; - “Cast this spell only …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "You may cast this spell any time you could cast an instant.")
;;      → '(cast flash)
;;     M-: (mtg-parse/spell-timing "Flash")
;;      → '(cast flash)
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "During combat, you may cast this spell any time you could cast an instant.")
;;      → '(cast ambush)
;;     M-: (mtg-parse/spell-timing "Ambush")
;;      → '(cast ambush)
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "During your turn, you may cast this spell any time you could cast an instant.")
;;      → '(cast rush)
;;     M-: (mtg-parse/spell-timing "Rush")
;;      → '(cast rush)
;;
;; e.g. “Mandate of Peace”:
;;     M-: (mtg-parse/spell-timing "Cast this spell only during combat.")
;;      → '(cast (only combat))
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-timing "")
;;      → '() 
;;


()


Counter target spell if it's blue.
Destroy target permanent if it's blue.


;;
;; e.g. “Red Elemental Blast”:
;;     M-: (mtg-match/destroy-ish "Destroy target blue permanent.")
;;      → '(destroy blue)
;;
;; e.g. “Red Elemental Blast”:
;;     M-: (mtg-parse/counter-ish "Counter target blue spell.")
;;      → '(counter blue)
;;
;; e.g. “Pyroblast”:
;;     M-: (mtg-match/destroy-ish "Destroy target permanent if it's blue.")
;;      → '(destroy blue)
;;
;; e.g. “Pyroblast”:
;;     M-: (mtg-parse/counter-ish "Counter target spell if it's blue.")
;;      → '(counter blue)
;;


(mtg-parse/spell-casting)


Firespout
{2}{R/G} Sorcery
~ deals 3 damage to each creature without flying if {R} was spent to cast this spell and 3 damage to each creature with flying if {G} was spent to cast this spell. (Do both if {R}{G} was spent.)
→
{2}{R} ~ deals 3 damage to each creature without flying.
{2}{G} ~ deals 3 damage to each creature with flying.
{1}{R}{G} ~ deals 3 damage to each creature. (~ deals 3 damage to each creature without flying and 3 damage to each creature with flying.)


Mythos of Nethroi
{2}{B} Instant
Destroy target nonland permanent if it’s a creature or if {G}{W} was spent to cast this spell.
→
{2}{B} Destroy target nonland creature.
{W}{B}{G} Destroy target nonland permanent.


River's Grasp
{3}{U/B} Sorcery
If {U} was spent to cast this spell, return up to one target creature to its owner’s hand. If {B} was spent to cast this spell, target player reveals their hand, you choose a nonland card from it, then that player discards that card. (Do both if {U}{B} was spent.)


Mythos of Brokkos
{2}{G}{G} Sorcery
If {U}{B} was spent to cast this spell, search your library for a card, put that card into your graveyard, then shuffle.
Return up to two permanent cards from your graveyard to your hand.


;;
;; - “If {_} was spent to cast this spell, …”
;; - “Adamant — If at least three _ mana was spent to cast this spell, …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-casting "")
;;      → '() 
;;


(mtg-parse/spell-additional-cost)


(rx (mtg-words "As an additional cost to cast this spell, ") (mtg-phrase-until ?.) ".")


;;
;; “As an additional cost to cast this spell, …”
;;
;; e.g. “Bone Splinters”:
;;     M-: (mtg-parse/spell-additional-cost "As an additional cost to cast this spell, sacrifice a creature.")
;;      → '(sacrifice creature)
;;
;; e.g. “Bone Shards”:
;;     M-: (mtg-parse/spell-additional-cost "As an additional cost to cast this spell, sacrifice a creature or discard a card.")
;;      → '(or (sacrifice 1 creature) (discard 1 t))
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-additional-cost "As an additional cost to cast this spell, ")
;;      → '()
;;
;; e.g. “”:
;;     M-: (mtg-parse/spell-additional-cost "As an additional cost to cast this spell, ")
;;      → '()
;;


(mtg-parse/roll)


;;
;; e.g. “Contact Other Plane”:
;;     M-: (mtg-parse/roll "Roll a d20. 1–9 | Draw two cards. 10–19 | Scry 2, then draw two cards. 20 | Scry 3, then draw three cards.")
;;      → '(roll-d20 d20 ((1 . 9) "Draw two cards.") ((10 . 19) "Scry 2, then draw two cards.") ((20) "Scry 3, then draw three cards."))
;; 
;; e.g. “Berserker's Frenzy”:
;;     M-: (mtg-parse/roll "Roll two d20 and ignore the lower roll. 1—14 | Choose any number of creatures. They block this turn if able. 15—20 | You choose which creatures block this turn and how those creatures block.")
;;      → '(roll-d20 (max d20 d20) ((1 . 14) "Choose any number of creatures. They block this turn if able.") ((15 . 20) "You choose which creatures block this turn and how those creatures block."))
;; 
;; e.g. “Revivify”:
;;     M-: (mtg-parse/roll "Roll a d20 and add the number of creature cards in your graveyard that were put there from the battlefield this turn. 1-14 | Return all creature cards in your graveyard that were put there from the battlefield this turn to your hand. 15+ | Return those cards from your graveyard to the battlefield.")
;;      → '(roll-d20 (+ d20 "the number of creature cards in your graveyard that were put there from the battlefield this turn") ((1 . 14) "Return all creature cards in your graveyard that were put there from the battlefield this turn to your hand.") ((15 . t) "Return those cards from your graveyard to the battlefield."))
;; 
;; e.g. “Valiant Endeavor”:
;;     M-: (mtg-parse/roll "Roll two d6 and choose one result. Destroy each creature with power greater than or equal to that result. Then create a number of 2/2 white Knight creature tokens with vigilance equal to the other result.")
;;      → '(roll-d6 (choose d6 d6) "Destroy each creature with power greater than or equal to that result. Then create a number of 2/2 white Knight creature tokens with vigilance equal to the other result.")
;;
;; Wizard's Spellbook
;; '("{T}: Exile target instant or sorcery card from a graveyard. Roll a d20. Activate only as a sorcery." "1-9 | Copy that card. You may cast the copy." "10-19 | Copy that card. You may cast the copy by paying {1} rather than paying its mana cost." "20 | Copy each card exiled with Wizard's Spellbook. You may cast any number of the copies without paying their mana costs.")
;;
;; 
;;


(mtg-parse/dungeon)
(mtg-parse/dungeon-room)
(mtg-parse/dungeon-room-name)
(rx (mtg-abiility-word (group-n 1 mtg-any-name)))
(mtg-parse/dungeon-room-effect)
(rx )
(mtg-parse/dungeon-room-leads)
(rx (mtg-reminder-text (: "Leads to: " (group-n 1 (0+ (not mtg-close-paren))))))


(rx-define mtg-ability-word (s)
  `(: ,s (1+ blank) mtg-dash (1+ blank)))


(rx-define mtg-any-ability-word
  `(mtg-ability-word mtg-any-name))


(rx-define mtg-any-name
  '(or (: mtg-word-char (0+ mtg-name-char))
       (: (0+ mtg-name-char) mtg-word-char)))


(rx-define mtg-any-word
  '(1+ mtg-word-char))


(rx-define mtg-name-char
  '(or mtg-word-char (char ?\ )))


(rx-define mtg-word-char
  '(or alphanumeric (char ?- ?' ?– ?’ ?‘)))


(rx-define mtg-reminder-text (s)
  `(: mtg-open-paren ,s mtg-close-paren))


(rx-define mtg-any-reminder-text
  `(mtg-reminder-text (0+ (not mtg-close-paren))))


(defun mtg-parse--split-lines (text)
  "Split TEXT into lines (by “\n” or “;”)."
  (string-split (rx mtg-eol) text))
(defun mtg-parse--split-sentences (text)
  "Split TEXT into sentences (by “.” &al)."
  (string-split (rx mtg-period) text))
(defun mtg-parse--split-csvs (text)
  "Split TEXT into comma-separated values (by “,” &al)."
  (string-split (rx mtg-comma) text))
(defun mtg-parse--split-modes (text)
  "Split TEXT into chooseable modes (by “•” &al)."
  (string-split (rx mtg-bullet) text))
(defun mtg-parse--split-results (text)
  "Split TEXT into die-roll results (by “|”)."
  (string-split (rx mtg-guard) text))
(defun mtg-parse--split-s (text)
  "Split TEXT into s (by “”)."
  (string-split (rx mtg-) text))
;;(defun mtg-parse--split-s (text)
;;  "Split TEXT into s (by “”)."
;;  (string-split (rx mtg-) text))


;;
;; - “<ROOM> — <TEXT>. [(Leads to: <ROOM>[, <ROOM>]*)]”
;;
;; e.g. “Lost Mine of Phandelver
Dungeon”:
;;     M-: (mtg-parse/dungeon "Lost Mine of Phandelver
Dungeon" '("Cave Entrance — Scry 1. (Leads to: Goblin Lair, Mine Tunnels)" "Goblin Lair — Create a 1/1 red Goblin creature token. (Leads to: Storeroom, Dark Pool)" "Mine Tunnels — Create a Treasure token. (Leads to: Dark Pool, Fungi Cavern)" "Storeroom — Put a +1/+1 counter on target creature. (Leads to: Temple of Dumathoin)" "Dark Pool — Each opponent loses 1 life and you gain 1 life. (Leads to: Temple of Dumathoin)" "Fungi Cavern — Target creature gets -4/-0 until your next turn. (Leads to: Temple of Dumathoin)" "Temple of Dumathoin — Draw a card.")
;;      → (mtg-make-dungeon :id 'lost-mine-of-phandelver-dungeon :name "Lost Mine of Phandelver
Dungeon" :rooms (list (mtg-make-dungeon-room :id 'cave-entrance :name "Cave Entrance" :leads '(goblin-lair mine-tunnels) :text '((scry 1))) (mtg-make-dungeon-room :id 'goblin-lair :name "Goblin Lair" :leads '(storeroom dark-pool) :text '((create goblin))) (mtg-make-dungeon-room :id 'mine-tunnels :name "Mine Tunnels" :leads '(dark-pool fungi-cavern) :text '((create treasure))) (mtg-make-dungeon-room :id 'storeroom :name "Storeroom" :leads '(temple-of-dumathoin) :text '((put-counter plus creature))) (mtg-make-dungeon-room :id 'dark-pool :name "Dark Pool" :leads '(temple-of-dumathoin) :text '((drain 1 (opponent each)))) (mtg-make-dungeon-room :id 'fungi-cavern :name "Fungi Cavern" :leads '(temple-of-dumathoin) :text '((gets-powtou -4 -0 creature))) (mtg-make-dungeon-room :id 'temple-of-dumathoin :name "Temple of Dumathoin" :leads '() :text '((draw 1)))))
;;


(mtg-parse/saga)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “The Triumph of Anax”:
;;     M-: (mtg-parse/saga "I, II, III — Until end of turn, target creature gains trample and gets +X/+0, where X is the number of lore counters on ~.")
;;      → '(:lore (1 2 3) :text "Until end of turn, target creature gains trample and gets +X/+0, where X is the number of lore counters on ~.")
;;


(mtg-eval/saga)


;;
;; e.g. “The Triumph of Anax”:
;;     M-: (mtg-parse/saga '(((1 2 3) . "Until end of turn, target creature gains trample and gets +X/+0, where X is the number of lore counters on ~.") ((4) . "Target creature you control fights up to one target creature you don't control.")))
;;      → '((1 . "Until end of turn, target creature gains trample and gets +1/+0.") (2 . "Until end of turn, target creature gains trample and gets +2/+0.") (3 . "Until end of turn, target creature gains trample and gets +3/+0.") (4 . "Target creature you control fights up to one target creature you don't control. "))
;;


(As this Saga enters and after your draw step, add a lore counter. Sacrifice after IV.)\nI, II, III — Until end of turn, target creature gains trample and gets +X/+0, where X is the number of lore counters on The Triumph of Anax.\nIV — Target creature you control fights up to one target creature you don't control. (Each deals damage equal to its power to the other.)


(mtg-parse/class)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “Wizard Class”:
;;     M-: (mtg-parse/class "")
;;      → '() 


Wizard Class
{U} Enchantment — Class
(Gain the next level as a sorcery to add its ability.)
You have no maximum hand size.
{2}{U}: Level 2
When this Class becomes level 2, draw two cards.
{4}{U}: Level 3
Whenever you draw a card, put a +1/+1 counter on target creature you control.


(mtg-parse/freeze-ability)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “Tamiyo, the Moon Sage”:
;;     M-: (mtg-parse/freeze-ability "{+1}: Tap target permanent. It doesn’t untap during its controller’s next untap step.")
;;      → '(freeze 1 permanent t)
;;
;; e.g. “Ajani Vengeant”:
;;     M-: (mtg-parse/freeze-ability "{+1}: Target permanent doesn’t untap during its controller’s next untap step.")
;;      → '(freeze 1 permanent nil)
;;      → '(exert 1 permanent)
;;
;; e.g. “Fire // Ice”:
;;     M-: (mtg-parse/freeze-ability "Tap target permanent.")
;;      → '(freeze 0 permanent t)
;;      → '(tap permanent)
;;
;; e.g. “Time of Ice”:
;;     M-: (mtg-parse/freeze-ability "I, II — Tap target creature an opponent controls. It doesn’t untap during its controller’s untap step for as long as you control ~.")
;;      → '(freeze (while you-control-this) (creature :controlled-by opponent) t)
;;
;; e.g. “Stensia Innkeeper”:
;;     M-: (mtg-parse/freeze-ability "Tap target land an opponent controls. That land doesn’t untap during its controller’s next untap step.")
;;      → '(freeze 1 (land :controller opponent) t)
;;
;; e.g. “”:
;;     M-: (mtg-parse/freeze-ability "")
;;      → '(freeze 1 () t)
;;


(mtg-eval/normalize-freeze)


;; freeze N t ~=~ tap + exert N
;;
;; e.g. freeze 0 = tap:
;;     M-: (mtg-eval/normalize-freeze '(freeze 1 permanent nil))
;;      → '(exert 1 permanent)
;;
;; e.g. freeze N nil = exert N:
;;     M-: (mtg-eval/normalize-freeze '(freeze 0 permanent t))
;;      → '(tap permanent)
;;


(defconst mtg-kicker-check-rx "if ~ was kicked with its <m> kicker,")


(mtg-parse/kicker)


Jilt
{1}{U}; Kicker {1}{R}
Return target creature to its owner’s hand. If this spell was kicked, it deals 2 damage to another target creature.


Thornscape Battlemage
{2}{G}; Kicker {R} and/or {W}
When ~ enters the battlefield, if it was kicked with its {R} kicker, it deals 2 damage to any target.
When ~ enters the battlefield, if it was kicked with its {W} kicker, destroy target artifact.


Cetavolver
{1}{U} 1/1 Volver; Kicker {1}{R} and/or {G}
If ~ was kicked with its {1}{R} kicker, it enters the battlefield with two +1/+1 counters on it and with first strike.
If ~ was kicked with its {G} kicker, it enters the battlefield with a +1/+1 counter on it and with trample.


;;
;; - Kicker {…} (| Kicker—…) + If ~ was kicked, … (where ~ can be "this spell", "CARDNAME", "it")
;; - Kicker {…} and/or {…} + If ~ was kicked with its {…} kicker, …
;; - Kicker—…; Kicker—… + If ~ was kicked with its “…” kicker, …
;; - 
;;
;; e.g. “Jilt”:
;;     M-: (mtg-parse/kicker "")
;;      → (mtg-make-kicked-card :kicked-with `((t . ,(mtg-make-card ))) :unkicked (mtg-make-card )) 
;;
;; e.g. “Thornscape Battlemage”:
;;     M-: (mtg-parse/kicker "")
;;      → '(list :unkicked '() :kicked-with '(( . ()) ( . ())))
;;
;; e.g. “Cetavolver”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-eval/kicker)


Jilt
{1}{U}; Return target creature to its owner’s hand.
{2}{U}{R}; Return target creature to its owner’s hand. ~ deals 2 damage to another target creature.


Thornscape Battlemage
{2}{G} ø
{2}{R}{G}; When ~ enters the battlefield, it deals 2 damage to any target.
{2}{G}{W}; When ~ enters the battlefield, destroy target artifact.
{2}{R}{G}{W}; When ~ enters the battlefield, it deals 2 damage to any target, then destroy target artifact.


Cetavolver
{1}{U}    1/1; ø
{1}{G}{U} 2/2; Trample
{2}{U}{R} 3/3; First strike


If ~ was kicked with its {1}{R} kicker, it enters the battlefield with two +1/+1 counters on it and with first strike.
If ~ was kicked with its {G} kicker, it enters the battlefield with a +1/+1 counter on it and with trample.


;;
;; - 
;;
;; e.g. “Jilt”:
;;     M-: (mtg-eval/kicker "")
;;      → (mtg-make-kicked-card :kicked (mtg-make-card …) :unkicked (mtg-make-card )) 
;;      → (mtg-make-kicked-card :kicked-with `((t . ,(mtg-make-card :cost [1 u] :text '((bounce creature))))) :unkicked (mtg-make-card :cost [2 u r] :text '((bounce (creature :target a)) (damage 2 (creature :target b)))) :type '(instant) :colors '(u) :color-identity '(u r) ) 
;;
;; e.g. “Thornscape Battlemage”:
;;     M-: (mtg-eval/kicker "")
;;      → (mtg-make-kicked-card :unkicked '() :kicked-with '(( . ()) ( . ())))
;;
;; e.g. “Cetavolver”:
;;     M-: (mtg-eval/kicker "")
;;      → (mtg-make-kicked-card :unkicked '() :kicked-with '(( . ()) ( . ())))
;;


(mtg-parse/modalish)


;;
;; - “ …”
;; - “ …”
;; - The “”: “You may pay an additional {_} as you cast this spell.”; “If the {_} cost was paid, …”.
;; - The “Masteries”: “You may pay {_} rather than pay this spell’s mana cost.”; “If the {_} cost was paid, …”; “If that cost wasn’t paid, …”.
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


;;


--MTG-PARSE


(mtg-parse/)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-parse/)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-parse/)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-parse/)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-parse/)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-parse/)


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-parse/ "")
;;      → '() 
;;


(mtg-parse/surge)
“if ~'s surge cost was paid”
e.g. Reckless Bushwhacker


(mtg-parse/spectacle)
“if ~'s spectacle cost was paid”
e.g. Rafter Demon
e.g. Rix Maadi Reveler


(mtg-parse/prowl)
“if its prowl cost was paid”
e.g. Latchkey Faerie


(mtg-parse/madness)
“if ~'s madness cost was paid”
“if ~ was cast from exile”
e.g. Grave Robber
e.g. Avacyn's Judgment


(mtg-parse/flashback)
“if ~'s flashback cost was paid”
“if ~ was cast from a graveyard”
e.g. Ignite the Future
e.g. Secrets of the Key


Undergrowth
{G} Instant
As an additional cost to cast this spell, you may pay {2}{R}.
Prevent all combat damage that would be dealt this turn. If this spell’s additional cost was paid, this effect doesn’t affect combat damage that would be dealt by red creatures.


________________




;; -MTG-EVAL


;;


(mtg-eval CARD [GAME])


(mtg-eval 'death-s-shadow (mtg-make-game-state :your-life-total 3))
↓
(mtg-eval (mtg-make-card :pow 13 :tou 13 :text '((powtou-modify :pow (- x) :tou (- x) :where ((x your-life-total)))) '(your-life-total 3)))
↓
(mtg-eval (mtg-make-card :pow 13 :tou 13 :text '((powtou-modify :pow (- x) :tou (- x) :where ((x 3)))) '()))
↓
(mtg-eval (mtg-make-card :pow 13 :tou 13 :text '((powtou-modify :pow (- 3) :tou (- 3))) nil))
↓
(mtg-eval (mtg-make-card :pow (- 13 3) :tou (- 13 3) :text '() nil))
↓
(mtg-eval (mtg-make-card :pow 10 :tou 10 :text nil) nil)
↓
(mtg-make-card :pow 10 :tou 10 :text nil)


Gather the Townsfolk
{1}{W} Sorcery
Create two 1/1 white Human creature tokens.
Fateful hour — If you have 5 or less life, create five of those tokens instead.
↓
(> your-life-total 5) → Create two 1/1 white Human creature tokens.
|
(<= your-life-total 5) → Create five 1/1 white Human creature tokens.


;;
;; - “…”
;;
;; e.g. “Death's Shadow” (“~ gets -X/-X, where X is your life total.”):
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;
;; e.g. “”:
;;     M-: (mtg-eval "")
;;      → '() 
;;


(mtg-eval/multiply-effect count effect)


;;
;; - “…”
;;
;; - (mtg-eval/multiply-effect 0 x) → ()
;; - (mtg-eval/multiply-effect 1 x) → (list x)
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect 2 '(draw 2)))
;;      → '((draw 4))
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect 2 '(scry 2)))
;;      → '((scry 2) (scry 2))
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect 2 '(damage 1 (target t)))
;;      → '((damage 2 (target t) divide))
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect 2 '(damage 1 (each t)))
;;      → '((damage 2 (each t)))
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect '())
;;      → '(()) 
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect '())
;;      → '(()) 
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect '())
;;      → '(()) 
;;
;; e.g. “”:
;;     M-: (mtg-eval/multiply-effect '())
;;      → '(()) 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/ )


;;
;; - “…”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


(mtg-eval/excess-damage )


Lacerate Flesh
~ deals 4 damage to target creature. Create a number of Blood tokens equal to the amount of excess damage dealt to that creature this way.


;;
;; - “…”
;;
;; e.g. “Lacerate Flesh”:
;;     M-: (mtg-eval/ '((damage 4 (target creature) :excess (create excess-damage blood))) '(:tou 3 :marked-damage 2))
;;      → '((damage 4 _) (create 3 blood)) 
;;


(mtg-eval/<k>)
“if ~'s <k> cost was paid”
prowl
surge
spectacle
madness


;;
;; - “ …”
;; - “ …”
;;
;; e.g. “”:
;;     M-: (mtg-eval/ "")
;;      → '() 
;;


Baleful Mastery
{3}{B} Instant
You may pay {1}{B} rather than pay this spell’s mana cost.
If the {1}{B} cost was paid, an opponent draws a card.
Exile target creature or planeswalker.
↓
{3}{B} ; An opponent draws a card. Exile target creature or planeswalker.
|
{1}{B} ; Exile target creature or planeswalker.


Ingenious Mastery
{X}{2}{U} Sorcery
You may pay {2}{U} rather than pay this spell’s mana cost.
If the {2}{U} cost was paid, you draw three cards, then an opponent creates two Treasure tokens and they scry 2. If that cost wasn’t paid, you draw X cards.
↓
{X}{2}{U} ; You draw X cards.
|
{2}{U} ; You draw three cards, then an opponent creates two Treasure tokens and they scry 2.


Undergrowth
{G} Instant
As an additional cost to cast this spell, you may pay {2}{R}.
Prevent all combat damage that would be dealt this turn. If this spell’s additional cost was paid, this effect doesn’t affect combat damage that would be dealt by red creatures.
↓
{G} ; Prevent all combat damage that would be dealt this turn.
|
{2}{R}{G} ; Prevent all combat damage that would be dealt this turn. This effect doesn’t affect combat damage that would be dealt by red creatures.


Avacyn's Judgment
{1}{R} Sorcery
Madness {X}{R} (If you discard this card, discard it into exile. When you do, cast it for its madness cost or put it into your graveyard.)
Avacyn’s Judgment deals 2 damage divided as you choose among any number of targets. If this spell’s madness cost was paid, it deals X damage divided as you choose among those permanents and/or players instead.


;;


(mtg-eval/stormish )


;;
;; - Fork-ish:
;;     - “When you cast this spell, copy it [if …].”
;;     - e.g. “Mentor's Guidance”
;; - Storm-ish:
;;     - “When you cast this spell, copy it for each …”
;;     - ^ “Storm” = “”
;;     - ^ “Replicate {_}” = “”
;;     - e.g. “Show of Confidence”
;;     - e.g. “Grapeshot”
;;     - e.g. “Pyromatics”
;;
;; e.g. “”:
;;     M-: (mtg-eval/stormish "")
;;      → '() 
;;


Mentor's Guidance
{2}{U} Sorcery
When you cast this spell, copy it if you control a planeswalker, Cleric, Druid, Shaman, Warlock, or Wizard.
Scry 1, then draw a card.


Show of Confidence
{1}{W} Instant
When you cast this spell, copy it for each other instant and sorcery spell you’ve cast this turn. You may choose new targets for the copies.
Put a +1/+1 counter on target creature. It gains vigilance until end of turn.


;;






;;
________________




e.g. *Student of Warfare* (a `is:leveler` card) — doesn't match `has:FirstStrike`, but does match `gets:FirstStrike`:
- `[Level 0–1] ∅`
- `[Level 2–6] 3/3 First strike`
- `[Level 7+] 4/4 Double strike`
e.g. *Cobalt Golem* (a `rules:"~ gains flying"` card) — doesn't match `has:Flying`, but does match `gets:Flying`:
- `{1}{U}: ~ gains flying until end of turn.`


mtg.g4:


card : '\n'* ability ('\n'+ ability)* |;
ability : keywords | activated | staticOrSpell | triggered | modal | abilityWordAbility | additionalCostToCastSpell | weirdAbility;


https://www.emacswiki.org/emacs/download/sql-upcase.el


________________


MtGmacs
---


(defun mtg-card- (card)
  " CARD."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


;;^ e.g. ‹›:
;;     M-: (mtg- "")
;;       ↳ '()


(defun mtg-card- (card)
  " CARD."
  (declare (pure t) (side-effect-free t))
  (pcase ()
    (`() ())
    (`() ()))
    (_ ())))


(cl-case (read-char)
  (?a (do-a-thing))
  (?b (do-b-thing))
  ((?\r ?\n) (do-ret-thing))
  (t (do-other-thing)))


https://raw.githubusercontent.com/DamienCassou/hierarchy/master/examples/hierarchy-examples-faces.el
https://raw.githubusercontent.com/DamienCassou/hierarchy/master/examples/hierarchy-examples-fs.el
https://raw.githubusercontent.com/DamienCassou/hierarchy/master/examples/hierarchy-examples-major-modes.el
https://mtg.design/


cl-position  :test #'eq


(when (and strict _) (puthash _ _ WARNINGS))


TODO how to extend with extra fields.


'png      PNG 745×1040 (~2Mb a.k.a. 2000kb)
'large    JPG 672×936  (~303kb)
'normal   JPG 488×680  (~164kb)
'small    JPG 146×204  (~16kb)
'art-crop JPG W×H      (e.g. 626×457 131kb)


When requesting the image format, you may also provide a version parameter for the specific image version you would like returned. It can be small, normal, large, png, art_crop, or border_crop. The default is large.


Card images on Scryfall are copyright Wizards of the Coast (and/or their artist, for very old sets). when using the art_crop: List the artist name and copyright elsewhere in the same interface presenting the art crop, or use the full card image elsewhere in the same interface. Users should be able to identify the artist and source of the image somehow.


(cl-struct-slot-info 'mtg-color)
(name . opts), where name is the name of the slot and opts is the list of slot options given to defstruct. Dummy entries represent the slots used for the struct name and that are skipped to implement :initial-offset.


␣//␣


URL ‘https://scryfall.com/docs/api/cards’
mech:
.produced_mana: Colors of mana that this card could produce.
reserved.
.all_parts: If this card is closely related to other cards (because it calls them by name, or generates a token, or melds, etc) have this property with an array pf other cards.
Aesth:
.digital: True if this card was only released in a video game.
full_art: True if this card’s artwork is larger than normal.
games.
.printed_name: The localized name printed on this card, if any.
.printed_text: The localized text printed on this card, if any.
.printed_type_line: The localized type line printed on this card, if any.
.released_at: The date this card was first released.
.preview.previewed_at: The date this card was previewed.
.reprint: True if this card is a reprint.


(cl-defun
 (var…
 &optional (var initform svar)…
 &rest var
 &key ((keyword var) initform svar)…
 &aux (var initform)…))


(cl-loop for CARD being each mtg-card [of <edition>]
  …)
(put 'mtg-card 'cl-loop-for-handler …)


{2}{R}{R} 1/1 Wizard 🪄 Archer 🏹 
Whenever one or more instant and/or sorcery cards are put into your graveyard from anywhere, ~ deals that much damage to any target.
≥


https://mtgjson.com/api/v5/SetList.json


curl https://api.scryfall.com/symbology
curl https://c2.scryfall.com/file/scryfall-symbols/card-symbols/GU.svg


git clone https://github.com/andrewgioia/keyrune && cd keyrune/svg && cat tsp.svg


git clone https://github.com/andrewgioia/mana && cd mana/svg && cat u.svg


________________




MTG-PROP


;;; ‘mtg-prop’:


(cl-defstruct (mtg-card-prop)
  (id    nil :type symbol)
  (name  nil :type (or null string))
  ))


;;^
;;
;; 


;;


;;


(mtg-define-predicate immediate)
(or (type spell) (text "When ~ enters the battlefield, ") (text "When you cast this spell, ") (and (type planeswalker) (any text (loyalty-ability (and (text "draw") (<= loyalty-cost base-loyalty))))))


(mtg-define-creaturesque-predicate )
vehicle
self-animator creature-token-creator creature-fetcher 


mtg-ability/self-animation
(card nil :type (or nil symbol))


mtg-ability/token-creator
(card nil :type (or nil symbol))


;;


(defconst mtg-query-card-ability-word-alist )
addendum metalcraft 


(addendum-ish . (addendum (text "if you cast this spell during your main phase,")))
;;e.g. ‹Careful Consideration›: « Target player draws four cards, then discards three cards. If you cast this spell during your main phase, instead that player draws four cards, then discards two cards. »


(metalcraft-ish . (metalcraft (text "(if|unless|as long as|while) you control three or more artifacts")))
;;e.g. ‹Inventor's Fair›: « At the beginning of your upkeep, if you control three or more artifacts, you gain 1 life.\n{T}: Add {C}.\n{3}, {T}, Sacrifice ~: Search your library for an artifact card, reveal it, put it into your hand, then shuffle. Activate only if you control three or more artifacts. »


(-ish . ( (text "")))
;;e.g. ‹›: «  »


(-ish . ( (text "")))
;;e.g. ‹›: «  »


;;


(defconst mtg-query-card-keyword-ability-alist )


(devotion . )
(devotion-ish . (devotion + some chroma + etc))


;;


(defconst mtg-query-card-alist )
( . (and (text ) ()))
( . (and (text ) ()))


(defconst mtg-query-powtou-alist )
(square . (= pow tou))
(ferocious . (>= pow 4))


(defconst mtg-query-text-draw-alist )
cantrip . (draw 1)
(draw + n) . (draw-disc (n) (n-2))
(loot n) . (draw-disc +(n) -(n))
(rummage n) . (disc-draw (n) (n))
loot . (draw-disc +1 -1)
sift . (draw-disc +3 -2)


(defconst mtg-query-text-discover-alist )
(discover k /n) . (draw-from (or k 1) (or n 4))))
(impulse . (discover 1 /4))
(anticipate . (discover 1 /3))
(( …) . (discover milling …))
( . (discover  /))


(defconst mtg-query-text-damage-alist )
(zap . (damage 1 any))
(shock . (damage 2 any))
(bolt . (damage 3 any))


(defconst mtg-query-text-counter-alist )
(negate . (counter noncreature))
((force-spike mana-tithe spike tithe) . (counter (unless 1)))
((miscalculate miscalc) . (counter (unless 2)))
((mana-leak leak) . (counter (unless 3)))


(mtg-define-text-query (destroy :struct t :parser nil)
  "“Destroy” actions."
  (count 1 natnum)
  (kind t)
  (compensate nil)
  ( nil)
  ( nil)
  )


;; → (put 'destroy 'mtg-query (list 'mtg-text-query/destroy #'mtg-parse-text-query/destroy))
;; → (cl-defstruct (mtg-text-query/destroy (:type list) :named (:constructor mtg-make-text-query/destroy) (:constructor nil) (:copier nil)) (count 1 :type natnump) (kind t :type t) (compensate nil :type t) "“Destroy” actions.")
;; → (cl-defun mtg-parse-text-query/destroy (&optional (count 1) (kind t) (compensate nil)) "See struct ‘mtg-text-query/destroy’." (mtg-make-text-query/destroy …))


(cl-defun mtg-parse-text-query/destroy (&rest forms)
  "See struct ‘mtg-text-query/destroy’."
  (let ((FORMS forms)
        (COUNT      nil)
        (KIND       nil)
        (COMPENSATE nil)
        )
    (when (cl-member FORMS :test #'natnump)
      (setq COUNT (cl-find FORMS :test #'natnump))
      (setq FORMS (cl-remove FORMS :test #'natnump)))
    (pcase forms
      (`() (mtg-make-text-query/destroy))
      (() (mtg-make-text-query/destroy :kind () :count ()))
      (() (mtg-make-text-query/destroy :kind () :count ()))
      (_ (error "[Unknown %s arguments] %S" #'mtg-make-text-query/destroy forms)))))


(defconst mtg-query-text-destroy-alist '(
  ((destroy ) . ( ))
  (destroy . (destroy ))
  (murder  . (destroy (creature !)))
  (shatter . (destroy (artifact !)))
  ( . ( ))
  ) "Aliases for ‘mtg-text-query/destroy’.")


;; n.b. because `'(destroy noncreature)` implies `'(destroy planeswalker)` (i.e. `'(destroy planeswalker) ≤ '(destroy noncreature)` = `'(destroy (or land creature planeswalker artifact enchantment)` = `'(or (destroy land) (destroy creature) (destroy planeswalker) (destroy artifact) (destroy enchantment)`), the “loose” query `'(text (destroy planeswalker))` matches text with either “destroy target planeswalker” or “destroy target permanent” (ditto “destroy target nonland permanent”, “destroy target noncreature permanent”, etc), while the “strict” query `'(text (destroy planeswalker))` only matches exact text like “destroy target [non…] planeswalker”. likewise, “counter target planeswalker spell”, “counter target creature or planeswalker spell”, “counter target legendary spell” (and so on) all match `'(text (counter planeswalker))`, because the supermajority of planeswalkers are legendary (even though there exist legendary planeswalkers, e.g. ‹The Wanderer›).


(defconst mtg-query-text-bounce-alist )
(bounce . (bounce 1 t))
(rescue . (bounce yours))
(unsummon . (bounce creature))
(boomerang . (bounce permanent))
(remand . (bounce spell))
(venser . (bounce (| spell (? permanent))))  ; ‹Venser, Shaper Savant›


(defconst mtg-query-text-discard-alist )
(coerce . (discard 'choose))
(hymn . (discard 'random))
(mind-rot . (discard 2))


(defconst mtg-query-card-entering-trigger-alist )
(etb . (etb 'when ))


;; e.g. (mtgq (etb)) → (mtgq (etb when))
;; e.g. (etb when ) → "When ~ enters the battlefield, …"
;; e.g. (etb as choose)) → "As ~ enters the battlefield, choose …"
;; e.g. (etb with +1/+1)) → "~ enters the battlefield with …"
(etb . (or (text "When ~ … enters the battlefield…,") (and (text "Whenever (a[n]?|one or more) \\(?:1()+\\) enter[s]? the battlefield [under your control]…, …") (type "\\1"))))


(defconst mtg-query-card-exiting-trigger-alist )
(ltb . (ltb when))


(defconst mtg-query-text-fight-alist )
(fight . (fight yours-theirs))
(bite  . (fight yours))
(  . (fight theirs-itself))  ; ‹Kiku, Night's Flower›
(  . (fight theirs-theirs))
(fight:pump . (and (text fight) (text (or (gets-powtou) (put-counter +1/+1) (gains (or indestructible persist undying protection-from ))))))


(defconst mtg-query-text--alist )
( . ( ))




(defconst mtg-query-text--alist )
( . ( ))


(defconst mtg-query-text--alist )
( . ( ))


(defconst mtg-query-text--alist )
( . ( ))


(defconst mtg-query-text--alist )
( . ( ))


(defconst mtg-query-text--alist )
( . ( ))


(defconst mtg-query-text--alist )
( . ( ))


(defconst mtg-query-text-deathtouch-alist )
(deathtouch-ish . (or deathtouch triggered-deathtouch))
(triggered-deathtouch . (text "Whenever ~ (deals damage to|deals combat damage to|blocks|is blocked by|blocks or is blocked by) a( non[-a-z]+)? creature[, at …], destroy that creature.))


(defconst mtg-query-text-lifelink-alist )
(lifelink-ish . (or lifelink triggered-lifelink))
(triggered-lifelink . (text "Whenever ~ deals [combat] damage [to a (creature|player|planeswalker)], you gain (that much life|POW life)."))


;; (defconst mtg-query-text-xyz-alist )
;; ( . (xyz ))


;;


;; >602.1. Activated abilities have a cost and an effect. They are written as "[Cost]: [Effect.] [Activation instructions (if any).]"
;; >603.1. Triggered abilities have a trigger condition and an effect. They are written as "[When/Whenever/At] [trigger event or trigger condition], [effect]. [Instructions (if any).]"


;; M-: (mtg-query-cards (mtgq (undergrowth (etb (create))))) → '(izoni-thousand-eyed …)
;; Izoni, Thousand-Eyed: “Undergrowth — When ~ enters the battlefield, create a 1/1 black and green Insect creature token for each creature card in your graveyard.”


flying 
high-flying (and flying "~ can block only creatures with flying")
gains-flying (or (activation-effect flying) (trigger-effect flying))
flying-like (or flying horsemanship "~ can't be blocked except by creatures with flying")
shadow
shadow-like (or shadow high-flying)


animates (or mtg-card symbol)
phyrexian-totem . (animates (~ phyrexian-) )


Phyrexian Totem
{3} Artifact
{T}: Add {B}.
{2}{B}: ~ becomes a 5/5 black Phyrexian Horror artifact creature with trample until end of turn. Whenever ~ is dealt damage, if it's a creature, sacrifice that many permanents.


Phyrexian Negator
{2}{B} Creature 5/5 Phyrexian Horror
Trample
Whenever ~ is dealt damage, sacrifice that many permanents.


creates (or mtg-token symbol)
llamowar-mentor . (creates (~ llamowar-elves) )


Llamowar Mentor


Llamowar Elves




;;


________________




* gods:
   * permanents that (1) animate while a condition is satisfied, (2) have some protection.
   * animation can be: "~ isn't a creature unless …" (like the THS Gods); "~ can't attack or block unless …" (like the AKH Gods); .
   * protection can be: "indestructible" (like the THS Gods and AKH Gods); "hexproof" (like the  Gods); self-regrowth (like the HOU Gods); .
   * .
* animateés:
   * permanents that animate.
   * animations can be: temporarily becoming a creature; losing defender; .
   * * 

;;


________________




MTG-RX


;;; ‘mtg-rx’:


(defmacro mtg-define-category (char-name symbol-name docstring &rest chars)
  (cl-type-check char-name 'character)
  (cl-type-check symbol-name 'symbol)
  (cl-type-check docstring 'strong)
  (let ((VAR-NAME (intern (format "mtg-category/%s" (symbol-name symbol-name))))
        (VAR-DOCS (format "See (describe-category ?%c)" char-name)))
    `(progn
       (define-category ,char-name ,docstring)
       (defconst ,VAR-NAME ,chars ,VAR-DOCS)
       )))


(defmacro mtg-define-rx (name args docs &rest body)
  (cl-type-check name 'symbol)
  (cl-type-check args 'list)
  (cl-type-check docs 'string)
  (let ((VAR-NAME (intern (format "mtg-rx/%s" (symbol-name name))))
        (RX-NAME (intern (format "mtg-%s" name))))
    `(progn
       (rx-define ,NAME ,ARGS ,BODY)
       (defconst ,VAR-NAME ,docs)
       ,(unless (or args (memq name ¿rx-builtins?))
          `(add-to-list 'mtg-rx-alist (cons ,name ,RX-NAME)))
       )))


(defvar mtg-rx-alist
  '(
    ;;(word mtg-word)
    (mana-symbol (s) (mtg-mana-symbol s))
   ) "")


;; ‘rx-define’s


;;(mtg-define-rx  ()
;;  ""
;;  ())
;;(rx-define mtg- ()
;;  ())


;;(mtg-define-rx eol
;;  ""
;;  (char ?\n ?;))
;;(rx-define mtg-eol
;;  (char ?\n ?;))


(rx-define mtg-eol
  '(char ?\n ?;))


(rx-define mtg-dash
  ""
  '(or (char ?— ?～) "--"))


(rx-define mtg-point
  ""
  '(char ?• ?*))


(rx-define mtg-comma
  ""
  '(char ?, ?，))


(rx-define mtg-colon
  ""
  '(char ?: ?：))


(rx-define mtg-period
  ""
  '(char ?. ?。))


(rx-define mtg-plus
  ""
  '(char ?+))


(rx-define mtg-minus
  ""
  '(char ?− ?-))


(rx-define mtg-plus-minus
  ""
  '(or mtg-plus mtg-minus (char ?±)))


(rx-define mtg-open-quote
  ""
  '(char ?“ ?« ?„ ?「))


(rx-define mtg-close-quote
  ""
  '(char ?” ?» ?」))


(rx-define mtg-open-close-quote
  ""
  '(or (char ?") mtg-open-quote mtg-close-quote))


(let ((DIGIT-CHARS (cl-loop for CHAR from 128 to (max-char) collect (or (eq 'Nd (get-char-code-property CHAR 'general-category))))))
  (rx-define mtg-digit
    ""
    `(or digit (char ,@DIGIT-CHARS)))) 


;;↑ (cl-loop for CHAR from 0 to (max-char) …)
;;↑ (cl-loop for CHAR across unicode-category-table collect …)
decimal-digit-value
Corresponds to the Unicode Numeric_Value property for characters whose Numeric_Type is ‘Decimal’. The value is an integer. For unassigned codepoints, the value is nil, which means NaN, or “not-a-number”.
digit-value
Corresponds to the Unicode Numeric_Value property for characters whose Numeric_Type is ‘Digit’. The value is an integer. Examples of such characters include compatibility subscript and superscript digits, for which the value is the corresponding number. For unassigned codepoints, the value is nil, which means NaN.
numeric-value
Corresponds to the Unicode Numeric_Value property for characters whose Numeric_Type is ‘Numeric’. The value of this property is a number. Examples of characters that have this property include fractions, subscripts, superscripts, Roman numerals, currency numerators, and encircled numbers. For example, the value of this property for the character U+2155 (VULGAR FRACTION ONE FIFTH) is 0.2. For unassigned codepoints, the value is nil, which means NaN.


(rx-define mtg-roman-numeral
  ""
  '(or (1+ (char ?I ?V ?X))
       (char ?Ⅰ ?Ⅱ ?Ⅲ ?Ⅳ Ⅴ? Ⅵ? Ⅶ? Ⅷ? ?Ⅸ ?Ⅹ ?Ⅺ ?Ⅻ)))


(rx-define mtg-
  ""
  '(char ? ?))


(rx-define mtg-
  ""
  '(char ? ?))


;;(rx-define mtg-
;;  ""
;;  '(char ? ?))


(rx-define mtg-color
  ""
  '(mtg-word (or "white" "blue" "black" "red" "green")))  ;TODO `(mtg-word (or ,@mtg-known-color-names))


(rx-define mtg-basic-land-type
  ""
  '(mtg-word (or "Plains" "Island" "Swamp" "Mountain" "Forest")))  ;TODO `(mtg-word (or ,@mtg-known-basic-land-type-names))


(defconst mtg-known-color-names '("white" "blue" "black" "red" "green"))
(defconst mtg-known-basic-land-type-names '("Plains" "Island" "Swamp" "Mountain" "Forest"))


(rx-define mtg-
  ""
  '(mtg-word (or "" "" "" "" "")))


(rx-define mtg-trigger-word
  ""
  '(and bos (mtg-word (or "When" "Whenever" "At"))))


(rx-define mtg-replacement-before-word
  ""
  '(and bos (mtg-word (or "If"))))


(rx-define mtg-replacement-subjunctive-word
  ""
  '(mtg-word (or "would")))


(rx-define mtg-replacement-after-word
  ""
  '(mtg-word (or "instead")))


(rx-define mtg-color-word
  ""
  '(and bos (mtg-word (or "If"))))


;;(rx-define mtg-
;;  ""
;;  '(mtg-word (or "" "")))


(rx-define mtg- ()
  ""
  `())


(rx-define mtg-positive-integer
  ""
  `(: (char (?1 . ?9))
      (0+ (char (?0 . ?9)))))


(rx-define mtg-symbol (sym)
  ""
  `(and (char ?{) ,sym (char ?})))


(rx-define mtg-roman-symbol ()
  ""
  `(or mtg-roman-numeral (mtg-symbol (: ?r mtg-positive-integer)))


(rx-define mtg-saga-bol ()
  ""
  `(mtg-roman-symbol mtg-dash))


(rx-define mtg-ability-word-bol ()
  ""
  `(mtg-name (1+ blank) mtg-dash (1+ blank)))


(rx-define mtg-word (str)
  ""
  `(and bow ,str eow))


(rx-define mtg-powtou (pow tou)
  "Match “POW/TOU”."
  `(and ,pow (char ?/) ,tou))


(rx-define mtg-plusminus-powtou (pow tou pow-sign &optional tou-sign)
  "Match “POW/TOU” (with POW-SIGN and/or TOU-SIGN)."
  (let* ((POW-SIGN pow-sign)
         (TOU-SIGN (or tou-sign POW-SIGN)))
    `(and ,POW-SIGN ,pow (char ?/) ,TOU-SIGN ,tou)))


;;e.g. variable-flowstone effects (“+X/-Y”): (rx (mtgmtg-plusminus-pow (char ?X ?Y) (char ?X ?Y) (char ?+) (char ?-)))


;; (rx-to-string `(and ))


(rx-define mtg-mana-symbol (sym)
  ""
  `(and ))


(rx-define mtg-loyalty-symbol (sign num)
  ""
  `(mtg-symbol ,sign ,num))


(rx-define mtg-plus-loyalty-symbol (num)
  ""
  `(mtg-symbol mtg-plus ,num))


(rx-define mtg-minus-loyalty-symbol (num)
  ""
  `(mtg-symbol mtg-minus ,num))


(rx-define mtg-trigger-line
  ""
  '(and mtg-trigger-word mtg-phrase (char ?,) mtg-phrase (char ?.)))


(rx-define mtg-replacement-line
  ""
  '(and mtg-replacement-before-word mtg-phrase mtg-replacement-subjunctive-word mtg-phrase (char ?,) mtg-replacement-after-word mtg-phrase (char ?.)))


(rx-define mtg-
  ""
  `(or ()
       ()))


(rx-define mtg-symbolic
  ""
  `(1+ ()))


(rx-define mtg-number
  ""
  `(or (1+ mtg-digit)
       (1+ mtg-digit)))


(rx-define mtg-natural
  ""
  `(1+ mtg-digit))


(rx-define mtg-integer
  ""
  `(or (1+ (char ?0))
       (and mtg-plus-minus (1+ digit))))


(rx-define mtg-numeric
  ""
  `(or mtg-natural-numeric
       mtg-symbolic))


(rx-define mtg-arithmetic
  ""
  `(or mtg-numeric
       ()
       ()))


(rx-define mtg-any-mana-cost
  ""
  `(1+ mtg-any-mana-symbol))


(rx-define mtg-any-powtou
  "Match any “X/Y”."
  `(mtg-powtou mtg-arithmetic mtg-arithmetic))


(rx-define mtg-any-plusminus-powtou
  "Match any “±X/±Y”."
  `(mtg-plusminus-powtou mtg-natural mtg-natural mtg-plusminus mtg-plusminus))


(rx-define mtg-any-symbol
  ""
  `(mtg-symbol or mtg-numeric))


(rx-define mtg-empty-mana-cost
  ""
  "{}")


(rx-define mtg-any-loyalty-symbol
  ""
  `(mtg-symbol mtg-plus-minus mtg-numeric))


(rx-define mtg-any-
  ""
  `())


;;


(defconst mtg-comma-rx (rx mtg-comma))
(defconst mtg-colon-rx (rx mtg-colon))
(defconst mtg-period-rx (rx mtg-period))
(defconst mtg-eol-rx (rx mtg-eil))


;;


(mtg-define-category ?\n eol
  ?\n ?;)


;;(mtg-define-category ()
;;  ())


(defun mtg-category-init (&optional table)
  (let ((TABLE (or table (make-category-table))))
    (cl-loop for CHAR in mtg-category/eol
      (modify-category-entry CHAR ?\n TABLE))
    TABLE))


;;


(defmacro mtg-rx (&rest forms)
  `(rx-let ,@mtg-rx-alist))
    (rx ,@forms)))


(defun mtg-rx-to-string (&rest forms)
  (rx-let-eval mtg-rx-alist
    (apply #'rx-to-string forms)))


;;


________________




MTG-BNF


;;; ‘mtg-bnf’:


;;


________________




MTG-PCASE


;;; ‘mtg-pcase’:


;; ‘pcase-defmacro’s


(pcase-defmacro mtg-has-color (color)
  "Matches if ‘mtg-card’ EXPVAL has color COLOR."
  `(app mtg-card-colors (pred (memq ,color))))


(pcase-defmacro mtg-has-type (type)
  "Matches if ‘mtg-card’ EXPVAL has type TYPE."
  `(app mtg-card-types (pred (memq ,type))))


;; (pcase CARD ((mtg-has-type TYPE) …)) → (pcase CARD ((app mtg-card-types (pred (memq TYPE))) …)) → (cond ((memq TYPE (mtg-card-types CARD)) …))


(pcase-defmacro mtg-has-color (color)
  "Matches if ‘mtg-card’ EXPVAL has color COLOR."
  `(app mtg-card-colors (pred (memq ,color))))


(pcase-defmacro mtg-has-name (name)
  "Matches if ‘mtg-card’ EXPVAL's name matches regexp NAME."
  `(app mtg-card-name (rx ,name)))


;;(pcase-defmacro mtg- ()
;;  "Matches if EXPVAL ."
;;  `(and ()
;;        (pred ( ,))))


(pcase-defmacro mtg-immediate-loyalty-ability ()
  "Matches if CARD has a loyalty ability that's immediately activatable.
i.e. that's either (1) plus-loyalty, or (2) minus-loyalty with loyalty-cost less than or equal to base-loyalty."
  `(and (mtg-has-type 'planeswalker)
        ()
        (pred ( ,))))


(pcase ()
  ((mtg- ) ))


;;


________________




MTG-CARD-SYNTAX


;;; ‘mtg-card-syntax’:


;;


(defun mtg-parse-planeswalker-text (lines)
  " LINES."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


(defun mtg-parse-line (line)
  " LINE."
  (declare (pure t) (side-effect-free t))
  (let* ((RULES-LINE (mtg-strip-blank (mtg-strip-reminder-text line))))
  (pcase RULES-LINE


    (`(rx (and (not (char ?.)) ?)) (0+ blank) eos) (mtg-parse-keyword-abilities line)))
    (`(rx bos mtg-loyalty-cost) (mtg-parse-loyalty-ability line))
    (`(rx bos mtg-activation-cost ?:) (mtg-parse-activated-ability line)))
    (`(rx bos mtg-trigger-word) (mtg-parse-triggered-ability line)))
    (`(rx bos mtg-replacement-word) (mtg-parse-replacement-effect line)))


;;    (`(rx bos mtg-) (mtg-parse- line))
    (_ line))))


(defun mtg-strip-reminder-text (text)
  "Strip parenthesized text from TEXT."
  ( text))


(defun mtg-strip-blank (text)
  "Strip trailing/duplicate spaces from TEXT."
  ( text))


(defun mtg-parse-activated-ability (line)
  "Parse activated ability LINE into an ‘mtg-activated-ability’."
  (declare (pure t) (side-effect-free t))
  (pcase-let* (`(,COST-TEXT . ,EFFECT-TEXT)
                 (mtg-parse-activated-ability--split line))
    (let* ((COST   (mtg-parse-mtg-activation-cost COST-TEXT))
           (EFFECT (mtg-parse-mtg-activation-effect EFFECT-TEXT)))
      (mtg-make-activated-ability :cost COST :effect EFFECT))))


;;^
;; e.g. ‹Sinister Concoction›:
;;     M-: (mtg-parse-mtg-activation-cost "{B}, Pay 1 life, Mill a card, Discard a card, Sacrifice ~: Destroy target creature.")
;;    ↳ (mtg-make-activated-ability :cost '([b] (life 1) (mill 1) (discard (t 1)) (sacrifice ~)) :effect "Destroy target creature.")
;; ;; "{B}, Pay 1 life, Mill a card, Discard a card, Sacrifice ~: …" → '("{B}" "Pay 1 life" "Mill a card" "Discard a card" "Sacrifice ~") → '((mana [b]) (life 1) (mill 1) (discard (t 1)) (sacrifice ~))
;;
;; e.g. ‹Llanowar Mentor›:
;;     M-: (mtg-parse-activated-ability "{G}, {T}, Discard a card: Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”"
;;     ↳ (mtg-make-activated-ability :cost '([g] [t] (discard (* 1)) :effect "Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”" :raw "{G}, {T}, Discard a card: Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”")
;;
;; 


(defun mtg-parse-triggered-ability (line)
  " LINE."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


(defun mtg-parse-replacement-effect (line)
  " LINE."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


(defun mtg-parse-loyalty-ability (line)
  " LINE."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


;; [ "Creatures you control get +1/+0." "[+1]: Add {R} or {G}. Creature spells you cast this turn can't be countered." "[−2]: Target creature you control fights target creature you don't control." ] → '("Creatures you control get +1/+0." (loyalty-ability [+ 1] "Add {R} or {G}. Creature spells you cast this turn can't be countered.") (loyalty-ability [- 2] "Target creature you control fights target creature you don't control."))
;; ↑ (cadr (assq 'loyalty-ability …))


;;Domri, Anarch of Bolas
{1}{R}{G} Legendary Planeswalker [3] Domri
;;Creatures you control get +1/+0.
;;[+1]: Add {R} or {G}. Creature spells you cast this turn can't be countered.
;;[−2]: Target creature you control fights target creature you don't control.


(defun mtg-parse-keyword-abilities (line)
  " LINE."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


;;


(defun mtg-parse-activated-ability--split (line)
  "Split LINE on the first ‘mtg-colon-rx’."
  (when-let* ((COLON-INDEX (save-match-data
                             (string-match (rx mtg-colon) line))))
    (cons (string-trim (substring line 0 COLON-INDEX))
          (string-trim (substring line (1+ COLON-INDEX) nil)))))


;;^ c.f. (split-string ":" …): ours only splits first.


(defun mtg-parse-activation-cost (text)
  "Parse TEXT into comma-separated ‘mtg-cost-p’s."
  (declare (pure t))
  (let* ((COST-TEXTS (split-string mtg-comma-rx text 'do-omit-nulls 'do-trim-blanks)))
    (cl-loop for TEXT in COST-TEXTS
      collect (mtg-parse-cost TEXT))))


;;^
;; e.g. ‹Llanowar Mentor›:
;;     M-: (mtg-parse-activation-cost "{G}, {T}, Discard a card")
;;     ↳ (mtg-make-activation-cost '([g] [t] (discard (* 1)))
;;


(defun mtg-parse-cost (text)
  "Parse TEXT into an ‘mtg-cost-p’.
Can parse mana costs, self–tapping/exerting/rescuing/sacrificing/etc, life/mill/cremate/discard/sacrifice/etc."
  (declare (pure t) (side-effect-free t)
  (pcase text


    (`(rx bos (mtg-word "Discard")) (mtg-parse-discard-cost text))
    (`(rx bos (mtg-word "Sacrifice")) (mtg-parse-sacrifice-cost text))
    (`(rx bos (mtg-word "Tap")) (mtg-parse-tap-cost text))
    (`(rx bos (mtg-word "Mill")) (mtg-parse-mill-cost text))
    (`(rx bos (mtg-word "Exile") mtg-phrase (mtg-word "graveyard") eos) (mtg-parse-cremate-cost text))
    (`(rx bos (mtg-word "Return") mtg-phrase (mtg-word "hard") eos) (mtg-parse-rescue-cost text))
    (`(rx bos (mtg-word "")) (mtg-parse--cost text))
    (`(rx bos (mtg-word "")) (mtg-parse--cost text))
    (`(rx bos (mtg-word "")) (mtg-parse--cost text))


;;    (`(rx bos (mtg-word "")) (mtg-parse--cost text))
    (_ text)))


;;^
;;
;; e.g. paying life:
;;     M-: (mtg-parse-cost "Pay 1 life")
;;     ↳ (mtg-make-life-cost :count 1)
;;     ↳ (mtg-make-cost '(life 1)
;;
;; e.g. milling:
;;     M-: (mtg-parse-cost "Mill a card")
;;     ↳ (mtg-make-mill-cost :count 1)
;;     ↳ (mtg-make-cost '(mill 1))
;;
;; e.g. discarding:
;;     M-: (mtg-parse-cost "Discard a card")
;;     ↳ (mtg-make-discard-cost :count 1 :kind t)
;;     ↳ (mtg-make-cost '(discard t 1))
;;
;; e.g. cremating:
;;     M-: (mtg-parse-cost "Exile a card from your graveyard")
;;     ↳ (mtg-make-cremate-cost :count 1 :kind t)
;;     ↳ (mtg-make-cost '(cremate t 1))
;;
;; e.g. sacrificing:
;;     M-: (mtg-parse-cost "Sacrifice a Mountain")
;;     ↳ (mtg-make-sacrifice-cost :count 1 :kind 'mountain)
;;     ↳ (mtg-make-cost '(sacrifice '(:type (mountain))) 1))
;;
;; e.g. rescuing:
;;     M-: (mtg-parse-cost "Return an Island you control to its owner's hand")
;;     ↳ (mtg-make-rescue-cost :count 1 :kind 'island)
;;     ↳ (mtg-make-cost '(rescue '(:type '(island))) 1))
;;
;;


(defun mtg-parse-actions (text)
  "Parse TEXT ."
  (declare (pure t))
  (let* (( ())
         ( ()))
    ()))


(defalias 'mtg-parse-activation-effect #'mtg-parse-actions)


;;^
;; e.g. ‹Llanowar Mentor›:
;;     M-: (mtg-parse-actions "Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”"
;;     ↳ (list (mtg-make-action :actions (list (mtg-make-create-action :count 1 :token (mtg-make-token :pow 1 :tou 1 :colors '(g) :cardtypes '(creature) :subtypes '(elf druid) :name "Llanowar Elves" :abilities '("{T}: Add {G}.")))) :raw "Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”"))
;;


(defun mtg-parse-action (text)
  "Parse TEXT as ."
  (declare (pure t) (side-effect-free t)
  (pcase text


    (`(rx bos (mtg-word "Create")) (mtg-parse-create-act text))
    (`(rx bos (mtg-word "")) (mtg-parse--act text))
    (`(rx bos (mtg-word "")) (mtg-parse--act text))
    (`(rx bos (mtg-word "")) (mtg-parse--act text))
    (`(rx bos (mtg-word "")) (mtg-parse--act text))
    (`(rx bos (mtg-word "")) (mtg-parse--act text))


;;    (`(rx bos (mtg-word "")) (mtg-parse--act text))
    (_ text)))


(defun mtg-parse-create-act (text)
  "Parse TEXT as a “Create … token(s)” ‘mtg-create-act-p’.
“Create <n> <pow>/<tou> <colors> <sub-types> <card-types> token(s) [with <keyword-abilities>]. [It (They) has (have) <abilities>.]”.
Can parse both “with” abilities (in the same sentence) and “It has”/“They have” abilities (in the next sentence), known token kinds, optional ”named” and/or “legendary”, and more."
  (declare (pure t))


  (let* (( ())
         ( ()))
    ()))


;; “Create [<name>,] <n> <pow>/<tou> <colors> <sub-types> <card-types> token(s) named <name> [with <keyword-abilities>]. [It (They) has (have) <abilities>.]”
;; ↓
;; "Create (? mtg-any-card-name) mtg-any-number-word (? mtg-any-super-types) (? mtg-any-powtou) mtg-any-color-words (? mtg-any-sub-types) mtg-any-card-types (mtg-plural-word "token") (? "with" mtg-any-keywords) "." (? " " " (| "It has" "They have") mtg-any-abilities "."))
;; ↓
;; 


(rx-define mtg-any-number-word
  `(or "a" "an" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "X" "Y" "Z" "an amount of"))


(rx-define mtg-any-card-name
  `(mtg-word ))  ; capitalized


(rx-define mtg-any-color-word
  `(mtg-word mtg-any-color))


(rx-define mtg-any-color-words
  `(mtg-ands mtg-any-color-word)))


;;^ e.g. "blue" & "white and blue" & "white, blue, and black" & "colorless" all match.


(rx-define mtg-any-sub-types
  `(mtg-spaces mtg-any-sub-type)))


(rx-define mtg-any-sub-type
  `( ))
;;^ sub-types (creature-types, artifact-types, etc) are capitalized.


(rx-define mtg-any-card-types
  `(mtg-spaces mtg-any-card-type)))


(rx-define mtg-any-card-type
  `( capitalized ))
;;^ card-types & super-types are downcased.


(rx-define mtg-any-keywords
  `(mtg-ands mtg-any-keyword)))


(rx-define mtg-any-keywords
  `(mtg-word mtg-any-keyword))


(rx-define mtg-any-abilities
  `(or (mtg-ands (* mtg-any-keyword)
                 (* mtg-any-quoted-ability)))


(rx-define mtg-any-ability
  `(mtg-word ))  ; lowercase


(rx-define mtg-any-quoted-ability
  `(: mtg-open-quote (mtg-phrase mtg-close-quote) mtg-close-quote))


(rx-define mtg-phrase (&rest end-rx)
  `(1+ (not (or ,@end-rx))))


(rx-define mtg-spaces (x)
  `(: ,x
      (* (: " " ,x))
      ))


(rx-define mtg-ands (x &optional min max)
  `(: ,x
      (* (: ", " ,x))
      (? (: " and " ,x))
      ))


;;1 (| (: s) (: s " and " s) (: s ", " s ", and " s) …)
;;2 (: s (* (: ", " s)) (? (: " and " s)))


(rx-define mtg-ors (words &optional min max)
  `( )))


;;^
;; e.g. ‹Depose // Deploy›:
;;     M-: (mtg-parse-create-action "Create two 1/1 colorless Thopter artifact creature tokens with flying."
;;     ↳ (mtg-make-create-act :count 2 :token (mtg-make-token :pow 1 :tou 1 :colors '() :cardtypes '(artifact creature) :subtypes '(thopter) :abilities '(flying) :raw "Create two 1/1 colorless Thopter artifact creature tokens with flying.”")
;;     ↳ (mtg-make-create-action :count 2 :token 'thopter)
;;
;; e.g. ‹Llanowar Mentor›:
;;     M-: (mtg-parse-create-act "Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”"
;;     ↳ (mtg-make-create-action :count 1 :token (mtg-make-token :pow 1 :tou 1 :colors '(g) :cardtypes '(creature) :subtypes '(elf druid) :name "Llanowar Elves" :abilities '("{T}: Add {G}.")))) :raw "Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”")
;;


(defun mtg-parse-mtg-cost (text)
  "Parse TEXT."
  (declare (pure t) (side-effect-free t))
  (let* (( ())
         ( ()))
    ()))


;;


________________




________________




MTG-QUERY-SYNTAX


;;; ‘mtg-query-syntax’:


;;


(defconst mtg-card-query-verb-alist
  '(


    (+    . color)
    (++   . color-identity)
    (+++  . color-personality)
    (#+   . color-count)


    (*    . type)
    (**   . type-identity)
    (***  . type-)
    (#*   . type-count)
    (#*_  . subtype-count)
    (#*-  . cardtype-count)
    (#*^  . supertype-count)


    ($    . mana-cost)
    (#$   . mana-value)
    ($$   . nonmana-cost)
    ($$$  . smart-costs)


    ('    . name)
    (''   . face-name)
    ('''  . -)
    (#'   . face-count)


    (@    . rules-text)
    (@@   . rules-keywords)
    (@@@  . -)
    (~@   . loose-rules-text)
    (~@@  . loose-rules-keywords)
    (#@   . rules-line-count)
    (#@@  . rules-keyword-count)


    (/   . powtou)
    (!/  . base-powtou)
    (~/  . loose-powtou)


    (&    . )
    (&&   . -)
    (&&&  . -)


    (%    . )
    (%%   . -)
    (%%%  . -)


    (^    . ¿frame?)
    (^^   . -)
    (^^^  . -)


    (:    . edition-set)
    (::   . edition-rarity)
    (:::  . edition-block)


    (`    . release-date)
    (``   . original-release-date)
    (```  . -date)
    (#`   . reprint-count)


    (`* . original-type)
    (`@ . original-text)
    (`% . original-release-date)


    (;   . format-legality)
    (#;  . -count)


;;    (    . )
;;    (   . -)
;;    (  . -)


   ) "")


;; :
;;
;; • (#… . …-count)    — 
;; • (!… . strict-…)   — 
;; • (~… . loose-…)    — 
;; • (`… . original-…) — 
;; • (… . -…) — 
;;
;; (#…) — e.g.
;;
;; (!…) — e.g. ‘!3/’ means “has base power 3” (‘3/’ means “has power 3”, including “~ enters the battlefield with three +1/+1 counters on it”, “Modular 3”, “Graft 3”, etc).
;;
;; (~…) — e.g. ‘~3/’ means “can have power 3”: including a “{X}{G}” with “~ enters the battlefield with X +1/+1 counters on it” at X=3 (e.g. ‹ Hydra›); including a “*/*” with “As ~ enters the battlefield, choose 3/3, flying 1/1, or defender 0/8. ~ has the chosen ability and base power and toughness” (e.g. ‹Shapeshifter›). e.g. ‘~@@flying’ means “can gain flying”: including “{U}: ~ gains flying until end of turn.” (e.g. ‹Dukhara Peafowl›), c.f. ‘~@flying’ which means “has the keyword flying”; including, .
;;
;; (`…) — e.g.
;;
;; (…) — e.g.
;; 
;; e.g. « 1≤#$≤3 » → « #$1<=_<=3 » → « #$>=1,<=3 » → « #$>=1 && #$<=3 » → « (and (mana-value (>= 1)) (mana-value (<= 3))) » → « (mtg-with-card CARD (and (>= mana-value 1) (>= mana-value 3))) »
;; 


;; Logic Queries:
;; - unification:
;;     - e.g. `@[draw <n=NCARDS> cards] @[lose <n=NLIFE> life] (= NCARDS NLIFE)`, a.k.a. `(mtg-lambda ((mtg-natural NCARDS) (mtg-natural NLIFE)) (and (text (mtg-concat "draw " NCARDS " cards") (mtg-concat "lose " NLIFE " life")) (= NCARDS NLIFE)))`, matches `"Draw three cards, then lose 3 life."`, but not `"Draw two cards, then lose 3 life."` or `"Draw three cards, then lose 4 life."`; c.f. `@[draw <n> cards] @[lose <n> life]`, which will match card draw and life loss with different numbers.
;;     - e.g. 
;; 


;; :
;;     - e.g. 
;; 


is functional reprint
- everything but the name is the same
- type can differ


is colorshift:
- color personality can differ (e.g. See Serpent to Bog Serpent)


:
`(lambda (n) (and (> n 0) (/ (-+ n) (- n))))  ;→ -1/-1 +1/-1 -2/-2 +2/-2 -3/-3 +3/-3 …
(/ (+- (1 3)) (-+ (0 4)))


"[+](<n>|[XY])[/][+](<n>|[XY])"


;;


(defun mtg-parse-card-query-string (string)
  ""
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (mtg-query-mode +1)
    (mtg-parse-card-query-buffer )
    (buffer-substring (point-min) (point-max))))


;;M-: (mtg-parse-card-query "+u ++rg")
;;    '((color . (u)) (color-identity . (r g)))


;;M-: (mtg-parse-card-query "+u ++rg *p,c **i;dragon $$≥3 /3 @@flying || ")
;;    '()


(defun mtg-parse-card-query-buffer (&optional buffer)
  ""
  ())


(skip-syntax-forward "'" "\n")


;;


(define-derived-mode mtg-query-mode 'prog-mode
  "mtg-query-mode" "Major mode for parsing M:tG queries."
  (mtg-query-mode-init))


(defun mtg-query-mode-init ()
  "Initialize ‘mtg-query-mode’."
  (set-syntax-table mtg-query-mode-syntax-table)
  (set-category-table mtg-query-mode-category-table)
  (use-local-map mtg-query-mode-map)
  (setq-local comment-start "//")
  (setq-local eldoc-documentation-function #'mtg-query-mode-eldoc)
  ())


(defconst mtg-query-mode-syntax-table
  )


(defconst mtg-query-mode-syntax-table
  (let ((TABLE (make-syntax-table)))


    (modify-syntax-entry ?\ "" TABLE)
    (modify-syntax-entry ?\ "" TABLE)
    (modify-syntax-entry ?\ "" TABLE)
    (modify-syntax-entry ?\` ". p" TABLE)
    (modify-syntax-entry ?\~ ". p" TABLE)
    (modify-syntax-entry ?\! "'" TABLE)
    (modify-syntax-entry ?\? ". p" TABLE)
;;    (modify-syntax-entry ?\@ "$" TABLE)
    (modify-syntax-entry ?\@ "'" TABLE)
    (modify-syntax-entry ?\# "'" TABLE)
    (modify-syntax-entry ?\$ "'" TABLE)
    (modify-syntax-entry ?\% "'" TABLE)
    (modify-syntax-entry ?\^ "'" TABLE)
    (modify-syntax-entry ?\& "'" TABLE)
    (modify-syntax-entry ?\* "'" TABLE)
    (modify-syntax-entry ?\- "w p" TABLE)
    (modify-syntax-entry ?\+ "'" TABLE)
    (modify-syntax-entry ?\/ "w p" TABLE)
    (modify-syntax-entry ?\' "w p" TABLE)
    (modify-syntax-entry ?\, "w p" TABLE)
    (modify-syntax-entry ?\; "w p" TABLE)
    (modify-syntax-entry ?\: "'" TABLE)
    (modify-syntax-entry ?\| "'" TABLE)
    (modify-syntax-entry ?\= "." TABLE)
    (modify-syntax-entry ?\< "." TABLE)
    (modify-syntax-entry ?\> "." TABLE)
    (modify-syntax-entry ?\ "_ p" TABLE)
    (modify-syntax-entry ?\{ "(}" TABLE)
    (modify-syntax-entry ?\} "){" TABLE)
    (modify-syntax-entry ?\[ "(]" TABLE)
    (modify-syntax-entry ?\] ")[" TABLE)
    (modify-syntax-entry ?\( "()" TABLE)
    (modify-syntax-entry ?\( ")(" TABLE)
    (modify-syntax-entry ?\“ "(”" TABLE)
    (modify-syntax-entry ?\” ")“" TABLE)
;;    (modify-syntax-entry ?\ "(" TABLE)
;;    (modify-syntax-entry ?\ ")" TABLE)
    (modify-syntax-entry ?\# "<" TABLE)
    (modify-syntax-entry ?\n ">" TABLE)
    (modify-syntax-entry ?\/ ".n14" TABLE)
    (modify-syntax-entry ?\* ".n23" TABLE)
;;    (modify-syntax-entry ?\ "" TABLE)


    (identity TABLE))
  "Syntax-Table for ‘mtg-query-mode’.")


;;>p identifies an additional prefix character for Lisp syntax. These characters are treated as whitespace when they appear between expressions. When they appear within an expression, they are handled according to their usual syntax classes.


(defconst mtg-query-mode-category-table
  (let ((TABLE (make-category-table)))


    (define-category ?\ "…" TABLE)
;;    (define-category ?\ "…" TABLE)


    (modify-category-entry ?\ ?\ TABLE)
    (modify-category-entry (?\ . ?\) ?\ TABLE)
;;    (modify-category-entry (?\ . ?\) ?\ TABLE nil)
    TABLE)
  "Category-Table for ‘mtg-query-mode’.")


;;(modify-category-entry CHARACTER CATEGORY TABLE RESET-P)


(defconst mtg-query-mode-map
  (let ((MAP (make-sparse-keymap)))


    (define-key MAP (kbd "") #')
    (define-key MAP (kbd "") #')
    (define-key MAP (kbd "") #')
    (define-key MAP (kbd "") #')
    (define-key MAP (kbd "") #')
    (define-key MAP (kbd "") #')


    (identity MAP))
  "Keymap for ‘mtg-query-mode’.")


;;


________________




MTG-RESULTS


;;; ‘mtg-results’:


;;


rarity-as-code  ; c u r m
rarity-as-name  ; common uncommon rare mythic
rarity-as-char  ; 🏅 🥈 🥇 🥇
rarity-and-edition-as-icon  ; (mtg-edition-svg 'tsp 'c) (mtg-edition-svg 'tsp 'u) (mtg-edition-svg 'tsp 'r) (mtg-edition-svg 'tsp 't)


(defvar mtg-svg/edition/tsp-r-)


(defun mtg-edition-svg (edition-id rarity-id &optional width height)
  "Return image EDITION-ID with color RARITY-ID and size WIDTH×HEIGHT.
Caches."
  (declare (pure t))
  (let* ((IMAGE-ID (intern (if (and width height) (format "mtg-svg/edition/%s-%s-%sx%s" edition-id rarity-id width height) (format "mtg-svg/edition/%s-%s" edition-id rarity-id))))
         (IMAGE (symbol-value IMAGE-ID)))
    (if IMAGE
        IMAGE
      (let* ((IMAGE ()))
        (set IMAGE-ID IMAGE)
        IMAGE))))


;;


________________




MTG-EVAL


;;; ‘mtg-eval’:


(mtg-eval )


;;e.g.:
- “Spellstutter Sprite”: at least `When ~ enters the battlefield, counter target spell with mana value 1 or less.` (≤ `Creature — Faerie` + `When ~ enters the battlefield, counter target spell with mana value X or less, where X is the number of Faeries you control.`), unless they kill it.
- “Gray Merchant of Asphodel”: at least `When ~ enters the battlefield, each opponent loses 2 life and you gain 2 life.` (≤ `{_}{B}{B}` + `When ~ enters the battlefield, each opponent loses X life, where X is your devotion to black. You gain life equal to the life lost this way.`), unless they kill it.
- “Geist-Honored Monk”: at least a `3/3` (`*/* Creature` + `~'s power and toughness are each equal to the number of creatures you control.` + `When ~ enters the battlefield, create two 1/1 white Spirit creature tokens with flying.`).
- “Fear of Death”: at least `Enchanted creature gets -2/-0.` (`Enchanted creature gets -X/-0, where X is the number of cards in your graveyard.` + `When ~ enters the battlefield, mill two cards.`), unless your graveyard is cremated.
- “”: at least `` (`` + `When ~ enters the battlefield, `).
- “”: at least `` (`` + `When ~ enters the battlefield, `).
;;


________________




DEFSTRUCT


;;


(cl-defstruct (mtg-card (:constructor mtg-make-card) (:constructor nil) (:copier nil))
  "a (physical) “Magic: The Gathering” card, with one-or-more faces."
  (id :type symbol) (name :type string) layout faces value colors editions
  (online-only-p nil :type boolean)
  (can-be-offline-p nil :type boolean)
  (can-be-blackborder-p nil :type boolean)
  (cares-about-graveyard-order-p nil :type boolean)
  (-p nil :type boolean)
  (-p nil :type boolean)
  (-p nil :type boolean)
  (-p nil :type boolean)
  (-p nil :type boolean)
  (-p nil :type boolean)
;;  (-p nil :type boolean)
  )


;;^
;; • (make-mtg-card :online-only-p t …)  ; from 'past (the “Astral Cards” set, whose cards like “Faerie Dragon” have text like “{1}{G}{G}: Play a random effect.”) or 'j21 (the “Jumpstart: Historic Horizons” set, whose cards like “Reckless Ringleader” have text like “When ~ enters the battlefield, choose a creature card in your hand. It perpetually gains haste.”). 
;; • (make-mtg-card :can-be-offline-p t …)  ; cards from online-only sets that could be printed/played offline (i.e. don't have digital effects, like “perpetually”); e.g. “” from “”.
;; • (make-mtg-card :can-be-blackborder-p t …)  ; cards from silver-bordered sets that could be black-bordered (i.e. don't have silly effects, like dexterity); e.g. “The Cheese Stands Alone” from “Unglued”, which became “Barren Glory”.
;; • (make-mtg-card :cares-about-graveyard-order-p t …)  ; e.g. “Shallow Grave” (“Put the top creature card of your graveyard onto the battlefield.”) from “Mirage”.
;; • (make-mtg-card :-p t …)  ; 
;; • (make-mtg-card :-p t …)  ; 
;; • (make-mtg-card :-p t …)  ; 
;; • (make-mtg-card :-p t …)  ; 
;; • (make-mtg-card :-p t …)  ; 
;; • (make-mtg-card :-p t …)  ; 
;; 


(cl-defstruct (mtg-card-face (:constructor mtg-mtg-card-face) (:constructor nil) (:copier nil))
  "a unique “Magic: The Gathering” card face (cards have one-or-more faces)."
  (id :type symbol) (name :type string) layout
  cost value colors color-indicator super-types card-types sub-types power toughness loyalty rules-text frame-layout frame-sides raw-cost raw-types raw-text raw-size props tags multiverse-id scryfall-id)


(cl-defstruct (mtg-card-print (:inherit mtg-card-face) (:constructor mtg-make-card-print) (:constructor nil) (:copier nil))
  "a printed “Magic: The Gathering” card printing, i.e. aesthetics-too."
  (id nil :type symbol) (name nil :type string) edition rarity flavor-text ccn artist border frame artwork printings variations foreign original props tags hasbro scryfall mtgjson)


(cl-defstruct (mtg-creature-card (:inherit mtg-card-face) (:constructor mtg-make-creature-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” creature card face."
  (pow nil :type (or integer symbol)))
  (tou nil :type (or integer symbol)))


(cl-defstruct (mtg-planeswalker-card (:inherit mtg-card-face) (:constructor mtg-make-planeswalker-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering”  card."
  (loy nil :type (or natnum symbol)))


(cl-defstruct (mtg-split-card (:inherit mtg-card) (:constructor mtg-make-split-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” split card."
  (left  nil :type mtg-card-face)
  (right nil :type mtg-card-face)


(cl-defstruct (mtg-dfc-card (:inherit mtg-card) (:constructor mtg-make-dfc-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” double-faced (transformable) card."
  (front nil :type mtg-card-face)
  (back  nil :type mtg-card-face))


(cl-defstruct (mtg-flip-card (:inherit mtg-card-face) (:constructor mtg-make-flip-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” flip card."
  (rightside-up nil :type mtg-card-face)
  (upside-down  nil :type mtg-card-face))


(cl-defstruct (mtg-leveler-card (:inherit mtg-card-face) (:constructor mtg-make-leveler-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” Level-Up card."
  (levels nil :type list))


(cl-defstruct (mtg-saga-card (:inherit mtg-card-face) (:constructor mtg-make-saga-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” Saga card."
  (chapters nil :type list))


(cl-defstruct (mtg-class-card (:inherit mtg-card) (:constructor mtg-make-class-card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering” Class card."
  (chapters nil :type list))


(cl-defstruct (mtg--card (:inherit mtg-card-face) (:constructor mtg-make--card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering”  card."
  ( nil :type ))


(cl-defstruct (mtg--card (:inherit mtg-card-face) (:constructor mtg-make--card) (:constructor nil) (:copier nil))
  "a “Magic: The Gathering”  card."
  ( nil :type ))


;;(cl-defstruct (mtg--card (:inherit mtg-card-face) (:constructor mtg-make--card) (:constructor nil) (:copier nil))
;;  "a “Magic: The Gathering”  card."
;;  ( nil :type ))


(defun mtg-card-get (prop struct)
  ""
  (or (cl-struct-slot-value 'mtg-card prop struct)
      (plist-get prop (mtg-card-props card))))
(put 'mtg-card-get 'byte-optimizer 'byte-compile-inline-expand)


(defun mtg-card-set (prop value struct)
  ""
  (setf (cl-struct-slot-value 'mtg-card prop struct) value)
(put 'mtg-card-set 'byte-optimizer 'byte-compile-inline-expand)


;;


(cl-defstruct (mtg-rules-text)
  keywords keyword-abilities activated-abilities triggered-abilities static-abilities spell-abilities lines orig raw)


;;(cl-defstruct (mtg-rules-text)
;;  keywords abilities lines orig raw)
;;(cl-defstruct (mtg-abilities)
;;  keyworded activated triggered static spell)


;;^ :: Set MtgKeyword (i.e. listp symbolp)
;; raw :: String (i.e. stringp)
;; lines :: List MtgText (i.e. listp stringp)


(cl-defstruct (mtg-cost)
  mana additional alternative raw)


;;^ :: MultiSet MtgManaSymbol (i.e. listp symbolp)
;; :: List MtgText (i.e. listp of arrayp of stringp)


(cl-defstruct (mtg-types)
  super card sub orig raw)


;;^ :: Set MtgType (i.e. listp symbolp)


(cl-defstruct (mtg-colors)
  _ identity personality)


;;^ :: Set MtgColor (i.e. listp symbolp)


(cl-defstruct (mtg-)
   raw)


;;^ :: 


(cl-defstruct (mtg-)
   raw)


;;^ :: 


(cl-defstruct (mtg-)
   raw)


;;^ :: 


(cl-defstruct (mtg-size)
   pow tou (pow-effects nil) (tou-effects nil) (plus-counters 0) (minus-counters 0)
  raw raw-counters raw-effects)


;;^ :≥: (consp integerp integerp)
;;
;; “”
;;M-: (mtg-make-size :pow 3 :tou 3 :raw "3/3")
;; “”
;;M-: (mtg-make-size :pow 0 :tou 0 :plus-counters 3 :raw "0/0" :raw-counters "Graft 3")
;; “”
;;M-: (mtg-make-size :pow 0 :tou 0 :plus-counters 'x :raw "0/0" :raw-counters '(("~ enters the battlefield with X +1/+1 counters on it." [x x]))
;; “”
;;M-: (mtg-make-size :pow 0 :tou 1 :pow-effects '([+ card-types-in-your-graveyard]) tou-effects '([+ card-types-in-your-graveyard]) :raw-effects ("~ gets +1/+1 for each card type among cards in your graveyard."))
;; “”
;;M-: (mtg-make-size :pow [card-types-in-your-graveyard] :tou [+ 1 card-types-in-your-graveyard]] :raw-effects ("~’s power is equal to the number of card types among cards in your graveyard and its toughness is equal to that number plus 1."))
;; “Grim Strider”
;;M-: (mtg-make-size :pow 7 :tou 7 :pow-effects '([- cards-in-your-hand]) tou-effects '([- cards-in-your-hand]) :raw-effects ("~ gets -1/-1 for each card in your hand."))
;; “”
;;M-: (mtg-make-size :pow 1 :tou 1 :pow-effects '([+ ]) tou-effects '([+ ]) :raw-effects ("~ gets +X/+X, where X is ."))
;; “”
;;
;; 


(cl-defstruct (mtg-string)
  lang text)


;;^ 


;;(cl-defstruct (mtg-string)
;;  lang text orig)
;; ;;^ for mtg-face-name, mtg-rules-text, mtg-types, and mtg-flavor-text.


(cl-defstruct (mtg-identifiers)
  mtgjson hasbro scryfall)


;;^ :: 


mtg-parse-abc "" → #s(mtg-abc … "")


________________




MTG-ABC


;;; ‘mtg-abc’:


(cl-defstruct (mtg-ABC)
  id name )


(defmacro mtg-define-ABC (id &rest kwargs)
  `(prog1 (quote ,id)
          (defconst mtg-ABC/,id (mtg-make-ABC :id (quote ,id) ,@kwargs))
          (put (quote ,id) 'mtg-ABC mtg-ABC/,id)
          (add-to-list 'mtg-known-ABCs (quote ,id))))


(defun mtg-get-ABC (id)
  (get id 'mtg-ABC))


(defun mtg-put-ABC (id ABC)
  (put id 'mtg-ABC ABC))


(mtg-define-ABC …)


;;(defun mtg-put-ABC (id &optional ABC)
;;  (intern id mtg-known-ABC-obarray)
;;  (when ABC (put id 'mtg-ABC ABC))
;;  id)


(defconst mtg-known-ABC-obarray (make-vector ) "")
;; (intern … mtg-known-ABC-obarray)
;; (intern-soft … mtg-known-ABC-obarray)


;;(defvar mtg-known-ABC-list nil "")
;; ;; (add-to-list 'mtg-known-ABC-list …)
;; ;; (memq … mtg-known-ABC-list)


________________




MTG-TYPE


;;; ‘mtg-type’:


(cl-defstruct (mtg-type)
  (id        nil :type symbol)
  (name      nil :type string)
  (code      nil :type symbol)
  (kind      nil :type symbol)
  (char      nil :type character)
;;(permanent nil :type boolean)
  )


;;(cl-defstruct (mtg-card-type (:include mtg-type)) subtypes)
;;(cl-defstruct (mtg-land-subtype))


(defmacro mtg-define-type (id &rest kwargs)
  `(prog1 (quote ,id)
          (defconst mtg-type/,id (mtg-make-type :id (quote ,id) ,@kwargs))
          (put (quote ,id) 'mtg-type mtg-type/,id)
          (add-to-list 'mtg-known-types (quote ,id))))


(defmacro mtg-define-land-type (id &rest kwargs)
  `(progn
     (mtg-define-type ,id :kind 'sub-land ,@kwargs)))


(defmacro mtg-define-basic-land-type (id &rest kwargs)
  `(progn
     (mtg-define-land-type ,id :has-rule t ,@kwargs)))


(defmacro mtg-define-spell-type (id &rest kwargs)
  `(progn
     (mtg-define-type ,id :kind 'sub-spell ,@kwargs)))


(defmacro mtg-define-artifact-type (id &rest kwargs)
  `(progn
     (mtg-define-type ,id :kind 'sub-artifact ,@kwargs)))


(defmacro mtg-define-creature-type (id &rest kwargs)
  `(progn
     (mtg-define-type ,id :kind 'sub-creature ,@kwargs)))


(defmacro mtg-define-planeswalker-type (id &rest kwargs)
  `(progn
     (mtg-define-type ,id :kind 'sub-planeswalker ,@kwargs)))


(defun mtg-get-type (id)
  (get id 'mtg-type))


(defun mtg-put-type (id type)
  (put id 'mtg-type type))


;;


(defconst mtg-known-spell-types '(instant sorcery) "")
(defconst mtg-known-permanent-types '(land artifact enchantment creature planeswalker) "")
(defvaralias 'mtg-known-nonpermanent-types 'mtg-known-spell-types)
(defconst mtg-known-card-types (append mtg-known-spell-types mtg-known-permanent-types) "")


;;


(mtg-define-type sorcery
  :kind 'card :code 's)
(mtg-define-type instant
  :kind 'card :code 'i :has-rule t)
(mtg-define-type land
  :kind 'card :code 'l)
(mtg-define-type artifact
  :kind 'card :code 'a)
(mtg-define-type creature
  :kind 'card :code 'c)
(mtg-define-type planeswalker
  :kind 'card :code 'p)
(mtg-define-type tribal
  :kind 'card)


(mtg-define-type legendary
  :kind 'super :code 'L :char ?👑 :has-rule t)
(mtg-define-type snow
  :kind 'super :code 'S :char ?❄️)
(mtg-define-type basic
  :kind 'super :code 'B :has-rule t)


(mtg-define-basic-land-type plains
  :code 'w :char ?🌞)
(mtg-define-basic-land-type island
  :code 'u :char ?🏝️)
(mtg-define-basic-land-type swamp
  :code 'b :char ?💀)
(mtg-define-basic-land-type mountain
  :code 'r :char ?🌋)
(mtg-define-basic-land-type forest
  :code 'g :char ?🌳)


(mtg-define-land-type 
  :char ?)


(mtg-define-spell-type 
  :char ?)


(mtg-define-artifact-type 
 :char ?)


(mtg-define-creature-type 
  :char ?)


(mtg-define-planeswalker-type )


;;(mtg-define-type 
;;  :kind ' :code ' :char ?)


;; The new structure is considered a “specialization” of the included one. In fact, the predicate and slot accessors for the included type will also accept objects of the new type.


________________




MTG-COLOR


;;; ‘mtg-color’:


(cl-defstruct (mtg-color)
  id code)


(defmacro mtg-define-color (id &rest kwargs)
  `(prog1 (quote ,id)
          (defconst mtg-color/,id (mtg-make-color :id (quote ,id) ,@kwargs))
          (put (quote ,id) 'mtg-color mtg-color/,id)
          (add-to-list 'mtg-known-colors (quote ,id))))


(defun mtg-get-color (id)
  (get id 'mtg-color))


(defun mtg-put-color (id color)
  (put id 'mtg-color color))


(mtg-define-color white
  :code 'w)
(mtg-define-color blue
  :code 'u)
(mtg-define-color black
  :code 'b)
(mtg-define-color red
  :code 'r)
(mtg-define-color green
  :code 'g)


________________




MTG-ZONE


(cl-defstruct (mtg-zone)
  (id        nil :type symbol)
  visibility ordering ownership
  )


;;^ 
;; • secret vs private vs public — 
;; • ordered vs unordered — 
;; • (owned by a) player vs not (shared by everyone9 — 
;; •  vs  — 


(defvar mtg-known-zones nil
  "")


(mtg-define-zone battlefield
  :visibility 'public
  :ordering   'unordered
  :ownership   nil
  "")
(mtg-define-zone hand
  :visibility 'private
  :ordering   'ordered
  :ownership  'player
  "")
(mtg-define-zone graveyard
  :visibility 'public
  :ordering   'ordered
  :ownership  'player
  "")
(mtg-define-zone deck
  :visibility 'secret
  :ordering   'ordered
  :ownership  'player
  "")
(mtg-define-zone exile
  :visibility 'public
  :ordered    nil
  :ownership  'player
  "")


________________




MTG-KEYWORD


;;; ‘mtg-keyword’:


(cl-defstruct (mtg-keyword)
  (id        nil :type symbol)
  (name      nil :type string)
  (abbr-code nil :type symbol)
  (abbr-char nil :type character)
  rule-id reminder-text (scope 'wotc) edition-released
  (subsumes nil) (zones nil) (abilities nil) (idempotent-p t) (color-identity '()) (color-personality '()) (type-identity '()) (evasion-p nil) 
  "
:
• :idempotent-p — e.g. Flying and  are, Prowess and Flanking aren't. In the comprehensive rules, an idempotent keyword has a rule like “Multiple instances of <keyword> on the same object are redundant.”.
• :evasion-p — e.g. 
• :graveyard — e.g. 
• :color-identity — e.g. (:id 'extort) has (:color-identity '(w b)).
• :color-personality — e.g. (:id 'fear) and (:id 'swampwalk) both have (:color-personality '(b)), because they reference “black creatures” and “swamps“, respectively.
• :type-identity — e.g. (:id 'embalm) has (:type-identity '(zombie)), and (:color-personality '(w)).
• : — e.g. 


See also “702. Keyword Abilities” in the comprehensive rules (at URL ‘https://www.yawgatog.com/resources/magic-rules/#R702’).")


(mtg-define-keyword flying
  :name          "Flying"
  :abbr-code     "FLY"
  :abbr-char     ?🪶
  :subsumes      '(reach)
  :zones         '(battlefield)
  :abilities     '(static)
  :idempotent-p  t
  :evasion-p     t
  :released      'lea
  :rule-id       'r702-9
  :reminder-text "This creature can't be blocked except by creatures with flying and/or reach.")


(mtg-define-keyword flash
  :name          "Flash"
  :abbr-code     "FLS"
  :abbr-char     ?⚡
  :rule-id       'r702-8
  :reminder-text "You may cast this spell any time you could cast an instant."
  ;;
  :zones         '(stack)
  :abilities     '(static)
  :idempotent    t
  :released      'lea
  :released '¿tsp?)


(mtg-define-keyword embalm
  :name              "Embalm"
  :abbr-code         "EMB"
  :abbr-char         ?🧟
  :zones             '(graveyard)
  :abilities         '(activated)
  :color-identity    '()
  :color-personality '(white)
  :types-identity    '(zombie)
  :released          'akh
  :rule-id           'r702-
  :reminder-text     "")


(mtg-define-keyword lifelink
  :name              "Lifelink"
  :abbr-code         ""
  :abbr-char         ?_
  :zones             t
  :abilities         '()
  :idempotent-p      t
  :re-damage-p       t
  :released          '_
  :rule-id           'r702-
  :reminder-text     "")


;;


(mtg-define-keyword deathtouch
  :name              "Deathtouch"
  :abbr-code         ""
  :abbr-char         ?_
  :zones             t             ; 702.2d
  :abilities         '(static)     ; 702.2a
  :potency           'idempotent  ; 702.2f
  :re-damage-p       t
  :released          '_
  :rule-id           'r702-2
  :reminder-text     "")
;;:effect '("")  ; 702.2b


;;
;;>702.2. Deathtouch
;;>702.2a. Deathtouch is a static ability.
;;>702.2d. The deathtouch rules function no matter what zone an object with deathtouch deals damage from.
;;>702.2f. Multiple instances of deathtouch on the same object are redundant.
;;>702.2b,2c,2e. …
;;
;;


(mtg-define-keyword wither
  :name              "Wither"
  :abbr-code         ""
  :abbr-char         ?_
  :zones             t
  :abilities         '()
  :idempotent-p      t
  :re-damage-p       t
  :released          '_
  :rule-id           'r702-
  :reminder-text     "")


(mtg-define-keyword infect
  :name              "Infect"
  :abbr-code         ""
  :abbr-char         ?_
  :subsumes          '(wither)
  :zones             t
  :abilities         '()
  :idempotent-p      t
  :re-damage-p       t
  :released          '_
  :rule-id           'r702-
  :reminder-text     "")


(mtg-define-keyword 
  :name              ""
  :abbr-code         ""
  :abbr-char         ?_
  :idempotent-p      t
  :released          '_
  :rule-id           'r702-
  :reminder-text     "")


(mtg-define-keyword 
  :name              ""
  :abbr-code         ""
  :abbr-char         ?_
  :subsumes          '()
  :zones             '()
  :abilities         '()
  :idempotent-p      nil
  :evasion-p         nil
  :color-identity    '()
  :color-personality '()
  :types-identity    '()
  :released          '_
  :rule-id           'r702-
  :reminder-text     "")


(mtg-define-keyword first-strike
  :name "First strike" :abbr-code "1RK" :abbr-char ?_ :rule-id    'r702-7 :reminder-text "This creature .")


(mtg-define-keyword double-strike
  :name "Double strike" :abbr-code "2RK" :abbr-char ?_ :subsumes '(first-strike) :rule-id    'r702-4 :reminder-text "This creature .")


(mtg-define-keyword prowess
  :name "Prowess" :abbr-code "PRW" :abbr-char ?_ :idempotent-p nil :released 'ktk :rule-id    'r702-_ :reminder-text "Whenever you cast a noncreature spell, this creature gets +1/+1 until end of turn.")


(mtg-define-keyword prowl
  :types-identity '(rogue)
  )


(mtg-define-keyword fear
  :color-personality '(b)
  )


(mtg-define-keyword swampwalk
  :color-personality '(b)
  )


(mtg-define-keyword 
  :name "" :abbr-code "" :abbr-char ?_ :subsumes '() :rule-id 'r702- :reminder-text "This creature ." :zones '(battlefield) :abilities '(static) :idempotent-p t :evasion-p nil :color-identity '() :color-personality '() :types-identity '() :released '_)
;;
;;(mtg-define-keyword 
;;  :name          ""
;;  :abbr-code     ""
;;  :abbr-char     ?_
;;  :zones         '(battlefield)
;;  :abilities     '(static)
;;  :idempotent-p  t
;;  :evasion-p     nil
;;  :subsumes      '()
;;  :color-identity    '()
;;  :color-personality '()
;;  :types-identity    '()
;;  :released      '_
;;  :rule-id       'r702-
;;  :reminder-text "This creature .")


(mtg-define-keyword 
  :name              ""
  :abbr-code         ""
  :abbr-char         ?_
  :subsumes          '()
  :zones             '()
  :abilities         '()
  :idempotent-p      nil
  :evasion-p         nil
  :color-identity    '()
  :color-personality '()
  :types-identity    '()
  :released          '_
  :rule-id           'r702-
  :reminder-text     "")


;;


(defun mtg-get-keyword (id)
  (get id 'mtg-keyword))


(defun mtg-put-keyword (id keyword)
  (put id 'mtg-keyword keyword))


(defun mtg-keyword-custom-p (keyword-id)
  (neq 'wotc (mtg-keyword-scope (mtg-get-keyword keyword-id))))


;; rules-text isomorphic to keywords:
;; e.g. Inventor's Fair has implicit Metalcraft.


;;


________________




MTG-POWTOU


;;; ‘mtg-powtou’:


(cl-defstruct (mtg-powtou)
  (id  nil :type symbol)
  (pow nil :type (or integer symbol vector))
  (tou nil :type (or integer symbol vector))
  (raw nil :type string)  
  ) 


;;


;;


(defun mtg0-powtou-bounds-at-point ()
  "."
  (save-excursion
    (skip-chars-backward "/-+×*0123456789")
    (save-match-data
      (if (looking-at "[-+]?([0-9]+|*)([-+×]([0-9]+|*))?[/][-+]?([0-9]+|*)")
          (cons (match-beginning 0) (1- (match-end 0)))
         nil))))


(defconst mtg-powtou-rx
  (rx ()
      (char ?/)
      ()
      )
  "")
;;i.e. "[-+]?([0-9]+|*)([-+×]([0-9]+|*))?[/][-+]?([0-9]+|*)"


;;


________________




MTG-ABILITY


;;; ‘mtg-ability’:


(cl-defstruct (mtg-ability)
  (id        nil :type symbol)
  (desc      nil :type string)
  (mana-p    nil :type boolean)
  (damage-p  nil :type boolean)  
  ) 


(cl-defstruct (mtg-triggered-ability)
  id trigger effect raw) 


(cl-defstruct (mtg-activated-ability)
  id cost effect raw) 


(cl-defstruct (mtg-replacement-ability)
  id before after raw) 


(cl-defstruct (mtg-static-ability)
  id raw) 


;;


(defun mtg-parse-activated-ability (s)
  (let* (( (s)))
    (mtg-make-activated-ability :cost "" :effect "" :raw s)))


(defun mtg-parse-triggered-ability (s)
  (let* (( (s)))
    (mtg-make-triggered-ability :trigger "" :effect "" :raw s)))


(defun mtg-parse-replacement-ability (s)
  (let* (( (s)))
    (mtg-make-replacement-ability :before "" :after "" :raw s)))


;;


(defun mtg-text--remove-reminder-text (s)
  (replace-regexp-in-string (rx (0+ space) (char ?() (0+ (not (char ?)))) (char ?))) "" s))


;;


________________




MTG-KEY


;;; ‘mtg-key’:


(cl-defstruct (mtg-keyphrase)
  id name token regexp)


;; M-: (mtg-keyphrase :id 'as-long-as)
;;     #s[mtg-keyphrase as-long-as "as long as" AS_LONG_AS ""]


(defmacro mtg-define-ABC (id &rest kwargs)
  `(prog1 (quote ,id)
          (defconst mtg-ABC/,id (mtg-make-ABC :id (quote ,id) ,@kwargs))
          (put (quote ,id) 'mtg-ABC mtg-ABC/,id)
          (add-to-list 'mtg-known-ABCs (quote ,id))))


(defun mtg-get-ABC (id)
  (get id 'mtg-ABC))


(defun mtg-put-ABC (id ABC)
  (put id 'mtg-ABC ABC))


(mtg-define-ABC …)


;;(defun mtg-put-ABC (id &optional ABC)
;;  (intern id mtg-known-ABC-obarray)
;;  (when ABC (put id 'mtg-ABC ABC))
;;  id)


(defconst mtg-known-ABC-obarray (make-vector ) "")
;; (intern … mtg-known-ABC-obarray)
;; (intern-soft … mtg-known-ABC-obarray)


;;(defvar mtg-known-ABC-list nil "")
;; ;; (add-to-list 'mtg-known-ABC-list …)
;; ;; (memq … mtg-known-ABC-list)


________________




TABULATED-LIST-MODE


tabulated-list-format
;; FORMAT = [(COLUMN-NAME COLUMN-WIDTH COLUMN-SORT) …] : (vectorp stringp natnump (&or boolenp functionp))
;; i.e. the table column.
;; COLUMN-SORT: If nil, the column cannot be used for sorting. If t, the column is sorted by comparing string values. Otherwise, this should be a predicate function for `sort'.


tabulated-list-entries
;; ENTRIES = ((| (ID CONTENTS …) : (listp-of (&or (listp (&or nil objectp) (vectorp-of (&or stringp (consp stringp listp))) functionp)))
;; CONTENTS = [(| TEXT (TEXT . PROPERTIES)) …]
;; i.e. the table rows.
;; ID is either nil, or a Lisp object that identifies the entry. If the latter, the cursor stays on the same entry when re-sorting entries. Comparison is done with equal.
;; CONTENTS is a vector with the same number of elements as tabulated-list-format. Each vector element is either a string, which is inserted into the buffer as-is, or a list (label . properties), which means to insert a text button by calling insert-text-button with label and properties as arguments (see Making Buttons). There should be no newlines in any of these strings.


tabulated-list-sort-key
;; SORT-ORDER = (? (COLUMN-NAME . FLIP-ORDER-P)) : (&or nil (consp stringp booleanp))


tabulated-list-printer
;; (setq tabulated-list-printer nil) ≈ (setq tabulated-list-printer #'insert)


;;(list (list key1 (vector col1 col2 col3)) …)


(defconst mtg-card-results-mode-map
  (let ((MAP (make-sparse-keymap)))


    (define-key MAP (kbd "RET") #'mtg-tabulated-list-view-card)
    (define-key MAP (kbd "SPC") #'mtg-tabulated-list-mark-card)
    (define-key MAP (kbd "") #'mtg-results-)
    (define-key MAP (kbd "s") #'mtg-results-sort-cards-by)
    (define-key MAP (kbd "h") #'mtg-results-mode-help)


;;    (define-key MAP (kbd "") #'mtg-results-)
    MAP)
  "‘keymapp’ for ‘mtg-card-results-mode’.")
 
(defconst mtg-tabulated-list-buffer-name "*M:tG Results*")


(defconst mtg-tabulated-list-card-format
  [
    ("Name" 60 t)  ; mtg-compare-names (string-collate-lessp STRING1 STRING2 &optional LOCALE IGNORE-CASE) "en_US.UTF-8" t


    ("Mana-Cost"  8 nil)
    ("Mana-Value" 2 <)


    ("POW" 3 mtg-compare-arithmetic)
    ("TOU" 3 mtg-compare-arithmetic)
    ("LOY" 2 mtg-compare-numeric)


    ("Colors"            5 mtg-compare-colorsets)
    ("Color-Identity"    5 mtg-compare-colorsets)
    ("Color-Personality" 5 mtg-compare-colorsets)


    ("Super-Types" 20 mtg-compare-typesets)
    ("Card-Types"  20  mtg-compare-typesets)
    ("Sub-Types"   20   mtg-compare-typesets)


    ("Keywords" 20 nil)
    ("Rules-Text" ∞ nil)


    ("Released" 11 t)


;;  ("" 0 t)
  ]
  "")


(defconst mtg-tabulated-list-cardprint-format
  [
    ("Name" 60 t)


    ("Set"    3 t)
    ("Block"  3 t)
    ("Rarity" 1 t)


    ("Artist" 20 t)
    ("Flavor-Text" ∞ nil)


    ("Layout" 8 t)
    ("Frame"  8 t)
    ("Border" 1 t)


    ("#Reprints" 2  t)
    ("Date"      11 t)


    ("Language" 2 t)


;;  ("" 0 t)
  ]
  "")


;;


("" 'help-echo "")


;;(SET-CODE 'help-echo SET-NAME)
;;(KEYWORD-CODE 'help-echo KEYWORD-NAME-AND-REMINDER-TEXT)
;;(_ (insert-image (create-image "ARTWORK-THUMBNAIL.jpg")))


(defalias 'mtg-results-eldoc #'mtg-eldoc)


(defun mtg-eldoc ()
  "Return a docstring for the _, or nil."
  (let* ((WORD (thing-at-point 'mtg-word))
          (KIND (car (mtg-kinds-of WORD))))
    (pcase KIND


      (`(card    . ,ID) ( ID))
      (`(edition . ,ID) ( ID))
      (`(keyword . ,ID) (mtg-keyword-eldoc ID))


;;      (`(abc . ,ID) (mtg-abc-eldoc ID))
      (t nil))))


(defalias 'mtg-keyword-eldoc #'(lambda () (mtg-format-keyword (thing-at-point 'keyword))))


(defun mtg-get-keyword (id)
  (get id 'mtg-keyword))


;;(defun mtg-get-xyz-by-id (id)
;;  (get id 'mtg-xyz))


(defmacro mtg-define-keyword (id &rest kwargs)
  "Create an ‘mtg-keyword-p’ with KWARGS and register it as ID.
(create with function ‘mtg-make-keyword’ minus the keyword argument ‘:id’, define with macro ‘defconst’, and register with symbol property ‘mtg-keyword’.)"
  (let ((ID   id)
        (NAME (intern (format "mtg-keyword/%s" (symbol-name ID))))
        (KWARGS `(:id ,ID ,@kwargs)))
    `(prog1 ,ID
            (defconst ,NAME (mtg-make-keyword ,@KWARGS))
            (put ,ID 'mtg-keyword ,NAME))))


;;


(defun mtg-kinds-of (phrase)
  "Return the kinds of thing PHRASE can be, or nil.
M-: (mtg-kinds-of "Time Spiral")
    '((card . time-spiral) (edition . time-spiral))"
  (let* ((ID (mtg-intern-soft phrase)))
    ( ID)))


;;


(defconst mtg-tabulated-list-sort-key
 '("Name" . t))


(defconst mtg-tabulated-list-padding 2
  "")


(defconst mtg-tabulated-list-gui-sort-indicator-asc ?📈)
(defconst mtg-tabulated-list-gui-sort-indicator-desc ?📉)


(defconst mtg-tabulated-list-printer nil)


;;(defconst mtg-tabulated-list- )


(defun mtg-tabulated-list-card-entries (cards)
  )


(defun mtg-tabulated-list-cardprint-entries (prints)
  )


(defun mtg-tabulated-list-edition-entries (editions)
  )


;;; ‘mtg-results’


(defun mtg-card-results-mode-help ()
  (interactive)
  (let ((BUFFER ()))
    (_ BUFFER)))


(defun mtg-results-view-card (&optional card-id)
  (interactive)
  (let ((CARD-ID (or card-id (tabulated-list-get-id))))
    (_ CARD-ID)))


(defun mtg-results-mark-card (&optional card-id)
  (interactive)
  (let ((CARD-ID (or card-id (tabulated-list-get-id))))
    (_ CARD-ID)))


(defun mtg-results-sort-cards-by (&optional )
  (interactive)
  (let ((COLUMN ())
        (FIELD  ()))
    (_)))


(define-derived-mode mtg-card-results-mode tabulated-list-mode "mtg-card-results-mode" "Major mode "
  (mtg-results-mode-init))


(defun mtg-results-mode-init ()
    (setq tabulated-list-format mtg-tabulated-list-card-format)
    (setq tabulated-list-sort-key mtg-tabulated-list-sort-key)
    (setq tabulated-list-padding mtg-tabulated-list-padding)
    (setq tabulated-list-gui-sort-indicator-asc mtg-tabulated-list-gui-sort-indicator-asc)
    (setq tabulated-list-gui-sort-indicator-desc mtg-tabulated-list-gui-sort-indicator-desc)
    (use-local-map mtg-card-results-mode-map)
    (setq-local eldoc-documentation-function #'mtg-results-eldoc)


    (tabulated-list-init-header)
    ())


(defun mtg-list-cards (&optional query-string)
  (interactive "sCard Query: ")


  (let* ((card-query (mtg-parse-query query-string))
         (card-matches (mtg-run-query card-query))
         (card-entries (mtg-tabulated-list-card-entries-from card-matches)))


    (pop-to-buffer mtg-tabulated-list-buffer-name nil)
    (mtg-results-mode)
    (mtg-results-mode-refresh-data)
    
    (setq tabulated-list-entries card-entries)
    (tabulated-list-print t))


(defun mtg-tabulated-list-card-entries-from (cards)
  (cl-loop for card in cards
    collect (mtg-tabulated-list--card-entry-from card)))


(defun mtg-tabulated-list--card-entry-from (card)
  (let* ((ID (mtg-card-id card))
         (NAME (mtg-tabulated-list--name-entry-from (mtg-card-name card)))
         ( (mtg-card- card))
         ( (mtg-card- card))
         ( (mtg-card- card))
         (CARDTYPES (mtg-tabulated-list--types-entry-from (mtg-card-cardtypes card)))
         ( (mtg-card- card))
         ( (mtg-card- card))
  ;;     ( (mtg-card- card))
         (ROW (vector NAME ))
         (ENTRY (list ID ROW)))
    ENTRY))


;; ENTRIES = ((ID CONTENTS) …) : (listp-of (&or (listp (&or nil objectp) (vectorp-of (&or stringp (consp stringp listp))) functionp)))
;; CONTENTS = [(| TEXT (TEXT . PROPERTIES)) …]


'(:weight bold)  ; embolden
'(:slant italic) ; italicize


(defun mtg-tabulated-list--name-entry-from (name)
  (let* ((PROPS '(bold t))
         (TEXT name))
    (cons TEXT PROPS)))


(defun mtg-tabulated-list--types-entry-from (types)
  (let* ((FACE '(italics t))
         (TEXT (mtg-unintern-types types)))
    (propertize TEXT (list 'face FACE))))


(defun mtg-tabulated-list--_-entry-from (_)
  (let* ((PROPS '())
         (TEXT (mtg-unintern-_ _)))
    (cons TEXT PROPS)))


(defun mtg-tabulated-list--_-entry-from (_)
  (let* ((PROPS '())
         (TEXT (mtg-unintern-_ _)))
    (cons TEXT PROPS)))


;;(defun mtg-tabulated-list--_-entry-from (_)
;;  (let* ((PROPS '())
;;         (TEXT (mtg-unintern-_ _)))
;;    (cons TEXT PROPS)))


;;; ‘mtg-query’


(defun mtg-query-cards (query)
  )


(defun mtg-query-prints (query)
  )


;;;


(define-button-type mtg- '())


(define-button-type t 'keymap '(([RET] . #'push-button) ([mouse-2] . #'push-button)))


(insert-text-button :type 'mtg- ' t ' t)


;;


(defvar mtg-tabulated-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)


    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "<right>") #'tabulated-list-next-column)
    (define-key map (kbd "<left>") #'tabulated-list-previous-column)


    (define-key map (kbd "SPC") #'mtg-tabulated-list-mark-result)
    (define-key map (kbd "RET") #'mtg-tabulated-list-view-result)
   (define-key map (kbd */") #'mtg-tabulated-list-query)


    (define-key map "s" mtg-tabulated-list-sort-map)


    (define-key map (kbd "m") #'mtg-tabulated-list-mark-result)
    (define-key map (kbd "a") #'mtg-tabulated-list-toggle-mark-all-results)
    (define-key map (kbd "d") #'mtg-tabulated-list-save-marked-results)


    (define-key map [mouse-1] #'mtg-tabulated-list-)
    (define-key map [mouse-3] #'mtg-tabulated-list-)


  ;; 
  ;;(define-key map "n" #'next-line)
  ;;(define-key map "p" #'previous-line)
  ;;(define-key map "}" #'tabulated-list-widen-current-column)
  ;;(define-key map "{" #'tabulated-list-narrow-current-column)
  ;;(define-key map [mouse-2] #'mouse-select-window)


    map)
  "‘keymapp’ for ‘mtg-tabulated-list-mode’.")


(defvar mtg-tabulated-list-sort-map
  (let ((map (make-sparse-keymap)))


    (define-key map (kbd "n") #'mtg-tabulated-list-sort-by-name)
    (define-key map (kbd "v") #'mtg-tabulated-list-sort-by-mana-value)
    (define-key map (kbd "d") #'mtg-tabulated-list-sort-by-release-date)
    (define-key map (kbd "w") #'mtg-tabulated-list-sort-by-power)
    (define-key map (kbd "u") #'mtg-tabulated-list-sort-by-toughness)
    (define-key map (kbd "c") #'mtg-tabulated-list-sort-by-colors)
    (define-key map (kbd "t") #'mtg-tabulated-list-sort-by-types)
    (define-key map (kbd "k") #'mtg-tabulated-list-sort-by-keywords)
    (define-key map (kbd "") #'mtg-tabulated-list-sort-by-)
    (define-key map (kbd "") #'mtg-tabulated-list-sort-by-)
    (define-key map (kbd "s") #'tabulated-list-sort)
  ;;(define-key map (kbd "") #'mtg-tabulated-list-sort-by-)


    map)
  "")


(defun mtg-tabulated-list-toggle-mark ()
  (interactive)
  (if (mtg-tabulated-list-marked-p)
      (mtg-tabulated-list-unmark-result)
    (mtg-tabulated-list-mark-result)))


(defun mtg-tabulated-list-marked-p ()
  (string-prefix-p "✓" (thing-at-point 'line))


(defun mtg-tabulated-list-mark ()
  (interactive)
  (tabulated-list-put-tag "✓ " 'advance))


(defun mtg-tabulated-list-unmark ()
  (interactive)
  (tabulated-list-put-tag "  " nil))


(defun mtg-tabulated-list-save-marked-results ()
  (interactive)
  ()
  (tabulated-list-clear-all-tags)
  ())


(defun mtg-tabulated-list-sort-by-name ()
  (interactive)
  (tabulated-list--sort-by-column-name "Name"))


(defun mtg-tabulated-list-sort-by-mana-value ()
  (interactive)
  (tabulated-list--sort-by-column-name "Mana-Value"))


(defun mtg-tabulated-list-sort-by-release-date ()
  (interactive)
  (tabulated-list--sort-by-column-name "Date"))


(defun mtg-tabulated-list-sort-by-power ()
  (interactive)
  (tabulated-list--sort-by-column-name "POW"))


(defun mtg-tabulated-list-sort-by-toughness ()
  (interactive)
  (tabulated-list--sort-by-column-name "TOU"))


(defun mtg-tabulated-list-sort-by- ()
  (interactive)
  (tabulated-list--sort-by-column-name ""))


(defun mtg-tabulated-list-sort-by- ()
  (interactive)
  (tabulated-list--sort-by-column-name ""))


(defun mtg-tabulated-list-sort-by- ()
  (interactive)
  (tabulated-list--sort-by-column-name ""))


(defun mtg-tabulated-list-sort-by- ()
  (interactive)
  (tabulated-list--sort-by-column-name ""))


(defun mtg-tabulated-list-sort-by- ()
  (interactive)
  (tabulated-list--sort-by-column-name ""))


;;(defun mtg-tabulated-list-sort-by- ()
;;  (interactive)
;;  (tabulated-list--sort-by-column-name ""))


;;(event-start EVENT) If event is a drag event, this returns the drag’s starting position.
If event is a click or button-down event, this returns the location of the event.
;;(event-end EVENT) If event is a drag event, this returns the position where the user released the mouse button. If event is a click or button-down event, event-end is event-start.


________________




MTG-COMPRULES


"For example:
>702.9. Flying
>702.9a. Flying is an evasion ability.
>702.9b. A creature with flying can't be blocked except by creatures with flying and/or reach. A creature with flying can block a creature with or without flying. (See rule 509, "Declare Blockers Step," and rule 702.17, "Reach.")
>702.9c. Multiple instances of flying on the same creature are redundant."


(mtg-define-comprule r702.7
  '([702.9 "Flying"
      [702.9a "Flying is an evasion ability."]
      [702.9b "A creature with flying can't be blocked except by creatures with flying and/or reach. A creature with flying can block a creature with or without flying. (See rule 509, “Declare Blockers Step,” and rule 702.17, “Reach.”)"]
      [702.9c "Multiple instances of flying on the same creature are redundant."]]))


(defun mtg-yawgatog-rule-url (rule-id)
  "
e.g. (mtg-yawgatog-rule-url 'r702-9) → URL ‘https://www.yawgatog.com/resources/magic-rules/#R7029’"
  (let ((rule-s (replace-regexp-in-string (rx (char ?r ?-)) "" (symbol-name rule-id))))
    (format "https://www.yawgatog.com/resources/magic-rules/#R%s" rule-s)))


(defun mtg-yawgatog-keyword-url (keyword-id)
  "
e.g. (mtg-yawgatog-keyword-url 'flying) → URL ‘https://www.yawgatog.com/resources/magic-rules/#R7029’"
  (let ((rule-id (mtg-keyword-rule-id keyword-id)))
    (mtg-yawgatog-rule-url rule-id)))


________________




MTG-IMAGE


;;; ‘mtg-svg’:


(require 'svg)
(require 'xml)


(defun mtg-svg-parse-file (file)
  "Parse a “.svg” FILE."
  (declare (pure t) (side-effect-free t))
  (let* (
        )
    ()))


(SVG-ROOT (xml-parse-file file))
(SVG- (car (xml-node-children SVG-ROOT 'path)))
(SVG-PATH (car (xml-get-children SVG- 'path)))
(SVG-DATA (cdr (assq 'd (xml-node-attributes SVG-PATH))))


(post (car root))
       (login (car (xml-get-children post 'login)))
       (login-user (car (xml-node-children login)))
       (attrs (xml-node-attributes post))
       (time (cdr (assq 'time attrs)))


(setq parse-tree (xml-parse-file "test.xml"))    
(setq trials-node (assq 'trials parse-tree))
(setq trial-node (car (xml-get-children trials-node 'trial)))
(setq main-node (car (xml-get-children trial-node 'main)))
(setq trial_id-node (car (xml-get-children main-node 'trial_id)))
(car (xml-node-children trial_id-node))
==> "RBR-3ypfxv"
(setq scientific_title-node (car (xml-get-children main-node 'scientific_title)))
(car (xml-node-children scientific_title-node))


(defun mtg-svg-insert-file (file &optional width height)
  "Insert a “.svg” FILE."
  (insert-image (find-image `((:type svg :file ,file :width ,width :height ,height))))
;;(shr-insert-file file)
  )


;; e.g. “tsp.svg”: <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="23" height="32" viewBox="0 0 23 32"><title>tsp</title><path fill="#444" d="M17.678 8.759c-2.333 0.822-3.783 1.063-6.256 1.046-2.434-0.014-3.856-0.263-6.149-1.085-0.44-0.869-0.712-1.868-0.712-2.732h13.869c0 0.931-0.752 2.77-0.752 2.77zM14.905 20.411c2.201 1.158 3.631 2.849 3.631 4.907 0 0.303-0.028 0.291-0.087 0.687h-2.872c-1.974-1.472-3.34-3.895-3.34-7.142l-0.019-0.348c0 0 1.223 1.125 2.686 1.896zM10.758 18.961c0 3.248-1.369 5.673-3.346 7.045h-2.869c-0.062-0.294-0.093-0.284-0.093-0.592 0-2.056 1.436-3.749 3.632-4.902 1.472-0.771 2.689-1.999 2.689-1.999l-0.014 0.449zM22.972 5.692v-2.448c0-0.172-0.134-0.312-0.306-0.312h-22.347c-0.168 0-0.305 0.14-0.305 0.312v2.448c0 0.162 0.137 0.3 0.305 0.3h1.815l-0.003 0.235c0 2.804 1.96 4.877 4.386 6.155 1.408 0.735 1.728 1.63 1.728 3.304 0 1.652-0.351 2.594-1.772 3.34-2.431 1.276-4.389 3.351-4.389 6.152 0 0.286 0.020 0.556 0.057 0.828h-1.821c-0.168 0-0.305 0.14-0.305 0.308v2.452c0 0.165 0.137 0.302 0.305 0.302h22.347c0.172 0 0.306-0.137 0.306-0.302v-2.452c0-0.168-0.134-0.308-0.306-0.308h-1.864c0.031-0.272 0.056-0.541 0.056-0.828 0-2.801-1.963-4.877-4.391-6.152-1.419-0.746-1.77-1.688-1.77-3.34 0-1.612 0.373-2.543 1.82-3.304 2.429-1.276 4.388-3.351 4.388-6.155l-0.003-0.235h1.767c0.169 0 0.303-0.138 0.303-0.3z"></path></svg>
;; ;; d="M 17.678 8.759 c -2.333 0.822 -3.783 1.063 -6.256 1.046 -2.434 -0.014 -3.856 -0.263 -6.149 -1.085 -0.44 -0.869 -0.712 -1.868 -0.712 -2.732 h 13.869 c 0 0.931 -0.752 2.77 -0.752 2.77 z M 14.905 20.411 c 2.201 1.158 3.631 2.849 3.631 4.907 0 0.303 -0.028 0.291 -0.087 0.687 h -2.872 c -1.974 -1.472 -3.34 -3.895 -3.34 -7.142 l -0.019 -0.348 c 0 0 1.223 1.125 2.686 1.896 z M 10.758 18.961 c 0 3.248 -1.369 5.673 -3.346 7.045 h -2.869 c -0.062 -0.294 -0.093 -0.284 -0.093 -0.592 0 -2.056 1.436 -3.749 3.632 -4.902 1.472 -0.771 2.689 -1.999 2.689 -1.999 l -0.014 0.449 z M 22.972 5.692 v -2.448 c 0 -0.172 -0.134 -0.312 -0.306 -0.312 h -22.347 c -0.168 0 -0.305 0.14 -0.305 0.312 v 2.448 c 0 0.162 0.137 0.3 0.305 0.3 h 1.815 l -0.003 0.235 c 0 2.804 1.96 4.877 4.386 6.155 1.408 0.735 1.728 1.63 1.728 3.304 0 1.652 -0.351 2.594 -1.772 3.34 -2.431 1.276 -4.389 3.351 -4.389 6.152 0 0.286 0.020 0.556 0.057 0.828 h -1.821 c -0.168 0 -0.305 0.14 -0.305 0.308 v 2.452 c 0 0.165 0.137 0.302 0.305 0.302 h 22.347 c 0.172 0 0.306 -0.137 0.306 -0.302 v -2.452 c 0 -0.168 -0.134 -0.308 -0.306 -0.308 h -1.864 c 0.031 -0.272 0.056 -0.541 0.056 -0.828 0 -2.801 -1.963 -4.877 -4.391 -6.152 -1.419 -0.746 -1.77 -1.688 -1.77 -3.34 0 -1.612 0.373 -2.543 1.82 -3.304 2.429 -1.276 4.388 -3.351 4.388 -6.155 l -0.003 -0.235 h 1.767 c 0.169 0 0.303 -0.138 0.303 -0.3 z"


;; “keyrune/tsp.svg.el”:


(defconst mtg-svg/edition/tsp (let* ((WIDTH 23) (HEIGHT 32) (FILL-COLOR "#FFF") (STROKE-COLOR "#000") (SVG (svg-create WIDTH HEIGHT))) (svg-path SVG '((moveto ((17.678 . 8.759)) :relative nil) (curveto ((-2.333 0.822 -3.783 1.063 -6.256 1.046) (-2.434 -0.014 -3.856 -0.263 -6.149 -1.085) (-0.44 -0.869 -0.712 -1.868 -0.712 -2.732)) :relative t) (horizontal-curveto (13.869) :relative t) (curveto ((0 0.931 -0.752 2.77 -0.752 2.77)) :relative t) (close-path) (moveto ((14.905 . 20.411)) :relative nil) (curveto ((2.201 1.158 3.631 2.849 3.631 4.907) (0 0.303 -0.028 0.291 -0.087 0.687)) :relative t) (horizontal-lineto (-2.872) :relative t) (curveto ((-1.974 -1.472 -3.34 -3.895 -3.34 -7.142)) :relative t) (lineto ((-0.019 . -0.348)) :relative t) (curveto ((0 0 1.223 1.125 2.686 1.896)) :relative t) (close-path) (moveto ((10.758 . 18.961)) :relative nil) (curveto ((c 0 3.248 -1.369 5.673 -3.346 7.045)) :relative t) (horizontal-lineto (-2.869) :relative t) (curveto ((-0.062 -0.294 -0.093 -0.284 -0.093 -0.592) (0 -2.056 1.436 -3.749 3.632 -4.902) (1.472 -0.771 2.689 -1.999 2.689 -1.999)) :relative t) (lineto ((-0.014 . 0.449)) :relative t) (close-path) (moveto ((22.972 . 5.692)) :relative nil) (vertical-lineto (-2.448) :relative t) (curveto ((0 -0.172 -0.134 -0.312 -0.306 -0.312)) :relative t) (horizontal-lineto (-22.347) :relative t) (curveto ((-0.168 0 -0.305 0.14 -0.305 0.312)) :relative t) (vertical-lineto (2.448) :relative t) (curveto ((0 0.162 0.137 0.3 0.305 0.3)) :relative t) (horizontal-lineto (1.815) :relative t) (lineto ((-0.003 . 0.235)) :relative t) (curveto ((0 2.804 1.96 4.877 4.386 6.155) (1.408 0.735 1.728 1.63 1.728 3.304) (0 1.652 -0.351 2.594 -1.772 3.34) (-2.431 1.276 -4.389 3.351 -4.389 6.152) (0 0.286 0.020 0.556 0.057 0.828)) :relative t) (horizontal-lineto (-1.821) :relative t) (curveto ((-0.168 0 -0.305 0.14 -0.305 0.308)) :relative t) (vertical-lineto (2.452) :relative t) (curveto ((0 0.165 0.137 0.302 0.305 0.302)) :relative t) (horizontal-lineto (22.347) :relative t) (curveto ((0.172 0 0.306 -0.137 0.306 -0.302)) :relative t) (vertical-lineto (-2.452) :relative t) (curveto ((0 -0.168 -0.134 -0.308 -0.306 -0.308)) :relative t) (horizontal-lineto (-1.864) :relative t) (curveto ((0.031 -0.272 0.056 -0.541 0.056 -0.828) (0 -2.801 -1.963 -4.877 -4.391 -6.152) (-1.419 -0.746 -1.77 -1.688 -1.77 -3.34) (0 -1.612 0.373 -2.543 1.82 -3.304) (2.429 -1.276 4.388 -3.351 4.388 -6.155)) :relative t) (lineto ((-0.003 . -0.235)) :relative t) (horizontal-lineto (1.767) :relative t) (curveto ((0.169 0 0.303 -0.138 0.303 -0.3)) :relative t) (close-path)) :fill FILL-COLOR :stroke STROKE-COLOR) SVG) "TSP (⏳)")  ; (insert-image (svg-image mtg-svg/edition/tsp))


;; “mana/u.svg.el”:


(defconst mtg-svg/mana/u (let* ((WIDTH 32) (HEIGHT 32) (FILL-COLOR "#FFF") (STROKE-COLOR "#000") (SVG (svg-create WIDTH HEIGHT))) (svg-path SVG '((moveto ((23.11 . 29.095)) :relative nil) (curveto ((-1.903 1.937 -4.248 2.905 -7.033 2.905) (-3.126 0 -5.605 -1.070 -7.439 -3.21) (-1.733 -2.038 -2.599 -4.637 -2.599 -7.795) (0 -3.397 1.478 -7.271 4.433 -11.62) (2.411 -3.566 5.248 -6.692 8.509 -9.375) (-0.476 2.175 -0.713 3.72 -0.713 4.637) (0 2.107 0.662 4.162 1.986 6.166) (1.631 2.378 2.87 4.145 3.72 5.299) (1.325 2.005 1.987 3.958 1.987 5.859) (0.001 2.82 -0.951 5.198 -2.852 7.133)) :relative t) (close-path) (moveto ((23.058 . 18.216)) :relative nil) (curveto ((-0.509 -1.138 -1.104 -1.893 -1.784 -2.268) (0.102 0.204 0.153 0.493 0.153 0.867) (0 0.714 -0.204 1.732 -0.612 3.057)) :relative t) (lineto ((-0.662 . 2.038)) :relative t) (curveto ((0 1.189 0.593 1.784 1.783 1.784) (1.256 0 1.885 -0.833 1.885 -2.497) (0 -0.848 -0.254 -1.842 -0.764 -2.981)) :relative t) (close-path)) :fill FILL-COLOR :stroke STROKE-COLOR) SVG) "{U}")  ; (insert-image (svg-image mtg-svg/mana/u))


;; size, color, drop shadow, etc.






;;n.b. SVGs:
;;
;; MoveTo: M (m).
;; LineTo: L (l), H (h), V (v).
;; Cubic Bézier Curve: C (c), S (s).
;; Quadratic Bézier Curve: Q (q), T (t).
;; Elliptical Arc Curve: A (a).
;; ClosePath: Z (z).
;; ^ Commands are case-sensitive. An upper-case command specifies absolute coordinates, while a lower-case command specifies coordinates relative to the current position.
;; negative angles will be anti-clockwise; absolute negative x and y values are interpreted as negative coordinates; relative negative x values move to the left, and relative negative y values move upwards.
;; e.g. "M 0 0" means “move to the (¿) upper-left (?) corner”.
;; e.g. "m 10 -7" means “move 10px up and 7px to the left (from the last point)”. 
;;


;;; ‘mtg-jpg’:


;;; ‘mtg-png’:


;;; ‘mtg-image’:


;; '(:type jpg :file ".jpg" :width _ :height _ :max-width _ :max-height _)


(defconst mtg-thumbnail-image-type 
 'jpg)
(defconst mtg-thumbnail-image-width  146)
(defconst mtg-thumbnail-image-height 204)


(defun mtg-make-thumbnail-image (id &rest kwargs)
  (let* ((file (mtg-thumbnail-image-get-file id)))
    `(,@kwargs :type ,mtg-thumbnail-image-type :file ,file :width ,mtg-thumbnail-image-width :height ,mtg-thumbnail-image-height)))


(defun mtg-thumbnail-image-get-file (id)
  (format "scryfall/small/%s.jpg" (symbol-name id)))


(defun mtg-thumbnail-image-get-url (id)
  (alist-get 'small (mtg-identifiers-scryfall (mtg-print-identifiers (get id 'mtg-print)))))


;;


(defimage mtg-thumbnail-image/regeneration-cei-214
  (mtg-make-thumbnail-image :id 'regeneration-cei-214))
  "
URL ‘https://c1.scryfall.com/file/scryfall-cards/small/front/7/b/7bb17e54-2f33-434f-8bd3-73c15f6bc3ee.jpg’")


  



(defimage mtg-image-artwork/regeneration-cei214
  '((:type jpg :file "scryfall/art_crop/regeneration_cei214.jpg"))
  "
URL ‘https://c1.scryfall.com/file/scryfall-cards/art_crop/front/7/b/7bb17e54-2f33-434f-8bd3-73c15f6bc3ee.jpg’")


  



(defimage mtg--image/regeneration-cei214
  '((:type png :file "scryfall/png/regeneration_cei214.png"))
  "
URL ‘https://c1.scryfall.com/file/scryfall-cards/png/front/7/b/7bb17e54-2f33-434f-8bd3-73c15f6bc3ee.png’")


;;


;;n.b.:
;;
;; (defmacro defimage (symbol specs &optional doc) → `(defvar ,symbol (find-image ',specs) ,doc))
;;
;; image-load-path
;;
;;M-: (insert-image 'mtg-thumbnail-image/regeneration-cei214 &optional STRING AREA SLICE)
;;


(defmacro mtg-inline-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-substring-no-properties)))


________________




MTG-QUERY


;;; ‘mtg-query’:


(cl-defstruct (mtg-card-query)
  (id    nil :type symbol)
  (name  nil :type (or null string))
  (value t   :type t)
  )


;;^
;; 
;; ID — is for ui (by parsing aliases) and speed (via caching).
;;
;; M-: (mtg-make-card-query :id 'ancestral-like )
;;


;;


(defun mtg-parse-card-query (&optional string)
  "
M-: (mtg-parse-card-query \"'ancestral &u *spell $$≤1 @draw three cards@\")
    #s(mtg-card-query )
"
  (string))


;;
;; "'ancestral &u *spell $$≤1 @draw three cards@"
;; ↓
;; '(("'" "ancestral") ("&" "u") ("*" "spell") ("$$" "≤1") ("@" "draw three cards" "@"))
;;
;; (mtg-parse-card-query "'ancestral &u *spell $$≤1 @draw three cards@") a.k.a. (mtg-parse-card-query "n:ancestral c:blue t:spell v<=1 x:“draw three cards”")
;; ↓
;; (mtg-make-card-query :name (rx bow "ancestral" eow) :colors '(has (u)) :types '(has (or instant sorcery)) :mana-value (is (or 0 1)) :rules-text (rx bow "draw" eow bow "three" eow bow "cards" eow))
;; ↓
;; ()
;;


(mtg-parse-card-query "+u ++rg *c,p **i;dragon $$≥3 /3 @@flying || ")
↓
(mtg-make-card-query )
↓
()


(mtg-parse-card-query "")
↓
(mtg-make-card-query )
↓
()


(mtg-parse-card-query "")
↓
(mtg-make-card-query )
↓
()


(mtg-parse-mtgq-card-query "@[draw(s) three cards] @[lose(s) three life][gain(s) three poison counters]")
≈
(mtg-parse-mtgq-card-query "@(has \"draw(s) three cards\") @(has (or \"lose(s) (3) life\" \"gain(s) three poison counters\"))")
≈
(mtg-parse-scryfall-card-query "o:/draw[s]? three cards/ (o:/lose1[s]? (3|three) life/ or o:/gain[s]? three poison counters/)")
≈
(mtg-parse-elisp-card-query "(and (text (has (stem \"draw\") \"three cards\")) (text (has (or (stem \"lose\") (num 3) \"life\" (stem \"gain\") \"three poison counters\"))))")
↓
(mtg-build-card-query (and (text (has (stem "draw") "three cards")) (text (has (or (stem "lose") (num 3) "life" (stem "gain") "three poison counters")))))
↓
(mtg-build-card-query
  (and (text (has (or "draw" "draws") "three cards"))
       (text (has (or (or "lose" "loses") (or "3" "three") "life")
                  (or (or "gain" "gains") "three poison counters")))))


;; 
;;
;; ?␣ → `(and )
;; ?& → `(and )
;; ?| → `(or )
;; ?! → `(not )
;;
;; ?- → `(not )
;; ?, → `(and )
;; ?; → `(or )
;; ?- → `(not )
;;
;; - e.g. `…?` — *QueryVars* suffixed with a question-mark are (by convention) *Booleans*.
;; - e.g. `…#` — *QueryVars* suffixed with a hash-tag are (by convention) *Naturals*. 
;; 
;; 


(defun mtg-query--parse-form (form)
  ""
  (pcase-let* (((head . body) form)))
    (pcase head


      ((or '|| 'or)  `( ,@body))
      ((or '&& 'and) `( ,@body))


      ((or ' ') `( ,@body))
      ((or ' ') `( ,@body))


      ((pred functionp) `(apply ,head ,@body))
      (_
        (error "Unknown query-function or undefined elisp-function: ‘%s’" head)))))


;;


(defconst mtg-query--number-english-word-alist
  '((0  . "zero")
    (1  . "one")
    (2  . "two")
    (3  . "three")
    (4  . "four")
    (5  . "five")
    (6  . "six")
    (7  . "seven")
    (8  . "eight")
    (9  . "nine")
    (10 . "ten")
    (1 . "")
    (1 . "")
    (1 . "")
    (1 . "")
    (1 . "")
    (1 . "")
    (1 . "")
    (1 . "")
    (1 . "")
    (20 . "twenty")
    (99 . "ninety-nine")
;;  ( . "")
   )
  "")


(defun mtg-query--fuzz-word (s)
  (append (mtg-query--fuzz-number s)
          (mtg-query--fuzz-noun s)
          (mtg-query--fuzz-verb s)))


(defun mtg-query--fuzz-noun (s)
  (let* ((STEM
          s)  ;TODO make singular & american.
         (PLURAL-DECLENSIONS
          (list (concat STEM "s") (concat STEM "es")))
         (BRITISH-TRANSLATIONS
          (if (string-suffix-p "or" STEM) (list (concat (string-remove-suffix "or" STEM))) (list))))
         )
    (append (list STEM) PLURAL-DECLENSIONS BRITISH-TRANSLATIONS)))


(defun mtg-query--fuzz-verb (s)
  (let* ((STEM
          s)  ;TODO make 2nd-person & present-tense.
         (PERSON-CONJUGATIONS
          (list (concat STEM "s") (concat STEM "es")))
         (TENSE-CONJUGATIONS
          (list (concat STEM "ed") (concat STEM "t")))
         )
    (append (list STEM) PERSON-CONJUGATIONS TENSE-CONJUGATIONS)))


defun mtg-query--fuzz-number (s)
  (let* ((N
          (or (mtg-json--parse-natural s) s))  ;TODO make numeric.
         (LETTERS
          (alist-get N mtg-query--number-english-word-alist nil nil #'eql))
         (DIGITS
          (print1-to-string N))
         )
    (append (if LETTERS (list LETTERS) (list))
            (if DIGITS (list DIGITS) (list)))))


;;


(defun mtgq-to-string (form)
  )


(defmacro mtgq (&rest form)
  (mtgq-to-string form))


;; (safe-eval …)


;;


(defun mtg-match-card (query card)
  ""
  (and (mtg-match-card-name (mtg-card-query-name query) (mtg-card-name card))
       (mtg-match-card- (mtg-card-query- query) (mtg-card- card))
     ;;(mtg-match-card- (mtg-card-query- query) (mtg-card- card))
       ))


(defun mtg-match-card-name (query name &optional )
  (string-infix-p name query))


(defun mtg-match-card- (query )
  ())


;;(defun mtg-match-card- (query )
;;  ())


;;


(defmacro mtg-make-card-matcher (query)
  (let* ((NAME-QUERY (mtg-card-query-name query))
         ())
    `(lambda (card)
       (and ()
            ()))))


;;


(defalias 'mtg-query-cards #'mtg-query-cards-strictly-with-progress-reporter)


;;


(defun mtg-query-cards-strictly (query &optional cards)
  ""
  (cl-loop for CARD across (or cards mtg-known-cards)
    if (mtg-match-card query CARD)
    collect CARD))


;;


(require 'generator)


(iter-defun mtg-query-cards-lazily (query &optional cards)
  ""
  (cl-loop for CARD across (or cards mtg-known-cards)
    if (mtg-match-card query CARD)
    do (iter-yield CARD)
    finally return nil))


;;


(defun mtg-query-cards-strictly-with-progress-reporter (query &optional editions)
  ""
  (let* ((EDITIONS (or editions mtg-known-editions))
         (N (length EDITIONS))
         (PROGRESS-MESSAGE (format-message "Searching %s%d editions..." (if (not editions) "(all) " "") N))
         (PROGRESS-REPORTER
          (make-progress-reporter PROGRESS-MESSAGE 0 N)))
    (cl-loop for EDITION being each element of EDITIONS using (index K)
      do (progress-reporter-update PROGRESS-REPORTER (+1 K) (format " %s" (mtg-unintern-edition-code (edition-code EDITION))))
      append (cl-loop for CARD across (mtg-edition-cards EDITION)
               if (mtg-match-card query CARD)
               collect CARD)
      finally do (progress-reporter-done PROGRESS-REPORTER))))


;;


(defun mtg-edition-cards (edition)
  ""
  ())


(defun mtg-unintern-edition-code (s)
  "
M-: (mtg-unintern-edition-code 'lea)
\"LEA\""
  (upcase (symbol-name s)))


;;


;; “Cross Queries”:
;; - `NAME-WORDS` & `SUB-TYPES` —
;;     - e.g. `SUB-TYPES /\ NAME-WORDS` → `(intersection (mtg-sub-types-of TYPES) (mtg-words-of NAME))`
;;     - a.k.a. cards named after their tribe, a.k.a. any creature card with (at least) one of its creature types in its card name.
;;     - e.g. matches *Serra Angel*.
;;     - i.e. `(not (null (intersection CREATURE-TYPES (words NAME))))` (a.k.a. non-empty intersection, via the “falsiness” of `()`).
;;     - 
;; - `POW` & `TOU` & `MV` —
;;     - e.g. `(<= (min POW TOU) MV (max POW TOU))` → `()`
;; - `^…` rx —
;;     - e.g. `'^elves` (→ `(name (rx bol "elves"))`) means *“cards whose name starts with ‘elves’ (case-insensitive)”*, for example *Elves of Deep Shadow*.
;; - `…$` rx —
;;     - e.g. `'elves$` (→ `(name (rx "elves" eol))`) means *“cards whose name ends with ‘elves’”*, for example *Llanowar Elves*.
;; - 
;; - 


;; Number Queries: 
;; - `#%…` (how many colors) —
;;     - e.g. exactly bi-color cards (guild cards): `#%2` → `#%=2` → `(= COLORS# 2)` → `(= (length COLORS) 2)`
;;     - n.b. matches *Transguild Courier* too.
;;  - `#*…` (how many types) —
;;     - e.g. at least tri-tribal creature cards: `#*_2+ *c` → `#*_>=2 *>=creature` → `(and (>= SUB-TYPES# 2) (memq 'creature TYPES))` → `(and (>= (length SUB-TYPES) 2) (memq 'creature TYPES))`
;;     - n.b. matches *Avian Changeling* (and all `Changeling`s) too.
;; - `#$:…` (predicates on mana-value) —
;;     - e.g. `#$odd` → `#$:odd` → `(cl-oddp MANA-VALUE)`
;; - 
;; - `#/:…` & `#\:…` (predicates on power/toughness) —
;;     - e.g. `#/neg` → `#/:negative` → `(when (numberp POWER) (cl-minusp POWER))`
;; - 


;; Text Queries:
;; - `!@[…]`—
;;     - `!@` matches literal (i.e. case-sensitive, no noun stemming or verb stemming / no synonyms / no spelling variants / no digit-letter number variants, no parenthesized Elisp interpolation, etc).
;;     - e.g. `!@[]`.
;; - `@@@[…]`—
;;     - `@@@` searches within reminder text too (not just rules text) 
;;     - e.g. `@@@[]`.
;; - `@[… .. …]` & `@[… ... …]`—
;;     - `..` matches words/phrases within a line in order.
;;     - `...` matches lines in order.
;;     - e.g. `@[ .. ]`: .
;;     - e.g. `@[_ ... flash]`: .
;;
;; - `@[…~…]`—
;;     - `~` matches both the ‘mtg-card-name’ and "this spell".
;;     - e.g. `@[When ~ enters the battlefield,] | @[When you cast ~,]`
;; - `@[…(s)]` —
;;     - i.e. matches both “…” and “…s” or “…es”.
;;     - e.g. 
;; - `@[<2>]` (&al) —
;;     - i.e. `<2>` matches both “2” and “two”.
;;     - e.g. 
;; - `@[<2+>]` (&al) —
;;     - i.e. `<2+>` matches both “2” and “two or more” or “at least two”; `<0+>` matches both “0” and “zero or more” or “any number of”.
;;     - e.g. 
;; - `@[<2->]` (&al) —
;;     - i.e. `<2->` matches both “2” and “up to two” or “at most two” or “two or fewer”.
;;     - e.g. 
;; - `@[<1..2>]` & `@[<1+2->]` (&al) —
;;     - i.e. `<1..2>` matches both “1–2” and “one to two” or “one or two” or “one or both”.
;;     - e.g. 
;; - `@[a(n)]` —
;;     - i.e. matches both “a” an “an”.
;; - 


(defvar mtgq-predicate-environment-alist "")
(defmacro mtgq-define-predicate (name args &rest body)
  (let ((FUN-NAME (intern (format "mtg-predicate/%s" (symbol-name name)))))
    `(progn
       (defun ,FUN-NAME ,args ,@body)
       (add-to-list 'mtgq-predicate-environment-alist (cons ,name ,FUN-NAME)))))
;;e.g. (mtgq-define-predicate odd (n) (if (numberp n) (cl-oddp n) nil)) → (progn (defun mtg-predicate/odd (n) (if (numberp n) (cl-oddp n) nil)) (add-to-list 'mtgq-predicate-environment-alist (cons 'odd #'mtg-predicate/odd)))


(mtgq-define-predicate odd (n)
  (if (numberp n)
      (cl-oddp n)
    nil))
(mtgq-define-predicate even (n)
  (if (numberp n)
      (cl-evenp n)
    nil))
(mtgq-define-predicate negative (n)
  (if (numberp n)
      (cl-minusp n)
    nil))
(mtgq-alias-predicate neg negative)
(mtgq-define-predicate positive (n)
  (if (numberp n)
      (cl-plusp n)
    nil))
(mtgq-alias-predicate pos positive)


;; - `` — creature cards with both a “race” creature-type and a “class” creature-type. 


;; Query Notes: 
;; - if all letters are lowercase, the query is case-insensitive. if a letter is uppercase +or if a “(mtgq :case-sensitive t)” pragma is that the end of the query), the query becomes case-sensitive
;; - 
;; - 


;;


________________




;;; ‘mtg-query-text’:


(defconst mtg-default-land
  '(:mana-cost (is nil) :colors (is []))
  "Lands are all colorless and have no mana-cost.")


(defconst mtg-default-planeswalker
  '(:types (has legendary))
  "Planeswalkers are (almost) all legendary.")


;;n.b.:
;; `:colors (is [])` → ⟨ ∅ ≡ mtg-card-colors ⟩
;; `:types (has _)` → ⟨ _ ∈ mtg-card-types ⟩
;; 
;; ∅
;; ∈ ∋ ∉ ∌ ⋶ ⋽ ⋲ ⋺ ⋳ ⋻ ∊ ∍ ⋷ ⋾ ⋴ ⋼ ⋵ ⋸ ⋹
;; ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊈ ⊉ ⊊ ⊋ ⫅ ⫆ ⫋ ⫌ ⫇ ⫈ ⫉ ⫊
;;; ≡


;;




;;


________________




MTG-GAME


;;; ‘mtg-game’:


(cl-defstruct (mtg-game-)
  (id nil :type symbol) (name nil :type string) ( nil :type ))


;;


(mtg-define-gating-condition )


threshold delirium spell-mastery ascend 


(mtg-define-scaling-condition )


devotion 


;;


(mtg-define-spellish-predicate )


evoke exploit 
channel bloodrush 


;;




(defun mtg-eval/lethal-damage-for (source sink &optional effects)
  "Return how much damage SOURCE must deal to SINK to be lethal, or nil.
For creatures with trample, burn spells with “spell-trample”, and source that cares about excess damage."
  )
;;TODO sink (- (or real-toughness real-loyalty) marked-damage)


(defun mtg-eval/lethal-damage (damage health &optional deathtouch-source-p noncreature-sink-p)
  ""
  (cond
    ((< damage 1) nil)
    ((and deathtouch-source-p (not noncreature-sink-p)) 1)
    ((> damage health) (- damage health))
    (t damage)))


(defun mtg-eval/excess-damage (damage health &optional deathtouch-source-p noncreature-sink-p)
  ""
  (if-let* ((DAMAGE damage)
            (LETHAL-DAMAGE (mtg-eval/lethal-damage damage health deathtouch-source-p noncreature-sink-p))
            (EXCESS-DAMAGE (- DAMAGE LETHAL-DAMAGE)))
      EXCESS-DAMAGE))


(defun mtg-card-health (card)
  "Return creature CARD's toughness, planeswalker CARD's loyalty, or nil."
  (mtg-card-type-case card
    (creature     (mtg-creature-card-toughness card))
    (planeswalker (mtg-planeswalker-card-loyalty card))))


(defun mtg-game-object-health (object)
  "Return OBJECT's toughness minus marked damage (creatures), loyalty (planeswalkers), life (players), or nil."
  (mtg-game-object-type-case card
    (player (mtg-player-life-total object))
    ((permanent creature) (mtg-card-toughness object))
    ((permanent planeswalker) (mtg-card-loyalty object))
    (_ nil)))


(defun mtg-game-object-damageable-p (object &optional damage)
  "Whether OBJECT can be damaged."
  (mtg-game-object-kind-case object
    (player t)
    ((permanent creature) t)
    ((permanent planeswalker) t)
    (_ nil)))


(defmacro (mtg-card-type-case &rest CONDS)
  )
(defmacro (mtg-game-object-kind-case &rest CONDS)
  )


;;


________________




MTG-FRAMES


(cl-defstruct (mtg-card-frame)
  (layout 'normal) (style '-2003) (effects nil))


(mtg-define-layout normal "")
(mtg-define-layout  "")
(mtg-define-layout  "")
(mtg-define-layout  "")
(mtg-define-layout  "")
(mtg-define-layout  "")
(mtg-define-layout  "")
(mtg-define-layout  "")


(mtg-define-frame -1993 "")
The original Magic card frame, starting from Limited Edition Alpha.
(mtg-define-frame -1997 "")
The updated classic frame starting from Mirage block
(mtg-define-frame -2003 "")
The “modern” Magic card frame, introduced in Eighth Edition and Mirrodin block.
(mtg-define-frame -2015 "")
The holofoil-stamp Magic card frame, introduced in Magic 2015.
(mtg-define-frame future "")
The frame used on cards from the future


(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")
(mtg-define-frame  "")


legendary


The cards have a legendary crown


Example Cards


miracle


The miracle frame effect


Example Cards


nyxtouched


The Nyx-touched frame effect


Example Cards


draft


The draft-matters frame effect


Example Cards


devoid


The Devoid frame effect


Example Cards


tombstone


The Odyssey tombstone mark


Example Cards


colorshifted


A colorshifted frame


Example Cards


inverted


The FNM-style inverted frame


Example Cards


sunmoondfc


The sun and moon transform marks


Example Cards


compasslanddfc


The compass and land transform marks


Example Cards


originpwdfc


The Origins and planeswalker transform marks


Example Cards


mooneldrazidfc


The moon and Eldrazi transform marks


Example Cards


waxingandwaningmoondfc


The waxing and waning crescent moon transform marks


Example Cards


showcase


A custom Showcase frame


Example Cards


extendedart


An extended art frame


Example Cards


companion


The cards have a companion frame


Example Cards


etched


The cards have an etched foil treatment


Example Cards


snow


The cards have the snowy frame effect


;; all_parts..component = ('token 'meld_part 'meld_result 'combo_piece)
;; Cards with the layouts split, flip, transform, and double_faced_token will always have a card_faces property describing the distinct faces.
;; Cards with the layout meld will always have a related_cards property pointing to the other meld parts.


________________




MTG-DATA


;;; ‘mtg-data’:


;;


(cl-defun mtg--insert-card-data (&optional (name1 'mtg-cards) (name2 name1))
  "‘insert’s NAME1 as « (defconst NAME2 [ #s(mtg-card …) … ] \"…\") »"
  (let* ((DATA  (symbol-value name1))
         (CARDS (mtg-data-data DATA))
         (META  (mtg-data-meta DATA))
         (print-escape-newlines t)
         (print-quoted t)
         (standard-output (current-buffer)))


   (cl-loop for CARD across CARDS
     initially do (insert (format "(defconst %s [\n" name2))
     do (insert " ")
        (prin1 CARD)
        (insert "\n")
     finally do (insert " ]\n \"(An ‘mtg-data-p’ of cards.)\")\n\n"))))


;;


(defun mtg--insert-card-data (name &optional how)
  "Insert an ‘mtg-cards-p’ expression (if HOW has \'expr) or declaration (if HOW has \'decl) of the data named NAME."
  (cond ((memq 'decl how) (mtg--insert-card-data-declaration name how))
        ((memq 'expr how) (mtg--insert-card-data-expression name how))
        (t (mtg--insert-card-data-declaration name how))))


(defun mtg--insert-card-data-expression (name &optional how)
  "Insert an ‘mtg-cards-p’ expression of the data named NAME."
  ())


(defun mtg--insert-card-data-declaration (name &optional how)
  "Insert an ‘mtg-cards-p’ declaration of the data named NAME."


  (let* ((CARDS ())
         (META  ())
         )
    (let ((standard-output (current-buffer)))


      (progn
        (insert (format "(defconst %s %s\n" name (if 'read "[" if 'make "(vector ")))
        (cl-loop for CARD across CARDS
          do (insert "  ")
             (mtg--insert-card-datum CARD how)
        (insert (format "%s)\n\n"  (if 'read "]" if 'make ")"))))
        ()))))


(defun mtg--insert-card-datum (card &optional how)
  ""
  (cond ((memq 'read how) (mtg--insert-card-datum-literal card))
        ((memq 'make how) (mtg--insert-card-datum-constructor card)
        (t (mtg--insert-card-datum-literal card))))


(defun mtg--insert-card-datum-literal (card)
  "Write the CARD `read'ably as ‘mtg-card-p’ struct read-syntax (“#s(…)").
n.b. Assumes `standard-output' is `current-buffer'."
  (insert (format "%S" card)))


(defun mtg--insert-card-datum-constructor (card)
  "Write the CARD `read'ably as a ‘mtg-make-card’ funcall.
n.b. Assumes `standard-output' is `current-buffer'."
  ())


;;


;;


________________




MTG-JSON


(require 'url)


;;M-: (url-copy-file "https://mtgjson.com/downloads/VintageAtomic.json" "mtg-cards.json" t)


________________




MTG-DEFINE-PROPERTY


(mtg-define-property 
  )


________________




MTG-


;;; ‘mtg-’:


;;


________________




MTG-PRINT


;;; ‘mtg-print’:


;;


(defun mtg-pp-xyz (xyz-id)
  "Pretty-print the XYZ-ID."
  ( mtg-format-xyz (mtg-get-xyz xyz-id)))


(defun mtg-format-xyz (xyz)
  "Format XYZ."
  (let* ((NAME (propertize (mtg-xyz-name xyz) '(:weight . bold)))
         (TEXT (propertize (mtg-xyz-text xyz) '(:style . italic)))
    (format "%s %s" NAME TEXT)))


;; '((:style . italic) '(:weight . bold))
;;


(defun mtg-format-keyword (keyword)
  "Format KEYWORD."
  (let* ((NAME (propertize (mtg-keyword-name keyword) '(:weight . bold)))
         (REMINDER-TEXT (propertize (format "(%s)" (mtg-keyword-reminder-text keyword)) '(:style . italic)))
    (format "%s %s" NAME REMINDER-TEXT)))


;;


(defun mtg-format- ()
  "Format ."
  (let* ((NAME (propertize (mtg--name ) '(:weight . bold)))
         (TEXT (propertize (mtg--text ) '(:style . italic)))
    (format "%s %s" NAME TEXT)))


(defun mtg-format- ()
  "Format ."
  (let* ((NAME (propertize (mtg--name ) '(:weight . bold)))
         (TEXT (propertize (mtg--text ) '(:style . italic)))
    (format "%s %s" NAME TEXT)))


;;




________________




MTG-DEFINE-QUERY


(mtg-defquery is-token-producer-consumer
  (and (mtg-rx TEXT (: "create" token)) (mtg-rx TEXT (: (or "sacrifice … token" "tap … token you control" "the number of … tokens you control" "for each … token you control") token)))
  "")


;;(mtg-defquery NAME
;;  …
;;  "")
;;→
;;(progn
;;  (defun mtg-query/NAME (card)
;;    ""
;;    (mtg-with-card card
;;      …))
;;  (put NAME 'mtg-query mtg-query/NAME))


(mtg-define-query animates
  `(or (and (mtg-card-had-type CARD 'vehicle) (mtg-card-powtou CARD))
       (save-match-data (and (mtg-card-has-text CARD "(~|it) becomes a 0/0 _ _ creature" "put $1 +1/+1 counters on (~|it)") (mtg-make-powtou :pow (match 1) :tou (match 2))))
       (save-match-data (and (mtg-card-has-text CARD "~ becomes a $1/$2 _ _ creature") (mtg-make-powtou :pow (match 1) :tou (match 2))))
       ) "")


________________




;; as plist:


'(:n _ :w _ :u _ :b _ :r _ :g _ :x _ …)


(cadr (memq :u MANA))


;; as alist:


'((n . _) (w . _) (u . _) (b . _) (r . _) (g . _) (x . _) … (raw . ""))


(cdr (assq 'u MANA))


(setf (alist-get 'u MANA) (1+ (alist-get 'u MANA)))


;; as wide struct:


(cl-defstruct mtg-mana-cost n W U B R G X C WU UB BR RG GW WB BG GU UR RW 2W 2U 2B 2R 2G pW pU pB pR pG … raw)  ; W U B R G


;; as deep struct:


(cl-defstruct mtg-mana-cost
   )


(mtg-mana-cost-U MANA)


(cl-callf 1+ (mtg-mana-cost-U MANA) )
(cl-incf (mtg-mana-cost-U MANA))


;; 


________________




mtg-card-extra


(mtg-card-get k o)
(or (slot o k) (assq k (slot o 'extra)))


gv-define-simple-setter


________________




;;; ‘mtg-cost’:


"
For example, the card ‹Sinister Concoction› has an activated ability costing « {B}, Pay 1 life, Put the top card of your library into your graveyard, Discard a card, Sacrifice ~ ». :


• « {b} » — 
• « Pay 1 life » — 
• « Put the top card of your library into your graveyard » — 
• « Discard a card » — 
• « Sacrifice ~ » — 


which can be parsed into this ‘mtg-cost’:


    M-: (make-mtg-cost :mana [b] :life 1 :mill 1 :discard (1))
"


'(
  ;; Player costs (i.e. stuff YOU pay or do):
  (life 0)  ; e.g. 2 means “Pay 2 life”.
  (discard 0)  ; e.g. 2 means “Discard two cards”.
  (mill 0)  ; e.g. 2 means “Put the top two cards of your library into your graveyard”.
  (cremate 0)  ; e.g. 2 means “Exile two cards from your graveyard”.
  (energy 0)  ; e.g. 2 means “{E}{E}”.
)


(defconst mtg-pay-life-rx
  (rx (or (: "Pay" (1+ space) (group-n 1 mtg-numeral) (1+ space) "life")
          (: "Pay life equal to" (group-n 1 mtg-numeric-phrase))))
  "")


(define-rx (mtg-pay-n-life-cost &optional rx)
   `(or (: "Pay" (1+ space) ,(if rx rx 'mtg-numeral) (1+ space) "life"))))


(define-rx (mtg-pay-life-cost &optional n group)
   `(or (: "Pay" (1+ space) (group-n ,group mtg-numeral) (1+ space) "life")
       (: "Pay life equal to" (group-n ,group mtg-numeric-phrase))))


________________




(cl-defstrut mtg-print (:inherit mtg-card)
  )


(cl-defstrut mtg-maxcard (:inherit mtg-mincard)
  )


________________




‘mtg-card’ variants:


- ‘mtg-card’ — 
- ‘mtg-split-card’ — 


- ‘mtg-level-card’ — e.g `(make-mtg-leveller-card :name "Coralhelm Commander" :cost (make-mtg-cost :mana [u u]) :pow 2 :tou 2 :text [] :range (0 . 1) :level-up (make-mtg-cost :mana [1]) :levels (list (make-mtg-level :range (2 . 3) :pow 3 :tou 3 :text (make-mtg-text :keywords [flying])) (make-mtg-level :range (4 . t) :pow 4 :tou 4 :text (make-mtg-text :keywords [flying] :lines ["Other Merfolk creatures you control get +1/+1."])))) …)`


- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 
- ‘mtg--card’ — 


(cl-defgeneric mtg-mana-cost (object)
  "Return OBJECT's mana-cost, if any (an ‘mtg-mana-cost’ or nil).
")


(cl-defmethod mtg-mana-cost ((this mtg-card))
  (mtg-card-mana-cost this))


(cl-defun mtg-card-mana-cost (this)
  (mtg-card-get 'mana-cost this))


(cl-defmethod mtg-mana-cost ((this mtg-face))
  (mtg-face-mana-cost this))


(cl-defmethod mtg-mana-cost ((this mtg-keyword-ability))
  (mtg-keyword-ability-mana-cost this))


(cl-defmethod mtg-mana-cost ((this mtg-activated-ability))
  (mtg-activated-ability-mana-cost this))


;;(cl-defmethod mtg-xyz ((this mtg-abc))
;;  (mtg-abx-xyz this))
;;
;;(cl-defun mtg-card-mana-cost (this)
;;  (mtg-abc-get 'xyz this))


(cl-defmethod mtg-mana-cost ((this mtg-cost))
  this)


(cl-defun mtg-cost-mana (this)
  (mtg-cost-get 'mana this))


(cl-defmethod mtg-mana-cost ((this mtg-mana-cost))
  this)


(cl-defmethod mtg-mana-cost ((this symbol))
  (when-let* ((CARD (mtg-symbol-value this 'card)))
    (mtg-card-mana-cost CARD)))


________________




multiface cards:


- See Comp Rules 709.–717 and .
- 


;; Layouts:
;;
;; • split — . split card's color/value merges faces' colors/values. c.f. adventure cards, whose color/value aren't merged (they're just the main face's). See https://www.yawgatog.com/resources/magic-rules/#R709. 
;; • aftermath — . See https://www.yawgatog.com/resources/magic-rules/#R71.
;; • adventure — . See https://www.yawgatog.com/resources/magic-rules/#R716. 
;; • flip — . See https://www.yawgatog.com/resources/magic-rules/#R710. 
;; • transform — . See https://www.yawgatog.com/resources/magic-rules/#R712. 
;; • modal-dfc — . See https://www.yawgatog.com/resources/magic-rules/#R71.
;; • meld — . See https://www.yawgatog.com/resources/magic-rules/#R713. 
;; • level — . See https://www.yawgatog.com/resources/magic-rules/#R711. 
;; • class — . See https://www.yawgatog.com/resources/magic-rules/#R717. 
;; • saga — . See https://www.yawgatog.com/resources/magic-rules/#R715. 
;; •  — 
;; 


split, aftermath, adventure: each subcard has own everything (mana-cost (& color), ).


flip: upside-down subcard has no mana-cost.


transform, meld, modal-dfc: face-down subcard has no mana-cost (back's mana-value is front's, but its color can be own via color-indicator).


level, class, saga: each subcard has own text & p/t.


709.4a. Each split card has two names. If an effect instructs a player to choose a card name and the player wants to choose a split card's name, the player must choose one of those names and not both. An object has the chosen name if one of its names is the chosen name.
709.4b. The mana cost of a split card is the combined mana costs of its two halves. A split card's colors and mana value are determined from its combined mana cost. An effect that refers specifically to the symbols in a split card's mana cost sees the separate symbols rather than the whole mana cost.
709.4c. A split card has each card type specified on either of its halves and each ability in the text box of each half.


710.1b. The bottom half of a flip card contains an alternative name, text box, type line, power, and toughness.
710.1c. A flip card's color and mana cost don't change if the permanent is flipped.


711.1. Each leveler card has a striated text box and three power/toughness boxes. The text box of a leveler card contains two level symbols.




________________




Dig Up
{G} Sorcery
Cleave {1}{B}{B}{G} (You may cast this spell for its cleave cost. If you do, remove the words in square brackets.)
"Search your library for a [basic land] card, [reveal it,] put it into your hand, then shuffle."


(defun mtg-card-uncleave (card)
  ""
  (pcase-let* ((`(CLEAVE-ABILITY . TEXT) (mtg-card-split-cleave-text (mtg-card-text card))))
  (let* ((CASTING-COST (mtg-card-cost card))
         (CLEAVE-COST  (cadr CLEAVE-ABILITY))
         (NORMAL-TEXT (mtg-card-decleave-text TEXT))
         (CLEAVE-TEXT (mtg-card-recleave-text TEXT)))
    (let ((UNCLEAVE-CARD (mtg-card-set (copy-mtg-card card) :cost NORMAL-COST :text NORMAL-TEXT))
          (CLEAVE-CARD   (mtg-card-set (copy-mtg-card card) :cost CLEAVE-COST :text CLEAVE-TEXT)))
      (list UNCLEAVE-CARD CLEAVE-CARD)))))


(defun mtg-card-recleave-text (text)
  ""
  (replace-regexp-in-string (rx (1+ space)) " " (replace-regexp-in-string (rx "[" (1+ (not (char ?]))) "]") "" text)))


(defun mtg-card-decleave-text (text)
  ""
  (replace-regexp-in-string (rx (1+ space)) " " (replace-regexp-in-string (rx (char ?[ ?])) "" text)))


;; M-: (mtg-card-uncleave (make-mtg1-card :mana-cost [g] :text [(cleave [1 b b g]) "Search your library for a [basic land] card, [reveal it,] put it into your hand, then shuffle."]))
;;     (list (make-mtg1-card :mana-cost [g] :text ["Search your library for a basic land card, reveal it, put it into your hand, then shuffle."]) (make-mtg1-card :mana-cost [1 b b g] :text ["Search your library for a card, put it into your hand, then shuffle."]))


(defun mtg-card-split-cleave-text (text)
  )


;;


(defun mtg-card-unkicker (card)
  )


;; M-: (mtg-card-unkicker )
;;     (list )


(defun mtg-card-un (card)
  )


;; M-: (mtg-card-un )
;;     (list )


________________




(defun mtg-json--hash-file (file)
  (secure-hash 'sha256 (with-temp-buffer (insert-file-contents file))))


"https://mtgjson.com/file-models/%s"
https://mtgjson.com/file-models/card-atomic/
https://mtgjson.com/file-models/printing/


"https://mtgjson.com/api/v5/%s.json.sha256"
https://mtgjson.com/api/v5/VintageAtomic.json.sha256
https://mtgjson.com/api/v5/AllPrintings.json.sha256


________________




(cl-deftype unsigned-byte (&optional bits)
  (let ((BITS (if (eq '* bits) bits (1- (ash 1 bits))))
    `(integer 0 ,BITS)))
;; (unsigned-byte bits) → (list 'integer 0 (if (eq bits '*) bits (1- (ash 1 bits)))))


________________




;;; ‘mtg-print’:


  (edition       nil)
  (ccn           nil)
  (rarity        nil)
  (flavor-text   nil)
  (artist        nil)
  (border-color  nil)
  (frame-style   nil)
  (image         nil)
  (lang          nil)


;;^ ‘(mtg0-card-slots)’ can have an ‘mtg0-card-name’ symbol, to resolve the slot value from. n.b. these must form a DAG (directed a cyclic graph) at most. used by multiface cards. for example, .


;;


:id (mtg0-cardprint-id-from .name .set .ccn)


(defun mtg0-cardprint-id-from (name edition collectors-number)
  ""
  (let (( ()))
    ()))


:side (mtg0-card-side-parse .layout .side)


(defun mtg0-card-side-parse (layout side)
  ""
  (pcase `(,layout . ,side) (
      (`(split . a) 'left)
      (`(split . b) 'right)
      (`(aftermath . a) ')
      (`(aftermath . b) ')
      (`(adventure . a) ')
      (`(adventure . b) ')
      (`(flip . a) 'straight-up)
      (`(flip . b) 'upside-down)
      (`(dfc . a) 'front)
      (`(dfc . b) 'back)
      (`(meld . a) 'front-)
      (`(meld . b) 'front-)
      (`(meld . c) 'back)
      ;; (`( . a) ')
      ;; (`( . b) ')
      (`(normal . _) nil)))


________________




;;; ‘mtg-json’


URL ‘https://mtgjson.com/file-models/set/’


(defconst mtg-json-known-xyzs
  '(…)
  "Known from ‘.xyz’, see URL ‘https://mtgjson.com/file-models/set/#xyz’")


(defconst mtg-json-known-xyzs
  '(…)
  "Known from ‘.xyz’, see URL ‘https://mtgjson.com/file-models/set/#xyz’")


(defconst mtg-json-known-edition-codes
  '(…)
  "Known from ‘.code’, see URL ‘https://mtgjson.com/file-models/set/#code’")


(defconst mtg-json-known-edition-types
  '(expansion core commander funny masterpiece)
  "
from ‘.code’, ‘parentCode’, or ‘.mtgoCode’; see URL ‘https://mtgjson.com/file-models/set/#code’")


(defconst mtg-json-known-edition-names
  '(…)
  "
from ‘.name’, see URL ‘https://mtgjson.com/file-models/set/#name’")


(defconst mtg-json-known-block-names
  '(…)
  "
from ‘.block’, see URL ‘https://mtgjson.com/file-models/set/#block’")


(defconst mtg-json-known-token-types
  '()
  "
have ‘.reverseRelated’ (the names of the cards which create the token).
from ‘.tokens’; see URL ‘https://mtgjson.com/file-models/set/#tokens’")


(defconst mtg-json-keyrune-
  '
  "
from ‘.keyruneCode’
URL ‘https://keyrune.andrewgioia.com/’")


(defun mtg-json-parse-date (date)
  "
in ISO 8601 format; see URL ‘https://www.iso.org/iso-8601-date-and-time-format.html’
from ‘.releaseDate’; see URL ‘https://mtgjson.com/file-models/set/#releaseDate’"
  (iso8601-parse date))


(or .text (and .isTextless []))


(defconst mtg-json-known-offensive-cardprints
  '(pradesh-gypsies-* invoke-prejudice-leg imprison-leg jihad-* crusade-* cleanse-* stone-throwing-devils-*)
  "
For example:
• “Pradesh G***ies” has an offensive name.
• “Invoke Prejudice” () has an offensive artwork.
from ‘.hasContentWarning’; see URL ‘https://magic.wizards.com/en/articles/archive/news/depictions-racism-magic-2020-06-10’")


("Pradesh Gypsies" . "Pradesh G•••ies")


;; Card-Name fields: ‘.faceName’, ‘.name’, ‘.asciiName’, ‘.flavorName’.
;; 


(or .colors [])


;; .colorIdentity — all colors in ‘.manaCost’, ‘.colorIndicator’, and ‘.text’.


________________




;;; ‘mtg-image’


(defun mtg0-scryfall-image-url (scryfall-id)
  ""
  (format " https://api.scryfall.com/cards/%s?format=image" scryfall-id))


(defun mtg0-gatherer-image-url (scryfall-id)
  ""
  (format " https://gatherer.wizards.com/Handlers/Image.ashx?type=card&multiverseid=%s" multiverse-id))
;; &face=front &face=back


________________




;;; ‘mtg-query’


• $   mana-cost — 
• $$  mana-value — 
• $&  additional-cost — 
• $|  alternative-cost — 
• $!  true-cost — 
• $$$ -cost — 
• $   -cost — 






________________




;;; ‘mtg-result’


(defcustom mtg0-result-cardprint-quotient-merging-list
  '(name edition artwork)
  "By default, don't merge two printings unless they come from the same set and have the same art (and obviously, have the same name)."
  )


;; :
;;
;; • by name — group prints by card-names (duh).
;; • by edition — group only if they're printed in the same set.
;; • by artwork — group only if they have the same artwork 
;; • by rarity — group only if they have the same rarity.
;; • by  — group only if   
;; • by  — group only if   
;; • by  — group only if   
;; • by  — group only if   
;;


(defcustom mtg0-result-cardprint-quotient-picking-list
  '(english newest)
  "By default, pick the newest english-language printing from multiple printings."
  )


;; :
;;
;; • by date — e.g. keep the newest printing; e.g. keep the the oldest printing.
;; • by language — e.g. keep the English printing if available.
;; • by  — 
;; • by  — 
;;


(defun mtg0-result-cardprints-quotient (prints merger picker)
  "Drop equivalent results from PRINTS by their MERGER.


Inputs:
• MERGER — By default, MERGER is ‘mtg0-result-cardprint-quotient-merging-list’. If MERGER is nil, drop nothing.
• PICKER — By default, PICKER is ‘mtg0-result-cardprint-quotient-picking-list’.


For example, if MERGER is and PICKER is , . e.g. M-: (mtg0-result-cardprints-quotient … '(name artwork) '(oldest))"


  (mtg0-result-cardprints-quotient-pick (mtg0-result-cardprints-quotient-group prints merger) picker))


(defun mtg0-result-cardprints-quotient-group (prints merger)
  "Group PRINTS into non-empty lists along MERGER."
  )


(defun mtg0-result-cardprints-quotient-pick (prints-list picker)
  "Keep one print per PRINTS-LIST sublist via PICKER."
  )


________________




;;; ‘mtg-result’


(defcustom mtg0-result-sorting-alist
   `((name . (,#'mtg-card-name ,#'string-lessp ,#'string-greaterp))
     (mana-value . (,#'mtg-card-mana-value ,#'< ,#'>))
     (color . (,#'mtg-card-colors ,#' ,#'))
     ( . (,#'mtg-card- ,#' ,#'))
     ( . (,#'mtg-card- ,#' ,#'))
     ( . (,#'mtg-card- ,#' ,#'))
     ( . (,#'mtg-card- ,#' ,#'))
     (date . (,#'mtg-card-original-release-date ,#'mtg-date--less-p ,#'mtg-date--greater-p))
     (mana-cost . (,#'mtg-card- ,#' ,#')))
  ""
  )


;; Results order:
;;
;; • by name — 
;; • by mana-value — 
;; • by color — any permutation of {W,U,B,R,G} (in particular, starting from any color). by default, W→U→B→R→G.
;; • by date — 
;; • by type — any permutation of {}.  by default, primary order Spell→Creature→Planeswalker→Artifact→Land and secondary order Legendary→Snow.
;; • by strength ← (max POW TOU LOY) — 
;; • by devotion ← (COST) — 
;; • by  — 
;; • by  — 
;; • by  — 
;; • by  — 
;; • by  — 
;; • by  — 
;; • by  — 
;; • by  — 
;; • by color-identity — 
;; • by color-personality — 
;; • by mana-cost — {0}→{9} {9}→{0}
;;


(defun mtg0-sort-cards (cards order dir)
  "Sort CARDS by ORDER in DIR."
  (cl-destructing-bind (PROJECT COMPARE-ASC COMPARE-DESC) (mtg0-results-sorting-alist-get order dir)
    (cl-sort cards COMPARE :key PROJECT)))


(defun mtg0-sort-cards-by-name-from-A-to-Z (cards)
  "Sort CARDS by their names in ascending lexicographic order (A→Z)."
  (mtg0-sort-cards cards 'name 'a-z))


;;i.e. (cl-sort cards #'string-lessp :key #'mtg-card-name)


(defun mtg0-sort-cards-by-name-from-Z-to-A (cards)
  "Sort CARDS by their names in descending lexicographic order (Z→A)."
  (mtg0-sort-cards cards 'name 'z-a))


;;i.e. (cl-sort cards #'string-greaterp :key #'mtg-card-name)


(defun mtg0-card- (card)
  " CARD."
  ())


(defun mtg0-results-sorting-alist-get (order dir)
  "How to sort cards by ORDER in DIR."
  (cl-destructing-bind (PROJECT COMPARE-ASC COMPARE-DESC) (assq order mtg0-results-sorting-alist)
    (let ((COMPARE
            (pcase dir
              ((or 'a-z '0-9) COMPARE-ASC)
              ((or 'z-a '9-0) COMPARE-DESC))))
      (list PROJECT COMPARE))))


(cl-sort _ #'string-lessp    :key #'mtg-card-name)  ; A→Z
(cl-sort _ #'string-greaterp :key #'mtg-card-name)  ; Z→A


(cl-sort _ #'< :key #'mtg-card-value)  ; 0→9
(cl-sort _ #'> :key #'mtg-card-value)  ; 9→0


________________




(require 'time-date)


(defun mtg-date--pp (date-or-edition)
  "
M-: (mtg-date--pp 'lea)
    \"1993 Aug\"
"
  (let ((date (cl-typecase date-or-edition
                (string date-or-edition)
                (symbol (mtg-edition-get-date (mtg-edition-get date-or-edition))))))
    (format-time-string "%Y %m" date)))


(defun mtg-date--less-p (x y)
  "
e.g. (mtg-date--less-p \"2021-04-15 13:00:00\" \"2021-04-16 12:00:00\")"
  (time-less-p (date-to-time x) (date-to-time y)))


(defun mtg-date--greater-p (x y)
  "
e.g. (mtg-date--greater-p \"2021-04-15 13:00:00\" \"2021-04-16 12:00:00\")"
  (time-greater-p (date-to-time x) (date-to-time y)))


(cl-defun mtg-date--within-p (date1 date2 &key day month year)
  (<= (abs (days-between date1 date2))
      (time-to-number-of-days (make-decoded-time :day day :month month :year year))))


(cl-defun mtg-date--within-p (date1 date2 &key day month year)
  (let ((TIME-DELTA (make-decoded-time :day day :month month :year year)))
    (or ( date1 (time-subtract date2 TIME-DELTA))
        ( date1 (time-add date2 TIME-DELTA))))))


;; (format-seconds STRING SECONDS)
;;
;; The valid format specifiers are:
;; • %y is the number of (365-day) years.
;; • %d is the number of days.
;; • %h is the number of hours.
;; • %m is the number of minutes.
;; • %s is the number of seconds.
;; • %z is a non-printing control flag (see below).
;;




________________




mtg-decklist.el


(thing-at-point 'mtg-card-name)


(defun mtg-card-name-at-point ()
  )


(defun mtg-decklist--card-complete-at-point ()
  "The ‘completion-at-point-functions’ function for MTG cards."
  (let ((START (mtg-decklist--start-of-card-name-at-point)))
    (when START
      (list START (point) (mtg-decklist--card-names-in-format (mtg-decklist--current-format))
            :exclusive 'yes
            :company-docsig #'identity
            :company-doc-buffer #'mtg-decklist--company-doc-buffer))))


(defun mtg-decklist--company-doc-buffer (card-name)
  "Produce a `company-doc-buffer' for CARD-NAME in FORMAT."
  (let ((CARD (mtg-decklist-pp-card (mtg-decklist--get-card-by-name card-name))))
    (company-doc-buffer CARD)))


(defun mtg-read-card (&optional prompt card-names)
  "Read a card by its name."
  (interactive)
  (let* ((PROMPT "Card: ")
         (CARD-NAMES (mtg-card-names))
         (CARD (completing-read PROMPT CARD-NAMES)))
    CARD)))


________________




(defun mtg0-card-text-parse-leveler (rules-lines &optional size)
  (cl-loop for LINE across rules-lines
    ()))


[(leveler [[2]] ["Level up {2}"] [(nil . 0) [0 3] []] [(1 . 4) [0 6] []] [(5 . nil) [0 6] ["Islandwalk"]])]


;;e.g. (mtg0-card-text-parse-leveler ["Level up {2} ({2}: Put a level counter on this. Level up only as a sorcery.)" "LEVEL 1-4" "0/6" "LEVEL 5+" "6/6" "Islandwalk"])
;;     [(leveler [[2]] ["Level up {2}"] [(nil . 0) [0 3] []] [(1 . 4) [0 6] []] [(5 . nil) [0 6] ["Islandwalk"]])]
;;
;;n.b. case-sensitive “LEVEL”.
;;


;;^ i.e. “Halimar Wavewatch”: {"text": "Level up {2} ({2}: Put a level counter on this. Level up only as a sorcery.)\nLEVEL 1-4\n0/6\nLEVEL 5+\n6/6\nIslandwalk", "power": "0", "toughness": "3"} → ["Level up {2} ({2}: Put a level counter on this. Level up only as a sorcery.)" "LEVEL 1-4" "0/6" "LEVEL 5+" "6/6" "Islandwalk"] → ["Level up {2}" (leveler [[2]] [(nil . 0) (0 . 3) ""] [(1 . 4) (0 . 6) []] [(5 . nil) ( . ) ["Islandwalk"]])] OR ["Level up {2}" (leveler [(nil . 0) (0 . 3) ""] [(1 . 4) (0 . 6) []] [(5 . nil) ( . ) ["Islandwalk"]])]


Level up {2} ({2}: Put a level counter on this. Level up only as a sorcery.)
LEVEL 1-4
0/6
LEVEL 5+
6/6
Islandwalk (This creature can’t be blocked as long as defending player controls an Island.)


;;


________________




(
 '(spiritual . (spirit enchantment))  ;e.g. Geistlight Snare
)


________________




;;yasnippet


${1:(condition-case () (…) (error ""))}




________________


ns-speech-recognizer.el
nssr.el


(defvar nssr-recognizer nil
  "")


(defcustom nssr-recognized-hook (list #'nssr-insert-recognized)
  ""
  :type (hook :tag "" (nssr-insert-recognized nssr-message-recognized nssr-execute-recognized))
  :group 'nssr)


(defcustom nssr-recognitions-buffer-name "*NSSR Recognitions Log*"
  "")


(defcustom nssr-recognizeables-buffer-name "*NSSR Dictation Commands*"
  "")


(defun nssr-insert-recognized (command)
  ""
  (insert command)
  ;;TODO atomically-undoable
  )


(defun nssr-execute-recognized (command)
  ""
  ( command))


(defun nssr-navigate-recognized (command)
  ""
  ( command))


(defun nssr-message-recognized (command)
  ""
  (message command))


(cl-defun nssr ((commands nil) &key (exclusive-p nil) (foreground-only-p t) (handler #'…) (title "Emacs"))
  "


e.g. (nssr RECOGNIZER :exclusivity t :foreground-only t :commands '(\"start listening\" \"stop listening\" \"\") :on-recognition (list #'message))"


  (interactive ( ": ") (y-or-n-p "Exclusive? ") (y-or-n-p "Foreground only? ") (read-function "Handler for recognized command: ") (read-string "Title: "))


  (let ((RECOGNIZER (nssr-recognizer-start)))
    (nssr-recognizer-set RECOGNIZER exclusive-p foreground-only-p title commands)   
    t))


;;e.g. (nssr RECOGNIZER :exclusive t :foreground-only t :commands '("start listening" "stop listening" "…") :on-recognition (list #'message))"


(cl-defun nssr-start ()
  ""
  (when-let ((RECOGNIZER (nssr_recognizer_init)))
    (setq nssr-recognizer RECOGNIZER)
    t))


(cl-defun nssr-stop ()
  ""
  (when (nssr_recognizer_stop)
    (setq nssr-recognizer nil)
    t))


(defun nssr-inserter ()
  "Listen for _ to insert them."
  ())


(defun nssr-executor ()
  "Listen for emacs commands to execute them"
  ())


(defun nssr-navigator ()
  "Listen for words on screen to jump to them."
  ())


(defun nssr-toggler ()
  "Listen inclusively for “start”/“stop” to toggle the system default recognition & insertion."
  ())


(defun nssr-r ()
  "Listen for  to ."
  ())


(cl-defun nssr-recognizer-set (recognizer &key commands exclusive-p foreground-only-p on-recognition title)
  " (high-level)."
  (cl-type-check recognizer 'user-ptr "")
;;(cl-type-check commands '(list-of string) "") ;TODO
  (cl-type-check commands 'list "") 
  (cl-type-check exclusive 'boolean "")
  (cl-type-check foreground-only 'boolean "")
  (cl-type-check title 'string "")
  (cl-type-check on-recognition 'function "")


  (nssr_recognizer_set
    recognizer
    (not (not exclusive-p))
    (not (not foreground-only-p))
    ( commands)
    (length commands))


  (cl-loop for FUNCTION in on-recognition
    (add-hook 'nssr-recognized-hook FUNCTION))


  recognizer)


(cl-deftype nonempty-list-of (elem-type)
  `(and (list-of ,elem-type)
        (satisfies (lambda (list) (not (null list))))))
(cl-typep '(1 2 3) '(nonempty-list-of number)) ;; => t
(cl-typep nil '(nonempty-list-of number))      ;; => nil
It 


(nssr-start-recognizer)
(nssr-stop-recognizer)
(nssr-with-recognizer)


(defun nssr-recognizer-set (recognizer listens-in-foreground-only blocks-other-recognizers displayed-commands-title commands)
  " (low-level).


• RECOGNIZER: ‘user-ptr-p’ ()


• COMMANDS: ‘listp’ of ‘stringp’s


Each ‘stringp’ is a command (can be a word or a phrase) that the RECOGNIZER should listen for.


• LISTENS-IN-FOREGROUND-ONLY: ‘booleanp’


Whether the RECOGNIZER should only enable its COMMANDS when its application (i.e. Emacs?) is the frontmost one.


• BLOCKS-OTHER-RECOGNIZERS: ‘booleanp’


Whether the RECOGNIZER should block all other recognizers when listening (that is, other applications attempting to understand spoken commands).


• DISPLAYED-COMMANDS-TITLE: ‘stringp’ or nil


The title of the commands section in the Speech Commands window, or nil for no title.


• CALLBACK: ‘functionp’


See URL ‘https://developer.apple.com/documentation/appkit/nsspeechrecognizer’."


  ())


(defun nssr_recognizer_set (recognizer listensInForegroundOnly blocksOtherRecognizers displayedCommandsTitle callback commands nCommands)
  "


• RECOGNIZER: NSSpeechRecognizer


• COMMANDS: [String]?


An array of strings defining the commands for which the speech recognizer object should listen.


• DISPLAYEDCOMMANDSTITLE: String?


The title of the commands section in the Speech Commands window or nil if there is no title.


• LISTENSINFOREGROUNDONLY: Bool


A Boolean value that indicates whether the speech recognizer object should only enable its commands when its application is the frontmost one.


• BLOCKSOTHERRECOGNIZERS: Bool


A Boolean value that indicates whether the speech recognizer object should block all other recognizers (that is, other applications attempting to understand spoken commands) when listening.


See URL ‘https://developer.apple.com/documentation/appkit/nsspeechrecognizer’."


  ())


(defun nssr--set-commands (recognizer commands-list)
  )


(defun nssr--set-exclusivity (recognizer exclusivity-p)
  )


(defun nssr--set-foreground-only (recognizer foreground-only-p)
  )


(defun nssr--add-recognition-hook (recognizer function)
  )


________________


emacs-ns-speech-recognizer.m
enssr.m


/* NSSpeechRecognizer


var commands: [String]?
An array of strings defining the commands for which the speech recognizer object should listen.
Setting this property when the speech recognizer is already listening means that the current command list is updated and listening continues. The items in the array should be NSString objects. The command strings must match the current locale of the recognizer, which is selected in the Dictation pane of Accessibility system preferences.


NSString
A static, plain-text Unicode string object that bridges to String; use NSString when you need reference semantics or other Foundation-specific behavior.


String
A Unicode string value that is a collection of characters.


var displayedCommandsTitle: String?
The title of the commands section in the Speech Commands window or nil if there is no title.
When this property is a non-empty string, commands are displayed in the Speech Commands window indented under a section with this title. If title is nil or an empty string, the commands are displayed at the top level of the Speech Commands window. This default is not to display the commands under a section title.


var listensInForegroundOnly: Bool
A Boolean value that indicates whether the speech recognizer object should only enable its commands when its application is the frontmost one.
When the value of this property is true, the speech recognizer’s commands are only recognized when the speech recognizer’s application is the frontmost application—typically the application displaying the menu bar. If the value of the property is false, the commands are recognized regardless of the visibility of the application, including agent applications (agent applications, which have the LSUIElement property set, do not appear in the Dock or Force Quit window). The default value of this property is true.


var blocksOtherRecognizers: Bool
A Boolean value that indicates whether the speech recognizer object should block all other recognizers (that is, other applications attempting to understand spoken commands) when listening.
When the value of this property is true, all other speech recognition commands on the system are disabled until the speech recognizer is released, listening is stopped, or the property is set to false. Setting this property to true effectively takes over the computer at the expense of other applications using speech recognition, so you should use it only in circumstances that warrant it, such as when listening for a response important to overall system operation or when an application is running in full-screen mode (such as games and presentation software). The default is value of this property is false.


var delegate: NSSpeechRecognizerDelegate?
The delegate for the speech recognizer object.
func speechRecognizer(NSSpeechRecognizer, didRecognizeCommand: String)


*/


#include <assert.h>
#include <stdbool.h>
#include <stddef.h>


#import <Foundation/Foundation.h>
#import "Recognizer.h"


#include <emacs-module.h>


bool plugin_is_GPL_compatible = true;


//


int
emacs_module_init (struct emacs_runtime *rt)
{
  assert (rt->size > 0);
  emacs_env env = rt->emacs_env;


  ;
}


//


bool
nssr_recognizer_set (Recognizer* recognizer, BOOL blocksOtherRecognizers, BOOL listensInForegroundOnly, const char [] displayedCommandsTitle, const char* [] commands, int nCommands)
{


  recognizer. = ;
  recognizer. = ;
  recognizer. = ;
  recognizer. = ;


}


//






//


bool
emacs_ (struct emacs_env *env)
{
  Recognizer* r = [Recognizer new];


setHandler_NSSpeechRecognizer(r, PrintRecognition);
    setCommands_NSSpeechRecognizer(r, commands, length);
    setExclusivity_NSSpeechRecognizer(r, YES);
    setForegroundOnly_NSSpeechRecognizer(r, NO);
    start_NSSpeechRecognizer(r);


  beginRunLoop();   
  return 0;
}


//












    const int length = 2;
    const char* commands[length];
    commands[0] = "stop listening";
    commands[1] = "start listening";
        
}


// @autoreleasepool {}
//    [r.recognizer setCommands:@[@"stop listening",@"start listening"]];




emacs_setCommands (struct emacs_env *env)
{
  Recognizer* r = [Recognizer new];


}
// @autoreleasepool {}
//    [r.recognizer setCommands:@[@"stop listening",@"start listening"]];




Recognizer* new_NSSpeechRecognizer();


void free_NSSpeechRecognizer(Recognizer*);


void start_NSSpeechRecognizer(Recognizer*);


void stop_NSSpeechRecognizer(Recognizer*);


void setExclusivity_NSSpeechRecognizer(Recognizer*, BOOL);


void setForegroundOnly_NSSpeechRecognizer(Recognizer*, BOOL);


void setCommands_NSSpeechRecognizer(Recognizer*, const char* [], int length);


void setHandler_NSSpeechRecognizer(Recognizer* this, void(*handler)(const char*));


________________




/* See ‹emacs-module.h›. */


#include <assert.h>
#include <stdbool.h>
#include <stddef.h>


#include <emacs-module.h>


static bool have_intern;
static bool have_funcall;


bool plugin_is_GPL_compatible = true;


int
emacs_module_init (struct emacs_runtime *rt)
{
  assert (rt->size > 0);
  emacs_env env = rt->emacs_env;


  emacs_value nil = env->intern (env, "nil");
  //^ an Elisp symbol from a C string.


  emacs_value eN = env->make_integer (env, n);
  //^ an Elisp integer from a C int.


  intmax_t n = env->extract_integer (env, eN);
  //^ a C int from an Elisp integer (or value).


  emacs_value eX = env->make_float (env, x);
  //^ an Elisp number from a C double.


  intmax_t x = env->extract_float (env, eX);
  //^ a C double from an Elisp number (or value).


  emacs_value eS = env->make_string (env, /*const char star*/ s, /*ptrdiff_t*/ n);
  //^ an Elisp multibyte string from a C string (UTF-8, NULL-terminated) & length.


  char *s;
  ptrdiff_t *l;
  bool _ = env->copy_string_contents(env, eS, s, l);
  //^ a C string (NULL-terminated) & length from an Elisp string. 


  emacs_value _ = env->_ (env, _);
  //^ 


  emacs_value _ = env->_ (env, _);
  //^ 


  emacs_value _ = env->_ (env, _);
  //^ 


  emacs_value _ = env->_ (env, _);
  //^ 


}


/*


https://developer.apple.com/documentation/appkit/nsspeechrecognizer


https://www.gnu.org/software/emacs/manual/html_node/emacs/View-Mode.html


*/


/*.m*/


@implementation EmacsRecognized


- (id) init {
    self.env = rt->env;
    emacs_value nssr_recognized_hook = self.env->intern("nssr-recognized-hook");
    self.hook = self.env->symbol_value(nssr_recognized_hook);
  }


  didRecognizeCommand(ns_command) {
    emacs_value e_command = self.env->extract_string(ns_command);
    self.env->run_hook(self.hook, e_command)
  }
}


/*.h*/


@interface EmacsRecognized : NSObject <NSSpeechRecognizerDelegate>


@property (retain, atomic) emacs_value;


- (id) init;


- (void) didRecognizeCommand:(NSString *);


@end


/*
Emacs.app activates the run loop in a kind of stutter-step fashion using [NSApp run] followed by [NSApp stop] repeatedly.  Certain asynchronous events work, like open-file requests to the NSApplication delegate, and NSTextInput callbacks.


You cannot safely run Elisp code asynchronously.  So the way to trigger Elisp code from an async callback is to queue an event into the event_queue and then add code to run the corresponding Elisp code when that event is unqueued
*/




________________




(defun mtg0-card-put-name (symbol &optional card-name)
  (put symbol 'mtg0-name (or card-name (mtg0-capitalize (symbol-name symbol)))))
;;i.e. (put '… 'mtg0-name "…")
;;e.g. (put 'fire-+-ice 'mtg0-name "Fire // Ice")
;;e.g. (put ' 'mtg0-name (mtg0-capitalize ""))


(defun mtg0-make-obarray (n)
  "Mercer-prime-sized obarray.
e.g. “(eql 8191 (length (make-obarray 13))) ”.
n.b. N (→ ‘length’): 2 (3), 3 (7), 5 (31), 7 (127), 13 (8,191), 17 (131,071), 19 (524,287), 31 (2,147,483,647), …
See ‘https://en.m.wikipedia.org/wiki/Mersenne_prime’."
  (make-vector (- (** 2 n) 1) 0))`


________________




;;TODO keywords abilities either (a) don't end with ".", or (b) do have a "—" between the keyword phrase and keyword cost without spaces. e.g. "Suspend 3—Discard a red card."


;;TODO ‘:colorish’ has all the colors that the card explicitly or implicitly costs (e.g. “{_}”; e.g. “Discard a black card”; e.g. “Sacrifice a Mountain”; e.g. “Tap an untapped white creature you control”; e.g. “Exile a green card from your graveyard”; e.g. “Return an Island you control to its owner's hand”; e.g. “”; e.g. “”; &c) and totally costs (e.g. mana cost; e.g. activated ability; e.g. triggered ability with optional payment; e.g. activated/triggered costed keyword abilities (Flashback, Extort, &c); ).


;;TODO the 


;;
(defcustom mtg0-data-dir nil
  ""
  :type '((or nil file) :tag "Directory")
  :safe t
  :group #'mtg0)


(defcustom mtg0-cache-dir nil
  ""
  :type '((or nil file) :tag "Directory")
  :safe t
  :group #'mtg0)


(defcustom mtg0-config-dir nil
  ""
  :type '((or nil file) :tag "Directory")
  :safe t
  :group #'mtg0)


(defun mtg0-url-cache-dir ()
 (concat (file-name-as-directory (or mtg0-url-cache-dir (mtg0-xdg-get-cache-path "url")))))


(flet* ((xdg-get-data-path ( paths)
  ())
(xdg-get-cache-path (&optional path) ())
(xdg-get-config-path (&optional path) ())
)
(defun mtg0-xdg-get-data-path (&rest paths)
  ""
  (expand-file-name (concat (file-name-as-directory (or (mtg0-data-dir (xdg-get-data-path `("emacs" "mtg" ,@paths))) ))
  )
)


(defun mtg0-url-get-cache-dir ()
  ""
  (concat (file-name-as-directory (or mtg0-cache-directory (mtg0-xdg-get-cache-path))) "url"))


(cl-defun mtg0-url-fetch (url &key timeout cookies silent)
  (let ((url-cache-dir (mtg0-url-cache-dir))
        (url-automatic-caching t) 
       (url-cache-creation-function #'url-cache-create-filename-human-readable))
    (url-retrieve-synchronously url silent cookies timeout)))


(cl-defun mtg0-scryfall-image-view (name &key size)
  (interactive (list (mtg0--read-cardprint "<CARD>[-<SET>[-<CCN>]] (e.g. “time-walk-lea-”): ") (mtg0-scryfall--read-image-size "Image size (default “large”")))
  (let* ((ID ( (mtg0-cardprint-from name))
         (REQUEST-BUFFER (mtg0-scryfall-image-fetch ID size))
         (REQUEST-JSON (with-current-buffer REQUEST-BUFFER
           (goto-char (point-min))
           (mtg0-json--read-buffer))))
;TODO with-image-buffer
    (when (and REQUEST-JSON)
      (let-alist REQUEST-JSON)
        ()))))


;;e.g. M-: (mtg0-scryfall--image-view "" :size ')
;;e.g. M-x mtg0-scryfall--image-view "" :size ')


(cl-defun mtg0-scryfall--image-fetch (id &key image-type)
  (let* ((DATA (mtg0-scryfall--cardprint-data-fetch id)) 
         (URL (mtg0-scryfall-get-cardprint-data-image-url DATA image-type)))
      ( URL)))


;;e.g. (mtg0-scryfall-image-fetch "" :size ')
;;
;;n.b. (print_)id ≠ oracle_id


(cl-defun mtg0-scryfall--cardprint-data-fetch (id)
  (let* ((URL (format "%s/cards/%s?format=json" mtg0-scryfall-api-host-url id))
         (REQUEST-BUFFER (mtg0-url--fetch URL mtg0-scryfall-default-silent-p nil mtg0-scryfall-default-timeout-in-seconds id))
         (REQUEST-JSON (with-current-buffer REQUEST-BUFFER
           (goto-char (point-min))
           (mtg0-json--read-buffer))))
;TODO with-image-buffer
    (when (and REQUEST-JSON)
      …)))


(defconst mtg0-scryfall-known-image-type-list
  '(png large-jpg normal-jpg small-jpg art-crop-jpg border-crop-jpg)
  "")


(defun mtg0-scryfall-get-cardprint-data-image-url (alist image-type)
  (cl-typecheck 'list alist "")
  (cl-typecheck (or 'png 'large-jpg 'normal-jpg 'small-jpg 'art-crop-jpg 'border-crop-jpg) image-type "")
  (let-alist alist
    (pcase image-type
      ('png .image_uris.png)
      ('large-jpg .image_uris.large)
      ('normal-jpg .image_uris.normal)
      ('small-jpg .image_uris.small)
      ('art-crop-jgp .image_uris.art_crop)
      ('border-crop-jpg .image_uris.border_crop)
     )))


;;e.g. ( 'png)
;;
;;n.b. 


{ …, "image_uris": { "small": "https://c1.scryfall.com/file/scryfall-cards/small/front/7/b/7bf864db-4754-433d-9d77-6695f78f6c09.jpg?1562832669", "normal": "https://c1.scryfall.com/file/scryfall-cards/normal/front/7/b/7bf864db-4754-433d-9d77-6695f78f6c09.jpg?1562832669", "large": "https://c1.scryfall.com/file/scryfall-cards/large/front/7/b/7bf864db-4754-433d-9d77-6695f78f6c09.jpg?1562832669", "png": "https://c1.scryfall.com/file/scryfall-cards/png/front/7/b/7bf864db-4754-433d-9d77-6695f78f6c09.png?1562832669", "art_crop": "https://c1.scryfall.com/file/scryfall-cards/art_crop/front/7/b/7bf864db-4754-433d-9d77-6695f78f6c09.jpg?1562832669", "border_crop": "https://c1.scryfall.com/file/scryfall-cards/border_crop/front/7/b/7bf864db-4754-433d-9d77-6695f78f6c09.jpg?1562832669" } }, … }


(defmacro mtg0-scryfall--with-throttle (&rest body)
  "Rate-limiting against Scryfall API."
  `(condition-case nil
       (progn
         ( 1+ mtg0-scryfall--concurrent-requests)
         (when (< 50 (abs (- current-request-time mtg0-scryfall--latest-request-time)))  ; milliseconds
           ,@body))
     finally (prog ( 1- ) (setq mtg0-scryfall--latest-request-time current-request-time)))


;;


(defun mtg0-cardprint-from (cardprint)
  (cond ((mtg0-cardprint-p cardprint) cardprint)
        (symbolp cardprint) (intern-soft ))
        ((stringp cardprint) ( ))
        (t )))


(defun mtg0-read--cardprint (&optional prompt)
  )
;;TODO "<CARD>[-<SET>[-<CCN>]] (e.g. “time-walk-lea-”): " OR "<CARD>[ (<SET> [<CCN>])] (e.g. “Time Walk (LEA )”): "


(defun mtg0-scryfall--read-image-size (&optional prompt)
  )
;;TODO show both word and dimensions/format/.


(defconst mtg0-scryfall-api-host-url "https://api.scryfall.com"
 "")
(defconst mtg0-scryfall- ""
 "")
(defconst mtg0-scryfall- ""
 "")
(defconst mtg0-scryfall- ""
 "")
(defconst mtg0-scryfall-default-timeout-in-seconds 5
  "See URL ‘https://scryfall.com/docs/api’.
>Rate Limits and Good Citizenship: We kindly ask that you insert 50 – 100 milliseconds of delay between the requests you send to the server at api.scryfall.com. (i.e., 10 requests per second on average). Submitting excessive requests to the server may result in a HTTP 429 Too Many Requests status code. Continuing to overload the API after this point may result in a temporary or permanent ban of your IP address. The file origins used by the API, such as c1.scryfall.com, c2.scryfall.com, and c3.scryfall.com do not have these rate limits.")
(defconst mtg0-scryfall-default-silent-p nil
  "")
(defconst mtg0-scryfall-default-max-concurrent-requests 5)
 "")


;; https://api.scryfall.com/bulk-data
;; https://c2.scryfall.com/file/scryfall-bulk/all-cards/all-cards-20211028091124.json → (All Cards)(198 MB)(2021-10-28 09:11 UTC)(A JSON file containing every card object on Scryfall in every language.) → 


;; https://scryfall.com/docs/api/bulk-data
;; Oracle Cards        Download        12.2 MB        2021-10-28 09:03 UTC: A JSON file containing one Scryfall card object for each Oracle ID on Scryfall. The chosen sets for the cards are an attempt to return the most up-to-date recognizable version of the card.
;; Unique Artwork        Download        15.1 MB        2021-10-28 09:12 UTC: A JSON file of Scryfall card objects that together contain all unique artworks. The chosen cards promote the best image scans.
;; Default Cards        Download        31.2 MB        2021-10-28 09:02 UTC: A JSON file containing every card object on Scryfall in English or the printed language if the card is only available in one language.
;; All Cards        Download        198 MB        2021-10-28 09:11 UTC: A JSON file containing every card object on Scryfall in every language.
;; Rulings        Download        2.92 MB        2021-10-28 09:03 UTC: A JSON file containing all Rulings on Scryfall. Each ruling refers to cards via an `oracle_id`.
;;


;; https://c2.scryfall.com/file/scryfall-bulk/oracle-cards/oracle-cards-20211028090346.json ← {"object":"bulk_data","id":"27bf3214-1271-490b-bdfe-c0be6c23d02e","type":"oracle_cards","updated_at":"2021-10-28T09:03:46.439+00:00","uri":"https://api.scryfall.com/bulk-data/27bf3214-1271-490b-bdfe-c0be6c23d02e","name":"Oracle Cards","description":"A JSON file containing one Scryfall card object for each Oracle ID on Scryfall. The chosen sets for the cards are an attempt to return the most up-to-date recognizable version of the card.","compressed_size":12829096,"download_uri":"https://c2.scryfall.com/file/scryfall-bulk/oracle-cards/oracle-cards-20211028090346.json","content_type":"application/json","content_encoding":"gzip"} 


;;


:side
(pcase `(,.layout . ,.side) (
      (`(split . a) 'left)
      (`(split . b) 'right)
      (`(flip . a) 'rightside-up)
      (`(flip . b) 'upside-down)
:side
(cond (
      ((and (eq 'split .layout) (eq 'a .side)) 'rightside-up)
      ((and (eq 'split .layout) (eq 'b .side)) 'upside-down)
      )


________________




MTG-WIDGET


;; ‘mtg-widget’


;; for sorting results by orders: (1) draggable widget list; (2) rectangular sub-buffer (indirect-buffer?), c.f. magit-squash.


;; often (not always):
;; • symbol ‘mouse-1’ — Left button.
;; • symbol ‘mouse-2’ — Middle button and/or Wheel button click.
;; • symbol ‘mouse-3’ — Right button.
;; • symbol ‘mouse-4’ — Wheel up.
;; • symbol ‘mouse-5’ — Wheel down.


;;


(require 'widget)
(require 'wid-edit)


(defun mtg-query-widgets ()
  (interactive)
  )


(defun mtg-types-widget-insert ()
  ""
  (switch-to-buffer "*M:tG Card Queries*")
  (kill-all-local-variables)
  (make-local-variable 'mtg-types-widget)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ())
  (mtg-types-dropdown-widget-create)
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))


(defun mtg-types-dropdown-widget-create ()
  "Create a drop-down widget to choose a subtype."
  (apply #'widget-create 'menu-choice
                 :value ""
                 :help-echo ""
                 :notify #'mtg-types-widget-notify
                 (mtg-types-dropdown-widget-choices)))


(defun mtg-types-dropdown-widget-choices (&optional types)
  ""
  (cl-loop for TYPE in (or types mtg-all-types)
    with TYPE-NAME = (mtg-type-name TYPE)
    collect `(choice-item ,TYPE-NAME)))


(defun mtg-types-dropdown-widget-notify (widget &rest _)
  ""
  (let* ((TYPE (widget-value widget)))
    (mtg-query-add-type TYPE)))


(defun mtg-types-checklist-widget-create ()
  ""
  (apply #'widget-create 'checklist
                 :value ""
                 :help-echo ""
                 :notify #'mtg-types-checklist-widget-notify
                 (mtg-types-checklist-widget-choices)))


(defun mtg-types-checklist-widget-choices (&optional types)
  ""
  (cl-loop for TYPE in (or types mtg-all-types)
    with TYPE-NAME = (mtg-type-name TYPE)
    collect `(choice-item ,TYPE-NAME)))


(defun mtg-types-checklist-widget-notify (widget &rest _)
  ""
  (let* ((TYPES (widget-value widget)))
    (mtg-query-add-types TYPES)))


(defun 'mtg-types-widget-create #'mtg-types-checklist-widget-create)


;;


________________




(defmacro sboo-xdg--dir-home (environment-variable default-unix-path default-osx-path default-win32-path application-name application-author)
  (declare (debug (stringp stringp)))
  (let ((env-path (make-symbol "env-path"))
        (def-path (make-symbol "def-path"))
        (xdg-path (make-symbol "xdg-path")))
    `(let* ((,env-path (getenv ,environment-variable))
            (,def-path (cond ((memq system-type '(ms-dos windows-nt)) default-win32-path) ((eq system-type 'darwin) default-osx-path) (t default-posix-path))
            (,xdg-path (if (and (not (null ,env-path)) (file-name-absolute-p ,env-path))
           (concat ,env-path "/%n")
        ,def-path)))
        (expand-file-name (format-spec ,xdg-path '((?n . ,application-name) (?a . ,application-author))))))


;; XDG Data:
Linux        $XDG_DATA_HOME or ~/.local/share/<AppName>
OSX        ~/Library/Application Support/<AppName>
Windows        C:\Users\<username>\AppData\Local\<AppAuthor>\<AppName>
;; XDG Configuration:
;; • Linux        $XDG_CONFIG_HOME or ~/.config/<AppName>
;; • OSX        ~/Library/Application Support/<AppName>
;; • Windows        C:\Users\<username>\AppData\Local\<AppAuthor>\<AppName>
;; XDG Cache:
Linux        $XDG_CACHE_HOME or ~/.cache/<AppName>
OSX        ~/Library/Caches/<AppName>
Windows        C:\Users\<username>\AppData\Local\<AppAuthor>\<AppName>\Cache
;;


(defun sboo-xdg-config-home (application-name &optional (application-author ""))
  "Return the base directory for user specific configuration files.
According to the XDG Base Directory Specification version
0.8 (8th May 2021):
    \"$XDG_CONFIG_HOME defines the base directory relative to
    which user-specific configuration files should be stored.
    If $XDG_CONFIG_HOME is either not set or empty, a default
    equal to $HOME/.config should be used.\""
  (sboo-xdg--dir-home
    "XDG_CONFIG_HOME"
    "~/.config/%n"
    "~/Library/Application Support/%n"
    "C:\Users\%HOME%\AppData\Local\%a%n"
    application-name
    application-author))


XDG_CONFIG_HOME or ~/.config/<AppName>
OSX    ~/Library/Application Support/<AppName>
Windows    C:\Users\<username>\AppData\Local\<AppAuthor>\<AppName>


(defun sboo-xdg-cache-home ()
  "Return the base directory for user specific cache files.
According to the XDG Base Directory Specification version
0.8 (8th May 2021):
    \"$XDG_CACHE_HOME defines the base directory relative to
    which user-specific non-essential data files should be
    stored.  If $XDG_CACHE_HOME is either not set or empty, a
    default equal to $HOME/.cache should be used.\""
  (sboo-xdg--dir-home "XDG_CACHE_HOME" "~/.cache" ""))


(defun sboo-xdg-data-home ()
  "Return the base directory for user specific data files.
According to the XDG Base Directory Specification version
0.8 (8th May 2021):
    \"$XDG_DATA_HOME defines the base directory relative to which
    user-specific data files should be stored.  If $XDG_DATA_HOME is
    either not set or empty, a default equal to $HOME/.local/share
    should be used.\""
  (sboo-xdg--dir-home "XDG_DATA_HOME" "~/.local/share" ""))


;;


________________




MTG-


;;; ‘mtg-’:


;;






;;


________________




MTG-CARD


;;; ‘mtg-card’:


;;


(cl-defun mtg-card-guess-predicate (predicate)
  ""
  (pcase predicate
    ((and (pred symbolp) (memq mtg-types)) (lambda (card) (mtg-card-type-p predicate card)))
    ((and (pred symbolp) (memq mtg-colors)) (lambda (card) (mtg-card-color-p predicate card)))
;;    ((and (pred symbolp) (memq mtg-s)) (lambda (card) (mtg-card--p predicate card)))
    ((pred functionp) predicate)))


;;


(defun mtg-card-type-p (type card)
  (let ((CARD-ALL-TYPES (mtg-card-types card)))
    (pcase type
      ('spell
        (or (memq 'instant CARD-ALL-TYPES) (memq 'sorcery CARD-ALL-TYPES)))
      (t
        (memq type CARD-ALL-TYPES))))


(defun mtg-card-land-p (card)
  (mtg-card-type-p 'land card))


(defun mtg-card-spell-p (card)
  (mtg-card-type-p 'spell card))


;;


(defun mtg-card-color-p (color card)
  (let ((CARD-COLORS (mtg-card-colors card)))
    (pcase color
      ('colorless
        (eq '() CARD-COLORS))
      (t
        (memq color CARD-COLORS))))


(defun mtg-card-green-p (card)
  (mtg-card-color-p 'green card))


(defun mtg-card-colorless-p (card)
  (mtg-card-color-p 'colorless card))


;;






;;


________________




MTG-DECK


;;; ‘mtg-deck’:


(cl-defstruct (mtg-deck (:constructor mtg-make-deck) (:constructor nil) (:copier nil))
  (name nil :type string) (cards [] :type vector) (size 0 :type integer))


;;


;;


________________




MTG-FORMAT


;;; ‘mtg-format’:


(cl-defstruct (mtg-format (:constructor mtg-make-format) (:constructor nil) (:copier nil))
  (id nil :type symbol) (name nil :type string) (date :type string) (min-deck-size 60 :type integer) (max-card-count 4 :type integer) (legal-editions :type vector) (banned-cards [] :type vector) (restricted-cards [] :type vector) (edition-predicate :type vector) (card-predicate :type vector))


(defmacro mtg-define-format (id &rest kwargs)
  `(prog1 (quote ,id)
          (defconst mtg-format/,id (mtg-make-format :id (quote ,id) ,@kwargs))
          (put (quote ,id) 'mtg-format mtg-format/,id)
          (add-to-list 'mtg-known-formats (quote ,id))))


(cl-defun mtg-get-format (id)
  (get id 'mtg-format))


(cl-defun mtg-get-format (id format)
  (put id 'mtg-format format))


(defvar mtg-known-formats
  '()
  "")


;;


(mtg-define-format vintage
 :name ("Vintage" "Type 1") :min-deck-size 60 :max-card-count 4
  :legal-editions []
  :banned-cards []
  :restricted-cards []
  :edition-predicate #'mtg-edition-vintage-valid-p
  :card-predicate #'mtg-card-vintage-invalid-p
  )


(mtg-define-format commander
 :name '("Commander" "EDH") :min-deck-size 100 :max-card-count 1
  :edition-predicate #'mtg-edition-commander-p
  )


(mtg-define-format standard
 :name "Standard" :min-deck-size 60 :max-card-count 4
  :edition-predicate #'mtg-standard-edition-p
 )


(defconst mtg-average-limited-deck '(:cards 40 :lands 17)
  "")
(defconst mtg-average-constructed-deck '(:cards 60 :lands 24)
  "")
(defconst mtg-average-commander-deck '(:cards 100 :lands 40)
  "")


;;


(defun mtg-edition-vintage-p (edition &optional date)
  (mtg-edition-black-bordered-p edition))


(defun mtg-edition-standard-p (edition &optional date)
  (let ((DATE (or date (current-date))))
    (and (<= (- DATE (edition-date edition)) (make-date "2y"))
         (mtg-edition-black-bordered-p edition)
         )))


;;(defun mtg-edition--p (edition &optional date)
;;  (mtg-edition--p edition))


;;


(cl-defun mtg-validate-deck (format deck &optional date)
  (not (not (mtg-invalid-deck format deck date))))


(cl-defun mtg-invalid-deck (format deck &optional date)
  ())


(cl-defun mtg-invalid-vintage-deck (deck &optional date)
  (cl-loop for CARD across (mtg-deck-cards deck)
    collect (mtg-invalid-vintage-card CARD date) into INVALID-CARDS
    finally return INVALID-CARDS))


(cl-defun mtg-invalid-vintage-card (card count &optional date)
  (and (<= count (mtg-card-vintage-legality card))
       ()
       ))


(cl-defun mtg-card-vintage-legality (card)
  (or (mtg-card-vintage-legality--via-card card)
      (mtg-card-vintage-legality--via-format card)))


(cl-defun mtg-card-vintage-legality--via-card (card)
  (mtg-legality-count (cdr (assq 'vintage (mtg-card-get 'legalities)))))


(cl-defun mtg-card-vintage-legality--via-format (card)
  (cond ((memq card (mtg-format-banned-cards mtg-format/vintage)) 0)
        ((memq card (mtg-format-restricted-cards mtg-format/vintage)) 1)
        (t 4)))


;;


(cl-defun mtg-legality-count (legality)
  (pcase legality
         ('legal      4)
         ('restricted 1)
         ('banned     0)
         ('illegal    0)))


;;


________________




mana
;; • …: add {_} …
;; • … spells cost {_} less to cast …
;; • …






________________




MTG-MATH


;;; ‘mtg-math’


(require 'calc)


;; sampling from probability distributions (hypergeometric, binomial, etc) and calculating their expected values.
;; 


;; The k B (calc-utpb) [utpb] function uses the binomial distribution. Push the parameters n, p, and then x onto the stack; the result (‘utpb(x,n,p)’) is the probability that an event will occur x or more times out of n trials, if its probability of occurring in any given trial is p. The I k B [ltpb] function is the probability that the event will occur fewer than x times.


(cl-defun.mtg-deck-hypergeometric-exactly (n-deck-cards n-deck-hits n-looks n-hits)
  "The hypergeometric probability mass function (pmf).
Inputs:
• all are ‘natnump’s.
• N-DECK-CARDS — a.k.a. the “population size”.
• N-DECK-HITS — a.k.a. the “number of successes in the population”.
• N-LOOKS — a.k.a. the “sample sixe”.
• N-HITS — a.k.a. the “numbet of successes”.
Output:
• is a ‘numberp’.
• 0 ≤ (mtg-deck-hypergeometric-at-least _ _ _ _) ≤ 1
Notes:
• ⟨ P(X=x) = (K choose k) (N−K choose n−k) ÷ (N choose n) ⟩
See URL ‘https://en.wikipedia.org/wiki/Hypergeometric_distribution’
    >In probability and statistics, a probability mass function is a function that gives the probability that a discrete random variable is exactly equal to some value. Sometimes it is also known as the discrete density function."
  (flet ((C #'mtg-deck-binomial-coefficient))


    (/ (* (! (C n-deck-hits n-hits)) (! (C (- n-deck-cards n-deck-hits) (- n-looks n-hits))))
       (! (C n-deck-cards n-looks)))))


(defalias 'mtg-deck-hypergeometric-pmf #'mtg-deck-hypergeometric-exactly)


(cl-defun mtg-deck-hypergeometric-between (n-deck-cards n-deck-hits n-looks min-n-hits max-n-hits)
  "The probability of drawing at least MIN-N-HITS and at most MAX-N-HITS of some kind of card by drawing n-looks from the top of a deck with N-DECK-CARDS total cards and N-DECK-HITS of that kind of card."
  (cl-loop for n-hits from (or min-n-hits 0) to (or max-n-hits n-looks)
    sum (mtg-deck-hypergeometric-exactly
n-deck-cards n-deck-hits n-looks n-hits)))


(defalias 'mtg-deck-hypergeometric-cdf #'mtg-deck-hypergeometric-between)


(cl-defun mtg-deck-hypergeometric-at-least (n-deck-cards n-deck-hits n-looks n-hits-or-more)
  "The hypergeometric inclusive-complementary cumulative distribution function (cdf).
i.e. The probability of hitting **at least** N-HITS cards of some kind by drawing n-looks cards, where N-DECK-HITS is the number of cards of that kind in the deck and N-DECK-CARDS is the total number of cards in the deck.
Inputs:
• all are ‘natnump’s.
• N-DECK-CARDS ≥ n-looks ≥ 1
• N-DECK-HITS ≥ N-HITS-OR-MORE ≥ 0
Output:
• is a ‘numberp’.
• 0 ≤ (mtg-deck-hypergeometric-at-least _ _ _ _) ≤ 1
Examples:
• e.g. Collected Company (“Look at the top six cards of your library. Put up to two creature cards with mana value 3 or less from among them onto the battlefield. Put the rest on the bottom of your library in any order.”) in Constructed (such decks have 60 cards with about 30 creature cards):
    M-: (mtg-deck-hypergeometric-at-least 60 30 6 2)
    0.
• e.g.  (“”) in Limited:
    M-: (mtg-deck-hypergeometric-at-least 40 17 4 1)
    0.
• e.g. drawing a four-of in your opening hand:
    M-: (mtg-deck-hypergeometric-at-least 60 4 7 1)
    0.399  ; (1 – 0.601 = 0.399)
Notes:
• ⟨ P(X≥k) = P(X=k) + P(X=k+1) + … + P(X=n) = ⟩ (e.g. for Collected Company, calculate Hypergeometric[60;25;6](≥2), i.e. ⟨ H[6](X≥2) = H[6](X=2) + H[6](X=3) + H[6](X=4) + H[6](X=5) + H[6](X=6) ⟩)
• ⟨ P(X≥x) = P(X=x) + P(X>x) = P(X=x) + (1 − P(X≯x)) = P(X=x) + (1 − P(X≤x)) = ⟩ (i.e. “PMF + (1 - CDF)”)
• See URL ‘https://en.wikipedia.org/wiki/Hypergeometric_distribution’:
    >In probability theory and statistics, the hypergeometric distribution is a discrete probability distribution that describes the probability of ‹k› successes (random draws for which the object drawn has a specified feature) in ‹n› draws, without replacement, from a finite population of size ‹N› that contains exactly ‹K› objects with that feature, wherein each draw is either a success or a failure. In contrast, the binomial distribution describes the probability of ‹k› successes in ‹n› draws with replacement."
  (mtg-deck-hypergeometric-between n-deck-cards n-deck-hits n-looks n-hits-or-more nil))


;;  (+ (- 1 (mtg-deck-hypergeometric-at-most n-deck-cards n-deck-hits n-looks n-hits))
;;     (mtg-deck-hypergeometric-exactly n-deck-cards n-deck-hits n-looks n-hits))


(cl-defun mtg-deck-hypergeometric-at-most (n-deck-cards n-deck-hits n-looks n-hits-or-less)
  "The hypergeometric cumulative probability distribution (cdf).
i.e. The probability of hitting **at most** N-HITS cards of some kind by drawing n-looks cards, where N-DECK-HITS is the number of cards of that kind in the deck and N-DECK-CARDS is the total number of cards in the deck.
Notes:
• ⟨ P(X≤x) = ⟩
• See URL ‘https://en.wikipedia.org/wiki/Hypergeometric_distribution’:
    >In probability theory and statistics, the cumulative distribution function (CDF) of a real-valued random variable X, or just distribution function of X, evaluated at x, is the probability that X will take a value less than or equal to x."
  (mtg-deck-hypergeometric-between n-deck-cards n-deck-hits n-looks nil n-hits-or-less))


(cl-defun mtg-deck-hypergeometric-ev (n-deck-cards n-deck-hits n-looks)
  "The hypergeometric distribution's expected value (ev), a.k.a. mean-average."
  (/ (* n-looks n-deck-hits)
     n-deck-cards))


(defalias 'mtg-deck-hypergeometric-mean #'mtg-deck-hypergeometric-ev)


(cl-defun mtg-deck-hypergeometric-median (n-deck-cards n-deck-hits n-looks)
  "The hypergeometric distribution's median-average."
  ())


(cl-defun mtg-deck-hypergeometric-mode (n-deck-cards n-deck-hits n-looks)
  "The hypergeometric distribution's mode."
  ())


(cl-defun mtg-deck-hypergeometric-random-sample (n-deck-cards n-deck-hits n-looks)
  "Return a random sample from the hypergeometric distribution
Input:
• N-DECK-CARDS (<N>)
• N-DECK-HITS (<K>)
• n-looks (<n>)
Output:
• N-HITS (<k>) — 
Notes:
• ⟨ X ~ Hypergeometric(N,K,n) ⟩"
  (let* ((N-HITS (TODO n-deck-cards n-deck-hits n-looks)))
    N-HITS))


mtg-deck-multivariate-hypergeometric-pmf
URL‘https://strategy.channelfireball.com/all-strategy/mtg/channelmagic-articles/an-introduction-to-the-multivariate-hypergeometric-distribution-for-magic-players/’:
>For instance, if you want to know the probability of getting an opening hand with two Soul Spike and four Chancellor of the Dross, you need to divide your deck in three categories: Soul Spikes, Chancellors, and other cards. For those cases, you need the multivariate hypergeometric distribution.
mtg-deck-multivariate-hypergeometric-cdf


;;


(cl-defun mtg-deck-binomial-distribution-expected-pmf (&key n-all n-some k-some)
  "
⟨ E[K] = Kₚ × (1 + N) ÷ (1 + Nₚ) ⟩"
  (/ (* k-some (1+ n-all))
     (1+ n-some)))


(cl-defun mtg-deck-binomial-distribution-expected-cdf (&key n-all k-all n-some k-some)
  "
⟨  =  ⟩"
  (calcFunc-utpb k-some n-some (/ k-all n-all)))


(cl-defun mtg-deck-binomial-distribution-random-sample (&key n-all n-some k-all k-some)
  ( n-all n-some k-all k-some))


(defun mtg-deck-E-mill-lands-until-enough-milled-effect (num-cards num-lands num-lands-milled)
  "
e.g. in ‘mtg-format/commander', ‘mtg-card/mind-funeral’ mills almost 10 cards on average."
  (let ((average-cards-milled (mtg-deck-binomial-distribution-expected-value-k-all :n-all num-cards :n-some num-lands :k-all num-lands-milled)))
    average-cards-milled))


;; ^ “Mill-Until” Probabilities:
;; - `E[K] = Kₚ × (1 + N) ÷ (1 + Nₚ)` where `N` is the deck's total card count, `Nₚ` is the deck's land count (i.e. here the predicate `p` means “is some card a land”), `Kₚ` is the terminating condition (i.e. “until you mill M lands”), and `E[…]` is the expected value of some probability distribution while `K` is the probability distribution of milling some number of cards (i.e. `E[K]` is the average milled card count).
;; - e.g. *Mind Funeral*’s expected effect in commander is milling about `10` cards. For example, the average number of cards milled by “Target opponent reveals cards from the top of their library until four land cards are revealed. That player puts all cards revealed this way into his or her graveyard.” in a 40-land 100-card deck is exactly `400/41` and approximately `9.76`, calculated by `( × ) ÷ ()`.)
;; <https://boardgames.stackexchange.com/questions/34605/calculate-mind-funeral-efficiency>


;;


(cl-defstruct (mtg-deck-hypergeometric-stats (:type list) (:constructor mtg-deck-make-hypergeometric-stats) (:constructor nil) (:copier nil))
  (cards nil :type integer) (hits nil :type integer) (predicate nil :type function))


(cl-defstruct (mtg-card-hypergeometric-stats (:type list) (:constructor mtg-card-make-hypergeometric-stats) (:constructor nil) (:copier nil))
  (looks nil :type integer) (hits nil :type integer))


(cl-defun mtg-deck-make-hypergeometric-stats-from (deck &optional (predicate #'mtg-card-land-p))
  ""
  (let* ((PREDICATE (mtg-card-guess-predicate  predicate)))
    (mtg-deck-make-hypergeometric-stats :cards (mtg-deck-size deck) :hits (cl-loop for CARD in (mtg-deck-cards deck) count (funcall predicate CARD)) :predicate predicate))))


(cl-defun mtg-card-make-hypergeometric-stats-from (card)
  )


(cl-defun mtg-deck-hypergeometric-at-least-from-stats (deck-stats card-stats &optional predicate)
  ""
  (mtg-deck-hypergeometric-at-least (mtg-deck-hypergeometric-stats-cards deck-stats) (mtg-deck-hypergeometric-stats-hits deck-stats) (mtg-card-hypergeometric-stats-looks card-stats) (mtg-card-hypergeometric-stats-hits card-stats)))


  (let* ((STATS (mtg-deck-make-hypergeometric-stats deck predicate)))


(defun mtg-guess-hypergeometric-cdf (card deck)
  )


;;


(cl-defun mtg-deck-negative-hypergeometric-pmf (n-deck-cards n-deck-misses n-misses n-hits)
  "The negative hypergeometric probability distribution is the likelihood of ‹k› successes in a sample with exactly ‹r› failures.
i.e. The probability of hitting  N-HITS cards of some kind until drawing N-MISSES cards of some other kind, where N-DECK-HITS is the number of cards of that kind in the deck, N-DECK-MISSES is the number of cards of that other kind in the deck, and N-DECK-CARDS is the total number of cards in the deck.
Examples:
• e.g. How likely is Mind Funeral to mill exactly 10 nonland cards and 14 total cards (“Target opponent reveals cards from the top of their library until four land cards are revealed. That player puts all cards revealed this way into their graveyard.”) in Constructed (decks with 60 cards with about 25 land cards):
    M-: (‘mtg-deck-negative-hypergeometric-pmf’ 60 35 4 10)
    0.
Notes:
• ⟨ P(X=x) =  ⟩
• See URL ‘https://en.wikipedia.org/wiki/Negative_hypergeometric_distribution’:
    >In probability theory and statistics, the negative hypergeometric distribution describes probabilities for when sampling from a finite population without replacement in which each sample can be classified into two mutually exclusive categories like Pass/Fail, Male/Female or Employed/Unemployed. As random selections are made from the population, each subsequent draw decreases the population causing the probability of success to change with each draw. Unlike the standard hypergeometric distribution, which describes the number of successes in a fixed sample size, in the negative hypergeometric distribution, samples are drawn until ‹r› failures have been found, and the distribution describes the probability of finding ‹k› successes in such a sample. In other words, the negative hypergeometric distribution describes the likelihood of ‹k› successes in a sample with exactly ‹r› failures."
  (flet ((C #'mtg-deck-binomial-coefficient))


    (/ (* (C (+ n-hits n-misses -1) n-hits)
          (C (- n-deck-cards n-misses n-hits) (- n-deck-hits n-hits)))
       (C n-deck-cards n-deck-hits))))


(cl-defun mtg-deck-negative-hypergeometric-ev (n-deck-cards n-deck-hits n-misses)
  "
Examples:
• e.g. The average number of cards Mind Funeral mills (“Target opponent reveals cards from the top of their library until four land cards are revealed. That player puts all cards revealed this way into their graveyard.”) in Constructed (decks with 60 cards with about 25 land cards):
    M-: (‘mtg-deck-negative-hypergeometric-ev’ 60 35 4)
    .
Notes:
• ⟨ E(P[N,K,r](X)) = (r × K) ÷ (N-K+1) ⟩
• See URL ‘https://en.wikipedia.org/wiki/Negative_hypergeometric_distribution’"


  (/ (* n-misses n-deck-hits)
     (+ n-deck-cards (* -1 n-deck-hits) 1)))


(cl-defun mtg-deck-negative-hypergeometric-cdf (n-deck-cards n-deck-hits n-misses min-n-hits max-n-hits)
  "
Examples:
• e.g. How likely is Mind Funeral to mill at least 10 nonland cards or 14 total cards (“Target opponent reveals cards from the top of their library until four land cards are revealed. That player puts all cards revealed this way into their graveyard.”) in Constructed (decks with 60 cards with about 25 land cards):
    M-: (‘mtg-deck-negative-hypergeometric-cdf’ 60 35 4 10 nil)
    0.
"


  (cl-loop for n-hits from (or min-n-hits 0) to (or max-n-hits n-deck-hits)
    sum (mtg-deck-negative-hypergeometric-pmf
n-deck-cards n-deck-hits n-misses n-hits)))


;; (n-deck-cards n-deck-misses n-deck-hits n-misses n-hits)


;;


(cl-defun mtg-deck-multivariate-hypergeometric-cdf ()
  "
URL ‘https://strategy.channelfireball.com/all-strategy/mtg/channelmagic-articles/an-introduction-to-the-multivariate-hypergeometric-distribution-for-magic-players/’
>What the above calculation reveals is that if you play a 60-card Eldrazi Tron deck in Modern without any card selection, then you would draw at least one copy of each Urza land in your top 10 cards in 12.6% of the games. So barring mulligans, you’ll have access to 7 mana on turn 3 on the draw approximately once per 8 games on average."


  ())


;;


(defun mtg-deck--discovery-probability (&optional n-looks n-hits n-deck-hits n-deck-cards)
  "The probability of discovering at least one card.
Examples:
• M-: (mtg-deck--discovery-probability)
      0.
"
  (mtg-deck-hypergeometric-between (or n-deck-cards 60) (or n-deck-hits __) (or n-looks 4) (or n-hits 1) nil))


(defun mtg-deck-probability-of-hitting (&optional card deck)
  "The probability of CARD hitting in DECK.
Examples:
• e.g. Mulch (“Reveal the top four cards of your library. Put all land cards revealed this way into your hand and the rest into your graveyard.”):
    M-: ( mtg-deck-probability-of-hitting 'mulch 'average-standard)
        0.
• e.g. Narset, Parter of Veils (“Look at the top four cards of your library. You may reveal a noncreature, nonland card from among them and put it into your hand. Put the rest on the bottom of your library in a random order.”):
    M-: ( mtg-deck-probability-of-hitting 'narset-parter-of-veils 'vintage-jeskai-2019---2nd-place)
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-hitting ' ')
        0.
"
  (let* ((card-stats (mtg-make-hypergeometric-card-stats card))
         (deck-stats (mtg-make-hypergeometric-deck-stats deck (mtg-hypergeometric-card-stats-predicate card-stats)))
         (n-deck-cards (mtg-hypergeometric-deck-stats-cards deck-stats))
         (n-deck-hits (mtg-hypergeometric-deck-stats-hits deck-stats))
         (n-looks (mtg-hypergeometric-card-stats-looks card-stats))
         (n-hits (mtg-hypergeometric-card-stats-hits card-stats))
         )
    (mtg-deck-hypergeometric-between n-deck-cards n-deck-hits n-looks n-hits)))


(defun mtg-deck-probability-of-reaching (card deck &optional n)
  "The probability of CARD reaching N cards in/against DECK.
Examples:
• e.g. Mind Funeral (“Target opponent reveals cards from the top of their library until four land cards are revealed. That player puts all cards revealed this way into their graveyard.”):
    M-: ( mtg-deck-probability-of-reaching \\='mind-funeral \\=' 10)
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
• e.g.  (“”):
    M-: ( mtg-deck-probability-of-reaching ' ')
        0.
"
  (let* ((card-stats (mtg-make-negative-hypergeometric-card-stats card))
         (deck-stats (mtg-make-negative-hypergeometric-deck-stats deck (mtg-negative-hypergeometric-card-stats-predicate card-stats)))
         (n-deck-cards (mtg-negative-hypergeometric-deck-stats-cards deck-stats))
         (n-deck-hits (mtg-negative-hypergeometric-deck-stats-hits deck-stats))
         (n-misses (mtg-negative-hypergeometric-card-stats-hits card-stats))
         )
    (mtg-deck-negative-hypergeometric-between n-deck-cards n-deck-hits n-looks (+ n-hits))))


(defun mtg-deck-probability-of-hitting-multiple (hand-size deck-size card-plist deck-plist)
  "The probability of hitting the count/range & kind of card in N-HITS-PLIST among HAND-SIZE (a.k.a. N-LOOKS) cards in DECK.
Examples:
• e.g. Drawing the right number & count of lands into an average two-color Limited deck:
    M-: (mtg-deck-probability-of-hitting-multiple 40 7 '(forest 2 island 1) '(forest 9 island 8))
    0.
    M-: (mtg-deck-probability-of-hitting-multiple 40 7 '(forest 2 island 1 nil 4) '(forest 9 island 8 nil 23))
    0.
• e.g. “Twin Combo”, drawing both halves of the combo by turn 4 on the play, i.e. both a copier (‹Splinter Twin› or ‹Kiki-Jiki, Mirror Breaker›) and an untapper (‹Pestermite› or ‹Deceiver Exarch›):
    M-: (mtg-deck-probability-of-hitting-multiple 60 (+ 7 3) \\='(copier 1 untapper 1) \\=`(copier ,(+ 4 3) untapper ,(+ 4 3)))
    0.537
    M-: (mtg-deck-probability-of-hitting-multiple 60 (+ 7 3) \\='(copier 1 untapper 1) \\=`(copier ,(+ 4 3) untapper ,(+ 4 3)))
    M-: (mtg-deck-probability-of-hitting-multiple 60 (+ 7 3) \\='(copier 1 untapper 1) \\=`(copier ,(+ 4 4) untapper ,(+ 4 4)))
    0.613
• e.g. a ‹Force of Will› or a ‹Force of Negation›, and another blue card, in your opening hand:
    M-: (+ (mtg-deck-probability-of-hitting-multiple 60 7 \\='(force 1 blue 1) \\='(force 6 blue 17)) (mtg-deck-probability-of-hitting-multiple 60 7 \\='(force 2) \\='(force 6)))
    0.
• e.g. ‹› :
    M-: (mtg-deck-probability-of-hitting-multiple 60 7 \\='( 1  1) \\=`())
    0.
• e.g. ‹› :
    M-: (mtg-deck-probability-of-hitting-multiple 60 7 \\='( 1  1) \\=`())
    0.
• e.g. ‹Dig through Time› for both a land and a nonland:
    M-: (mtg-deck-probability-of-hitting-multiple 60 7 \\='(land 1 nonland 1) \\=`(land 25 nonland ,(- 35 1)))
    0.
Links:
• URL ‘https://strategy.channelfireball.com/all-strategy/mtg/channelmagic-articles/an-introduction-to-the-multivariate-hypergeometric-distribution-for-magic-players/’"
  (flet ((C #'mtg-deck-binomial-coefficient))
card-plist deck-plist
    (let ((DENOMINATOR
           (C deck-size hand-size))
          (N-DECK-CARDS
            (or deck-size
                (cl-loop for (_ COUNT) on deck-plist by #'cddr sum COUNT)))
          (CARD-PLIST-OPEN
            (cl-loop for (CARD COUNT) on card-plist by #'cddr 
              with COUNTS = (pcase COUNT
                       ((pred integerp)
                         (list COUNT))
                       (((let x (pred integerp)) . (let y (pred integerp)))
                         (number-sequence (max x 0) (min y (mtg-deck-count-card CARD N-DECK-HITS-LIST))))
                       ((pred listp)
                         COUNT))
              append (list CARD COUNTS))
          (CARD-PLIST-CLOSED
            (cl-loop for (CARD . OPEN-COUNTS) on CARD-PLIST-OPEN by #'cddr
              with N-REST = (- hand-size
                           (cl-loop for COUNT in COUNTS sum COUNT))
              with CLOSED-COUNTS = (append OPEN-COUNTS (cons nil N-REST))
              collect (cons CARD CLOSED-COUNTS)))
          (CARD-PLIST
            (cl-loop for (CARD . N-HITS-LIST) on CARD-PLIST-CLOSED by #'cddr
              if (cl-loop for N-HITS in N-HITS-LIST
                   all (natnump N-HITS)
              collect (cons CARD N-HITS-LIST)))
          )
      (cl-loop for (CARD . N-HITS-LIST) in CARD-PLIST by #'cddr
        multiply (cl-loop for N-HITS in N-HITS-LIST
          with N-DECK-HITS = (mtg-deck-count-card deck CARD)
          multiply (C N-HITS N-DECK-HITS) into NUMERATOR


        finally return (/ NUMERATOR DENOMINATOR)))))


;; 3 ≈ '(3 . 3) ≈ '(3)
;; '(2 . 4) ≈ '(2 3 4)


;; e.g.:
;; (… '(land (2 . 4) creature (2 . 4)) 7 …) →
;; '(land (2 3 4) creature (2 3 4) nil (- 7 land creature)) →
;; '((land 2 creature 2 nil 3) (land 2 creature 3 nil 2) (land 2 creature 4 nil 1) (land 3 creature 2 nil 2) (land 3 creature 3 nil 1) (land 3 creature 4 nil 0) (land 4 creature 2 nil 1) (land 4 creature 3 nil 0) (land 4 creature 4 nil -1)) → 
;; '((land 2 creature 2 nil 3) (land 2 creature 3 nil 2) (land 2 creature 4 nil 1) (land 3 creature 2 nil 2) (land 3 creature 3 nil 1) (land 3 creature 4) (land 4 creature 2 nil 1) (land 4 creature 3)) → 
;; ; given 'land and 'creature, 'nil means "nonland noncreature".
;; ;  invalid combinatorics (like “nil -1”) are dropped.
;; 


(defun mtg-deck-count-card (deck card)
  "Count how many CARD(s) are in DECK.
• CARD is a ‘symbolp’ or an ‘mtg-card-p’.
• DECK is a p‘listp’ or an ‘mtg-deck-p’."
  (plist-get card deck))


;; Tasha’s Hideous Laughter: “Each opponent exiles cards from the top of their library until that player has exiled cards with total mana value 20 or more.”


;;






;; (mtg-deck-hypergeometric-at-least N-DECK-CARDS N-DECK-HITS n-looks N-HITS)…


(defun mtg-deck--opening-hand-goldilocks-lands-probability (&optional n-deck-lands n-hand-cards n-hand-min-lands n-hand-max-lands)
  "Drawing 2–4 lands in your opening hand in standard.
e.g. (mtg-deck--opening-hand-goldilocks-lands-probability)
     0.7784  ;77.84%"
  (mtg-deck-hypergeometric-between 60 (or n-deck-lands 25) (or n-hand-cards 7) (or n-hand-min-lands 2) (or n-hand-max-lands 4)))


(defun mtg-deck--opening-hand-mox-probability (&optional n-deck-moxen n-hand-cards n-hand-moxen)
  "Drawing at least one mox (and one land?) in your opening hand in vintage.
e.g. (mtg-deck--opening-hand-mox-probability)
     0."
  (mtg-deck-hypergeometric-between 60 (or n-deck-moxen 5) (or n-hand-cards 7) (or n-hand-moxen 1) nil))


(defun mtg-deck--collected-company-probability (&optional n-creatures-in-deck n-cards-in-deck)
  "The probability of Collected Company hitting two creatures (with 30÷60 as default N-CREATURES-IN-DECK÷N-CARDS-IN-DECK).
e.g. (mtg-deck--collected-company-probability)
     0."
  (mtg-deck-hypergeometric-between (or n-cards-in-deck 60) (or n-creatures-in-deck 30) 6 2 nil))


(defun mtg-deck--two-land-drops-by-turn-two-off-one-land-opening-hand-in-limited-probability (&optional n-deck-hits n-deck-cards)
  "The probability of having at least two lands by turn 2 after keeping a one-land hand on the draw in Limited (with a 17-land 40-card deck).
e.g. (mtg-deck---probability)
     0."
  (mtg-deck-hypergeometric-between (or n-deck-cards 60) (or n-deck-hits __) 4 1 nil))
HYPGEOMDIST(0,2,16,33)=25.8%. This means that the complementary probability of hitting your second land drop is 74.2%.


(defun mtg-deck--goblin charbelcher-legacy-victory-probability (&optional n-deck-hits n-deck-cards)
  "The probability of Goblin Charbelcher killing the opponent (with a 50-card library containing five Mountains).
e.g. (mtg-deck---probability)
     0."
  (mtg-deck-hypergeometric-between (or n-deck-cards 60) (or n-deck-hits __) 4 1 nil))
HYPGEOMDIST(0,10,5,50)=31.1%.


(defun mtg-deck--mulling-once-to-leyline-probability (&optional n-deck-hits n-deck-cards)
  "The probability of starting the game with a Leyline of the Void if you play four and are always willing to mulligan to six.
e.g. (mtg-deck---probability)
     0."
  (mtg-deck-hypergeometric-between (or n-deck-cards 60) (or n-deck-hits __) 4 1 nil))
1-HYPGEOMDIST(0,6,4,60)=35.1%.


(defun mtg-deck---probability (&optional n-deck-hits n-deck-cards)
  "The probability of drawing at least .
e.g. (mtg-deck---probability)
     0."
  (mtg-deck-hypergeometric-between (or n-deck-cards 60) (or n-deck-hits __) 4 1 nil))


;;


(defun mtg-deck--binomial-coefficient (n k)
  "
⟨ ((N K)) = N! ÷ (K! × (N-K)!) ⟩"
  (/ (! n) (* (! k) (! (- n k)))))


(defun mtg--format-percentage (x)
  "Format a number X as a percentage.
M-: (mtg--format-percentage 0.12345)
    \"12.34%\""
  (format "%.2f%%" (* 100 x)))


;; Notes:
;; URL ‘https://strategy.channelfireball.com/all-strategy/mtg/channelmagic-articles/an-introduction-to-the-hypergeometric-distribution-for-magic-players/’
;; >twenty-two creatures are the absolute minimum that can be run, providing a 74% chance of hitting two creatures or an EV of 1.68.
;; >Cumulative Probability: P(X ≥ 1). The probability of drawing at least 1 copies. Usually, this is the number that we are interested in. So, the probability to draw at least one of your four Hardened Scales in your opening hand from a 60-card deck is 39.9%. 
;;


;;


________________




MTG-DATA


;;n.b. "_.mtg.el[.gz]"


(cl-defun mtg-load-cards (&optional file var ask-p)
  (let ((ASK-P (or ask-p current-prefix-arg)))
    (if ASK-P
        (funcall #'mtg--read-data :default-file "cards.mtg.el.gz" :default-var 'mtg-cards)
      (call-interactively #'mtg--read-data :file-prompt "" :var-prompt "" :default-file "cards.mtg.el.gz" :default-var 'mtg-cards))))


(cl-defun mtg-save-cards (&optional file var ask-p)
  (let ((ASK-P (or ask-p current-prefix-arg)))
    (if ASK-P
        (funcall #'mtg--write-data :default-file "cards.mtg.el.gz" :default-var 'mtg-cards :print-def nil :print-pretty nil)
      (call-interactively #'mtg--write-data :prompt "" :default-file "cards.mtg.el.gz" :default-var 'mtg-cards))))


(cl-defun mtg-load-prints
  )


(cl-defun mtg-save-prints
  )


(cl-defun mtg--load-data (file var &key file-prompt var-prompt)
  "Load FILE into VAR.
Prompt user with FILE-PROMPT and/or VAR-PROMPT."
  (interactive (list (mtg--read-file-name :prompt (or file-prompt "Read from file") :default "mtg-cards.json.el.gz")
                     (mtg--read-variable-name (or var-prompt "Load into variable") var)))
  (setf var load file)) 


(cl-defun mtg--save-data (var file &key var-prompt file-prompt print-def print-pretty)
  "Save VAR's value into FILE.
Print with newlines (¿and indentations?) if PRINT-PRETTY.
Print with a ‘defconst’ around VAR if PRINT-DEF.
Prompt user with VAR-PROMPT and/or FILE-PROMPT."
  (interactive
   (list
    (mtg--read-var-name (or var-prompt "Save variable") var nil)
    (mtg--read-file-name :prompt (or prompt "Write to file") :default "mtg-cards.json.el.gz")
    (y-or-n-p "Print within a ‘defconst’ (to be directly ‘load’able)?")
    (y-or-n-p "Print with newlines and indentations (much slower)?")
   )
  )
  (with-temp-file file
    (cond
      ((not (or print-def print-pretty))
       (prin1 (symbol-value var) (current-buffer)))
      ((and print-pretty (not print-def))
       (prin1-pp (symbol-value var) (current-buffer)))
      ((and print-def (not print-pretty))
       (prin1 `(defconst ,var (quote ,(symbol-value var))) (current-buffer)))
      ((and print-def print-pretty)
       ()
     )))


(defun mtg-write-card-data (&optional filepath object)
  "Write OBJECT to FILEPATH."
  (interactive
   (list
    (mtg--read-file-name "Write to file (mtg-cards.json.el.gz): " )
    (mtg--read-symbol " (): " 'mtg-data-))
  (let (())
    (mtg--write-elisp-data-file file object)))


(defun mtg-read-card-data (&optional filepath object)
  "Write OBJECT to FILEPATH."
  ())


(defun mtg-write-card-data (&optional filepath object)
  "Write OBJECT to FILEPATH."
  ())


(defun mtg--write-elisp-data-file (file object)
  "Write elisp OBJECT to FILE."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      ())))


(cl-defun mtg--read-file-name (prompt &optional default directories extensions)
  "Read a file name (DEFAULT-ing) under data DIRECTORIES, with PROMPT."
  (read-file-name (format "%s (default “%s”): " (or prompt "File") default) directories))


(cl-defun mtg--read-var-name (prompt &optional default feature)
  "Read a variable name (DEFAULT-ing) under feature FEATURE, with PROMPT."
  (read-variable (format "%s (default ‘%s’): " (or prompt "Variable") default) default))


;;


________________




MTG-EDIT-MODE


;;; ‘mtg-lint’:


(cl-defstruct (mtg-lint (:type list) (:named) (:constructor mtg-make-lint) (:constructor nil) (:copier nil))
  (id :type symbol) (wrong :type regexp) (right :type string) (since :type time) (desc "" :type string))


;;M-: (mtg-make-lint :id 'he-she->they :wrong (rx bow "he or she" eow) :right "they" :since 201 :desc "Since 201_, . See URL ‘’.")
;;M-: (mtg-make-lint :id ' :wrong  :right  :desc "")
;;


(cl-defstruct (mtg-abbrev (:type list) (:named) (:constructor mtg-make-abbrev) (:constructor nil) (:copier nil))
  (id :type symbol) (from :type string) (into :type string) (desc "" :type string) (gate nil :type (or nil function)) (case nil :type (or nil boolean)))


;;M-: (mtg-make-abbrev :id ' :from "" :into "")
;;


;;


TODO lint: capitalize each phrase in a comma-separated activation-cost. don't capitalize each phrase in a comma-separated keyword-list.


(defcustom mtg-mode-lint-alist
  '(


    (multimodal-ambitargeted :wrong mtg-rx/multimodal-ambitargeted :right nil
      :message "You shouldn't mix targeted modes and untargeted modes on a multimodal spell: the former can cause the latter to fizzle. (For example, replace “Choose one or both — • Target player sacrifices an artifact. • Draw a card.” with either “Choose one or both — • Target player sacrifices an artifact. • Target player draws a card.” or “Choose one or both — • Each opponent sacrifices an artifact. • You draw a card.”).")


    (they :wrong (rx bow "he or she" eow) :right "they" )
    (their :wrong (rx bow "his or her" eow) :right "their" )


    (this-spell :wrong (rx bow "When you cast ~" eow) :right "When you cast this spell" :since 201 :desc "See URL ‘’.")
    ( :wrong (rx bow "If ~ was kicked" eow) :right "If this spell was kicked" :since 201 :cond #'mtg-card-spell-p :desc "See URL ‘’.")


    (mana-cost-order :wrong (rx "{}") :right "" )  ; e.g. 
    (activation-cost-order :wrong (rx ) :right "" )  ; e.g. 


    (create :wrong (rx bow "put a \(\) token onto the battlefield" eow) :right "create a \1 token" )
    ;;^ “put a … token onto the battlefield” → “create a … token”
    (create-named :after '(create) :wrong #'mtg-mode-lint/create-named/wrong :right #'mtg-mode-lint/create-named/right )
    (create-legend )


    (activate :wrong (rx ". Activate this ability only ") :right ". Activate only " )


    ( :wrong (rx bow "" eow) :right "" :since 201 :desc "See URL ‘’.")
    ( :wrong (rx bow "" eow) :right "" :since 201 :desc "See URL ‘’.")
    ( :wrong (rx bow "" eow) :right "" :since 201 :desc "See URL ‘’.")
    ( :wrong (rx bow "" eow) :right "" :since 201 :desc "See URL ‘’.")


;;    ( :wrong (rx bow "" eow) :right "" :since 201 :desc "See URL ‘’.")
    )
  ""
  )


;;


;;; ‘mtg-abbr’:


(defcustom mtg-results-mode-abbrev-alist
  '(


    (etb   . ("enters the battlefield" "etb"))


    (eot . ("end of turn" "eot"))


  ))


(defcustom mtg-edit-mode-abbrev-alist
  '(


    (etb   . "When ~ enters the battlefield, ")
    (etboa . "Whenever ~ enters the battlefield or attacks, ")


    (ueot . "until end of turn")
    (uynt . "until your next turn")
    (ueoynt . "until the end of your next turn")


    (deal-damage . mtg-mode-abbrev/deal-damage)


    (counterspell . mtg-mode-abbrev/counterspell)


    (they-mill-lands-until . mtg-mode-abbrev/they-mill-lands-until)


;;    (mtg-make-abbrev :id ' :from "" :into "")
    )
  ""
  )


:enable-function :case-fixed


;;


(defun mtg-mode-make-lint-from ()
  )


(defun mtg-mode-lint/create-named/wrong ()
  )


(defun mtg-mode-lint/create-named/right ()
  )


;;


(defun mtg-mode-make-abbrev-from ()
  )


(defun mtg-mode-abbrev/they-mill-lands-until (count)
  (interactive "nHow many lands to mill? ")
  (format "Target opponent reveals cards from the top of their library until %d land cards are revealed. That player puts all cards revealed this way into their graveyard." (mtg--number-to-english count)))


;;e.g. (make-mtg-card :id 'mind-funeral :text (mtg-mode-abbrev/they-mill-lands-until 4)) → “Target opponent reveals cards from the top of their library until four land cards are revealed. That player puts all cards revealed this way into their graveyard.”


(defun mtg-mode-abbrev/deal-damage (n s)
  (interactive "dHow much damage: \nsWhat to damage: ")
  (format "~ deals %d damage to %s." n s))


(defun mtg-mode-abbrev/counterspell (s)
  (interactive "sWhat kind of spell to counter (can be blank): ")
  (cond ((string-equal-p "" s)
         "Counter target spell")
         (format "Counter target%s spell." (concat " " s)))
         ((string-prefix-p "with " s)
          (format "Counter target spell %s." s))
         (t
          (format "Counter target spell. %sv"))))


(defun mtg-mode-abbrev/ (s)
  (interactive "s: ")
  (format "%s" s))


;;(defun mtg-mode-abbrev/ (s)
;;  (interactive "s: ")
;;  (format "%s" s))


;;


;;; ‘mtg-edit-mode’:


;;


(define-derived-mode mtg-edit-mode 'prog-mode
  "mtg-edit-mode" "Parent major mode for editing M:tG-related text."
  (mtg-edit-mode-init))


(define-derived-mode mtg-edit-card-mode 'mtg-edit-mode
  "mtg-card-mode" "Major mode for editing custom M:tG cards."
  (mtg-edit-card-mode-init))


(define-derived-mode mtg-edit-edition-mode 'mtg-edit-mode
  "mtg-edition-mode" "Major mode for editing custom M:tG editions (i.e. sets and blocks)."
  (mtg-edit-edition-mode-init))


(define-derived-mode mtg-edit-deck-mode 'mtg-deck-mode
  "mtg-deck-mode" "Major mode for editing M:tG decklists."
  (mtg-edit-deck-mode-init))


;;


(defun mtg-edit-mode-init ()
  ())


;;


(defun mtg-edit-card-mode-init ()
  (define-abbrev-table 'mtg-edit-mode-abbrev-table (mtg-edit-mode-abbrev-table-from))
  (setq-local local-abbrev-table mtg-edit-mode-abbrev-table)
  (cl-loop for HOOK in mtg-edit-card-mode-completion-at-point-functions
    (add-hook 'completion-at-point-functions HOOK nil t))
  ())


(defcustom 'mtg-edit-card-mode-completion-at-point-functions
  (list #' mtg-card-name-completion-at-point
;;        #'mtg-card--completion-at-point
        )
  "")


(cl-defun mtg-edit-mode-abbrev-table-from (&optional (mtg-abbrev-alist mtg-mode-abbrev-alist))
  (let* ((emacs-abbrev-alist (cl-loop for ABBREV in mtg-abbrev-alist
           collect (list (mtg-abbrev-from ABBREV) (mtg-abbrev-into ABBREV) nil :enable-function (mtg-abbrev-gate ABBREV) :case-fixed (mtg-abbrev-case ABBREV)))))
    emacs-abbrev-alist))


;; abbrev-table-p := (abbrevname expansion [hook] [props...])


;;


(defun mtg-edit-edition-mode-init ()
  (setq-local outline-regexp mtg-edition-outline-regexp)
  (outline-minor-mode +1)
  (cl-loop for HOOK in mtg-edit-edition-mode-completion-at-point-functions
    (add-hook 'completion-at-point-functions HOOK nil t))
  ())


(defcustom 'mtg-edit-edition-mode-completion-at-point-functions
  (list #'mtg-edit-custom-card-name-completion-at-point
        #'mtg-edit-official-card-name-completion-at-point
;;        #'mtg-card--completion-at-point
        )
  "")


(defconst mtg-edition-outline-regexp
  "for ‘outline-regexp’."
  (rx ))


;;


(defun mtg-edit-deck-mode-init ()
  "Initialize ‘mtg-edit-deck-mode’."
  (use-local-map mtg-edit-deck-mode-map)
  (setq-local eldoc-documentation-function #'mtg-view-card-name-line-eldoc)
  (cl-loop for HOOK in mtg-edit-deck-mode-completion-at-point-functions
    (add-hook 'completion-at-point-functions HOOK nil t))
  ())


(defcustom 'mtg-edit-deck-mode-completion-at-point-functions
  (list #' mtg-card-name-line-completion-at-point
;;        #'mtg-card--completion-at-point
        )
  "")


(defun mtg-edit-deck-group-cards ()
  "Group by type, color, cost, etc."
  (interactive (list (mtg-read-card-prop "Group by: ")))
  ())


(defun mtg-edit-deck-sort-cards ()
  "Sort (within groups) by cost, count, etc."
  (interactive (list (mtg-read-card-prop "Sort by: ")))
  ())


(defun mtg-edit-deck-insert-color-stats (&optional -p)
  "Insert ."
  (interactive "P")


  (save-excursion
    (goto-char (point-min))
    (let* ((DECK (mtg-deck-parse-buffer))
           (STATS (mtg-deck-format-color-stats-oneline DECK)))
      (goto-char (point-min))
      (if (re-search-forward "// COLORS:" t)
          (insert )
        (insert (format "// COLORS: %s" STATS))))
    ()))


(defun mtg-edit-deck-insert-mana-value-stats (&optional -p)
  "Insert ."
  (interactive "P")
  ())


(defun mtg-edit-deck-insert-type-stats (&optional -p)
  "Insert ."
  (interactive "P")
  ())


(defun mtg-edit-deck-merge-card-lines (&optional -p)
  "Replace duplicate name lines with a single count+name line. Singular lines are still prefixed by “1”."
  (interactive "P")
  ())


(defun mtg-edit-deck-unmerge-card-lines (&optional -p)
  "Replace count+name lines with with that many name lines."
  (interactive "P")
  ())


(cl-defun mtg-edit-deck-inc-card-count (&optional card-name (count 1))
  "Increment the CARD-NAME at point by COUNT."
  (interactive ())
  ())


(cl-defun mtg-edit-deck-dec-card-count (&optional card-name (count 1))
  "Decrement the CARD-NAME at point by COUNT."
  (interactive ())
  (mtg-edit-deck-inc-card-count card-name (* -1 count)))


(cl-defun mtg-edit-deck-sideboard-card (&optional card-name (count 1))
  "Move the CARD-NAME at point from your maindeck into your sideboard."
  (interactive ())
  ())


(cl-defun mtg-edit-deck-maindeck-card (&optional card-name (count 1))
  "Move the CARD-NAME at point from your sideboard into your main deck."
  (interactive ())
  ())


;;


(defun mtg-view-deck-display-mana-value-bar-chart ()
  "."
  (interactive)
  ())


(defun mtg-view-deck-display-color-pie-chart ()
  "."
  (interactive)
  ())


;;


(defun mtg-deck-get-mana-value-stats (deck)
  "Return DECK's mana value statistics/distribution."
  ())


(defun mtg-deck-get-color-stats (deck)
  "Return DECK's color statistics/distribution."
  ())


(defun mtg-deck-get-type-stats (deck)
  "Return DECK's type statistics/distribution."
  ())


;;


(defun mtg-deck-parse-buffer (&optional -p)
  "Parse the ‘current-buffer’ into an ‘mtg-deck-p’, or nil."
  ())


;;


(defun mtg-card-parse-buffer (&optional -p)
  "Parse the ‘current-buffer’ into an ‘mtg-card-p’, or nil."
  ())


;;


(defun mtg-read-card-prop (&optional prompt)
  "Read an ‘mtg-card-prop-p’."
  (let ((PROMPT (or prompt "Card property: "))
        (COLLECTION (mtg-all-card-props)))
    (completing-read PROMPT COLLECTION )))


;;


(defvar js-mode-font-lock-keywords
  `((,(regexp-opt
       '("var" "for" "function" "if" "else")
       'symbols)
     . font-lock-keyword-face)


;;


(defun mtg-card-name-completion-at-point ()
  "Complete .
For hook ‘completion-at-point-functions’."
  (interactive)
  ())


(defalias 'mtg-card-name-completion-at-point #'mtg-edit-official-card-name-completion-at-point)


(defun mtg-edition-name-completion-at-point ()
  "Complete current word as within an ‘mtg-card-name’ phrase.
For hook ‘completion-at-point-functions’."
  (interactive)
  ())


(defun mtg-card-name-line-completion-at-point ()
  "Complete current line as an ‘mtg-card-name’.
For hook ‘completion-at-point-functions’."
  (interactive)
  ())


(defun mtg-complete-card-name (s)
  "Return completions for an ‘mtg-card-name’ from S."
  (let* ((CANDIDATES (mtg-known-card-names))
         )
    ()))


(defun mtg-read-card-name ()
  "Read an ‘mtg-card-name’."
  (let* ((CANDIDATES (mtg-known-card-names))
         )
    (completing-read CANDIDATES)))


(defun mtg-known-card-names ()
  "Return all known ‘mtg-card-name’s."
  (mtg-known-cards 'name))


(defun mtg-known-cards (&optional prop)
  "Return all known ‘mtg-card’s (or their mtg-card-<PROP>s)."
  (cl-loop for CARD across mtg-known-cards
    collect (if prop (mtg-card-get prop CARD) CARD)))


;;


mtg-ispell


dictionary is all words in all rules text across all cards (and flavor text?).


;;e.g. spellcheck “Stesnia”, frequently misspelled as “Stensia”.


;; ‘flyspell-mode’: ‘ispell-dictionary’? ‘ispell-complete-word-dict’?


;;


________________




MTG-VIEW-MODE


(define-derived-mode mtg-view-mode 'view-mode
  "mtg-view-mode" "Parent major mode for viewing M:tG-related things."
  (mtg-view-mode-init))


(define-derived-mode mtg-view-card-mode 'mtg-view-mode
  "mtg-view-card-mode" "Major mode for viewing M:tG cards."
  (mtg-view-card-mode-init))


(define-derived-mode mtg-view-edition-mode 'mtg-view-mode
  "mtg-view-edition-mode" "Major mode for viewing M:tG editions (i.e. sets and blocks)."
  (mtg-view-edition-mode-init))


(define-derived-mode mtg-view-deck-mode 'mtg-deck-mode
  "mtg-view-deck-mode" "Major mode for viewing M:tG decklists."
  (mtg-view-deck-mode-init))


;;


(defvar mtg-view-card-mode-map
  (let ((map (make-sparse-keymap)))


    (define-key map (kbd "c") mtg-view-card-mode-copy-map)


    (define-key map (kbd "") #'mtg-)


;;    (define-key map (kbd "") #'mtg-)


    map)
  "‘mtg-view-card-mode’s keybindings.")


(defvar mtg-view-card-mode-copying-map
  (let ((map (make-sparse-keymap)))


    (define-key map (kbd "n") #'mtg-mode-copy-current-card-name)
    (define-key map (kbd "") #'mtg-mode-copy-current-)
    (define-key map (kbd "") #'mtg-mode-copy-current-)
    (define-key map (kbd "") #'mtg-mode-copy-current-)
    (define-key map (kbd "") #'mtg-mode-copy-current-)
    (define-key map (kbd "") #'mtg-mode-copy-current-)
    (define-key map (kbd "") #'mtg-mode-copy-current-)
    (define-key map (kbd "") #'mtg-mode-copy-current-)


    (define-key map (kbd "n") #'mtg-tabulated-list-sort-by-name)
    (define-key map (kbd "v") #'mtg-tabulated-list-sort-by-mana-value)
    (define-key map (kbd "d") #'mtg-tabulated-list-sort-by-release-date)
    (define-key map (kbd "w") #'mtg-tabulated-list-sort-by-power)
    (define-key map (kbd "u") #'mtg-tabulated-list-sort-by-toughness)
    (define-key map (kbd "c") #'mtg-tabulated-list-sort-by-colors)
    (define-key map (kbd "t") #'mtg-tabulated-list-sort-by-types)
    (define-key map (kbd "k") #'mtg-tabulated-list-sort-by-keywords)
    (define-key map (kbd "") #'mtg-tabulated-list-sort-by-)
    (define-key map (kbd "") #'mtg-tabulated-list-sort-by-)
    (define-key map (kbd "s") #'tabulated-list-sort)


;;    (define-key map (kbd "") #'mtg-)


    map)
  "")


(defun mtg-view-card-mode-init ()
  "Initialize ‘mtg-view-card-mode’."
  (use-local-map mtg-view-card-mode-map)
  (setq-local eldoc-documentation-function #'mtg-view-card-eldoc)
  ())


;;


(defalias 'mtg-view-card-eldoc #'mtg-keyword-eldoc)


(cl-defun mtg-keyword-eldoc (&key keywords)
  "Echo the reminder text for the current keyword (in KEYWORDS)."
  (when-let* (
         (KEYWORD-RANGE (mtg-keyword-at-point nil :keywords keywords))
         (KEYWORD-NAME (buffer-substring (car KEYWORD-RANGE) (cdr KEYWORD-RANGE)))
         (KEYWORD-ID (mtg-parse-keyword KEYWORD-NAME))
         (KEYWORD (mtg-intern-keyword KEYWORD-ID :obarray keywords))
         (KEYWORD-REMINDER-TEXT (mtg-keyword-reminder-text KEYWORD))
         (KEYWORD-INFO (mtg-format KEYWORD-REMINDER-TEXT mtg-dummy-format-spec))
              )
    KEYWORD-INFO))


(cl-defun mtg-keyword-at-point (&optional position &key keywords)
  "Get the current keyword (in KEYWORDS) near POSITION."
  (let* ((CURRENT-POSITION (or position (point)))
         (ALL-KEYWORDS (or keywords mtg-all-keywords)))
    (save-excursion
      (when CURRENT-POSITION
        (goto-char CURRENT-POSITION))
      (thing-at-point 'mtg-keyword))))


(defun mtg-forward-keyword (&optional n)
  (interactive "P")  ;TODO
  (let ((N (round n)))  ;TODO
    (if (>= n 1)
        ()
      ())))


(put 'mtg-keyword 'forward-op #'mtg-forward-keyword)  ; register with ‘thingatpt’.


(cl-defun mtg-card-name-eldoc (&key names)
  )


;;


(defun mtg-edit-deck-mode-init ()
  "Initialize ‘mtg-view-card-mode’."
  (use-local-map mtg-view-card-mode-map)
  (setq-local eldoc-documentation-function #'mtg-view-card-eldoc)
  ())


;;


(defconst mtg-dummy-card
  (mtg-make-card :id a-b-c :name "A B C" :rules-text "…" ))


(defconst mtg-dummy-format-spec
  '((m . "{_}")
    (n . "‹N›")))


;;


(cl-defun mtg-parse-keyword (s)
  (let ((keyword-name ( s)))
    keyword-name))


(cl-defun mtg-intern-keyword (keyword-name &key obarray)
  (let ((keyword-id (intern-soft keyword-name (or obarray mtg-all-keywords))))
    keyword-id))


;;


________________




MTG-XYZ


;;


(define-derived-mode mtg-xyz-mode "mtg-xyz-mode" "Major mode for ing M:tG s."
  (mtg-xyz-mode-init))


(defvar mtg-xyz-mode-map
  (let ((map (make-sparse-keymap)))


    (define-key map (kbd "c") mtg-xyz-mode-copy-map)


    map)
  "‘mtg-xyz-mode’s keybindings.")


(defvar mtg-xyz-mode-copying-map
  (let ((map (make-sparse-keymap)))


    (define-key map (kbd "") #'mtg-xyz-)


    map)
  "")


(defun mtg-xyz-mode-init ()
  "Initialize ‘mtg-xyz-mode’."
  (use-local-map mtg-xyz-mode-map)
  (setq-local eldoc-documentation-function #'mtg-xyz-eldoc)
  ())


(defalias 'mtg-xyz-eldoc #'mtg-xyz-wuv-eldoc)


(defun mtg-xyz-wuv-eldoc (&optional )
  "Echo  for the current ."
  (let* ((WUV-AT-POINT (thing-at-point 'mtg-wuv))
         (WUV-INFO (mtg-wuv- WUV-AT-POINT))
         )
    WUV-INFO))


(defun mtg-wuv-at-point (&optional posn)
  "Get the current wuv."
  (save-excursion
    (when CURRENT-POSITION
      (goto-char (or posn (point))))
    (thing-at-point 'mtg-wuv))))


(put 'mtg-wuv 'forward-op #'mtg-forward-wuv)  ; register with ‘thingatpt’.


(defun mtg-forward-wuv (&optional n)
  (interactive "P")  ;TODO
  (let ((N (round n)))  ;TODO
    (if (>= n 1)
        ()
      ())))


;;


________________




MTG-CL


;;; ‘mtg-cl’


(cl-deftype mtg-set ()
  `(satisfies mtg-set-p))


(cl-defun mtg-set-p (xs)
  "XS is a set.
• nil: the empty set.
• t: the full set.
• cons: a listp of symbolp's."
  (or (booleanp xs)
      (and (proper-list-p xs)
           (cl-loop for x in xs always (symbolp x)))))


(cl-deftype mtg-set-of (type)
  `(satisfies (mtg-set-of-p ,type)))


(cl-defun mtg-set-of-p (type)
  ""
  (lambda (xs)
    (or (booleanp xs)
        (and (proper-list-p xs)
             (cl-loop for x in xs always (cl-typep x type))))))


(cl-deftype mtg-char ()
  (let ((max-char (max-char)))
    `(integer 0 ,max-char)))


;; c.f. 'character (and 'string-char), which are '(integer 0 255) only.
;; '(satisfies characterp)? '(integer 0 (max-char))? '(integer 0 1114111)?




;;


________________




MTG-TEST


;;; ‘mtg-test’:


;;


llanowar-mentor_en.card.mtg


Llanowar Mentor {G}
Creature — Elf Spellshaper
{G}, {T}, Discard a card: Create a 1/1 green Elf Druid creature token named Llanowar Elves. It has “{T}: Add {G}.”
>“One day the vines of this forest will be as much a part of you as your own fingers.”
1/1


llanowar-mentor_es.card.mtg


Mentor de Llanowar {G}
Criatura — Cambiahechizos elfo
{G}, {T}, descartar una carta: Crea una ficha de criatura Druida Elfo verde 1/1 llamada Elfos de Llanowar. Tiene "{T}: Agrega {G}
>“Algún día las lianas de este bosque serán tan parte de ti como tus dedos”.
1/1


llanowar-mentor_fr.card.mtg


Mentor de Llanowar {G}
Créature : elfe et sortisan
{G}, {T}, défaussez-vous d'une carte : Créez un jeton de créature 1/1 verte Elfe et Druide appelé Elfes de Llanowar. Il a « {T} : Ajoutez {G}. »
>« Un jour, les lianes de cette forêt feront partie de toi, autant que tes propres doigts. »
1/1


llanowar-mentor_de.card.mtg


Llanowar-Lehrmeister {G}
Kreatur — Elf, Spruchwandler
{G}, {T}, wirf eine Karte ab: Erzeuge einen 1/1 grünen Elf-Druide-Kreaturenspielstein namens Llanowarelfen. Er hat „{T}: Erzeuge {G}."
>„Eines Tages werden die Ranken dieses Waldes ebenso sehr ein Teil von dir sein wie deine eigenen Finger.“
1/1


llanowar-mentor_it.card.mtg


Mentore di Llanowar
{G} Creatura — Mutamagia Elfo
{G}, {T}, Scarta una carta: Crea una pedina creatura Druido Elfo 1/1 verde chiamata Elfi di Llanowar. Ha “{T}: Aggiungi {G}”.


>“Un giorno, le liane di questa foresta faranno parte di te quanto le tue stesse dita.”
1/1


llanowar-mentor_pt.card.mtg


Mentor de Llanowar {G}
Criatura — Elfo Magimodelador
{G}, {T}, descarte um card: Crie uma ficha de criatura verde 1/1 do tipo Elfo Druida com o nome Elfos de Llanowar. Ela tem “{T}: Adicione {G}”.
>“Um dia as gavinhas dessa floresta farão tanto parte de você quanto seus próprios dedos.”
1/1


llanowar-mentor_ja.card.mtg


ラノワールの助言者 {G}
クリーチャー — エルフ・スペルシェイパー
{G}, {T}, カード１枚を捨てる：「ラノワールのエルフ」という名前の緑の１/１のエルフ・ドルイド・クリーチャー・トークン１体を生成する。それは「{T}：{G}を加える。」を持つ。


>「この森の蔦が、自分自身の指と同じ程に自らの一部となるときが、いつの日か来るだろう。」
1/1


llanowar-mentor_ru.card.mtg


Лановарский Наставник {G}
Существо — Эльф Заклинатель
{G}, {T}, сбросьте карту: создайте одну фишку существа 1/1 зеленый Эльф Друид с именем Лановарские Эльфы. У нее есть способность «{T}: добавьте {G}».
>«Однажды лианы этого леса станут для тебя продолжением твоих собственных пальцев».
1/1


llanowar-mentor_zhs.card.mtg


罗堰明师
{G} 生物 ～妖精／塑法师
{G}，{T}，弃一张牌：派出一个1/1绿色，名称为罗堰妖精的妖精／德鲁伊衍生生物。它具有「{T}：加{G}。」
>「总有一天，这树林中的藤蔓就会像你的手指一样，成为你身体的一部分。」
1/1


llanowar-mentor_zht.card.mtg


羅堰明師 {G}
生物 ～妖精／塑法師
{G}，{T}，棄一張牌：派出一個1/1綠色，名稱為羅堰妖精的妖精／德魯伊衍生生物。它具有「{T}：加{G}」。
>「總有一天，這樹林中的藤蔓就會像你的手指一樣，成為你身體的一部分。」
1/1


;;


________________


NOTES
________________


---






---


invocation-name
;;→ "Emacs"
invocation-directory
;;→ "/Applications/Emacs.app/Contents/MacOS/"


---


(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))


---


(require 'thread)


(thread-first 9
  (- 6)
  (/ 3))
;;→ (/ (- 9 6) 3) → 1


(thread-last "123"
  ;; "123"
  (concat "abc" " ")
  ;; "abc 123"
  (replace-regexp-in-string "[a-z]" #'upcase)
  ;; "ABC 123"
  (format "(%s)")
  )
;;→ (format "(%s)" (replace-regexp-in-string "[a-z]" #'upcase (concat "abc" " " "123"))) → "(abc 123)"


---


CEDET.el:


- `bovine.el` — a `LL` parser generator. 
- `wisent.el` — a `LALR(1)` parser generator. Bison port. 
- 


CFGs:


- `LL(*)` — “Left-to-right, Leftmost-derivation”. 
- `LALR(1)` — “Look-Ahead LR” (“Look-Ahead Left-to-right, Rightmost-derivation”). 


---


`cl` --


(cl-number N)
;; [0…N]


(cl-check-type x (integer 1 *) "a positive integer")


(cl-assert (> x 10) t "x is too small: %d")


- (member 1 2 3 4) = (integer 1 4)
- (member nil) = null = (satisfies null)
- (integer * *) = integer
- (number * *) = number


(cl-deftype  ()
  `(satisfies ))


(cl-isqrt N)
;; (= 2 (cl-isqrt 4) (cl-isqrt 5) (cl-isqrt 6) (cl-isqrt 7) (cl-isqrt 8))


(cl-parse-integer STRING &key START END RADIX JUNK-ALLOWED)






---


`pcase` --


`pcase(')` --


    - 
    - e.g. `(pcase x ('y …))` = `(cond ((equal 'y x) …))` = `(cond ((eq 'y x) …))`
    - e.g. `(pcase x (':y …))` = e.g. `(pcase x (:y …))` = `(cond ((equal :y x) …))`
    - e.g. `(pcase x ('?y …))` = e.g. `(pcase x (?y …))` = `(cond ((equal ?y x) …))` = `(cond ((eql ?y x) …))`
    - e.g. `(pcase x ('"yes" …))` = `(pcase x ("yes" …))` = `(cond ((equal "yes" x) …))`


`pcase(pred)` --


    - `(pred FUNCTION)` = `(app FUNCTION identity)`
    - `(pred FUNCTION)` = `(and x (guard (FUNCTION x)))`
    - e.g. `(pcase x ((pred natnump) …))` = `(cond ((natnump x) …))`
    - e.g. `(pcase x ((pred (<= 0)) …))` = `(cond ((<= 0 x) …))` = `(cond ((>= x 0) …))`


`pcase(app)` --


    - ``


`pcase(guard)` --


    - ``


`pcase(let)` --


    - 
    - c.f. `pcase(and)`: `(pcase (read-number "Halve: ") ((let x (pred evenp)) (/ x 2)))` ~ `(pcase (read-number "Halve: ") ((and x (pred evenp)) (/ x 2)))`


`pcase(rx)` --


    - 
    - e.g. `(pcase x ((rx "") …))` = `(cond ((string-match (rx "") x) …))`




`pcase(and)` --


    - ``


`pcase(or)` --


    - ``


`pcase(,x ,y)` (i.e. patterns) & `pcase(,x ,x)` (i.e. nonlinear patterns) --


    - e.g. `(pcase '(a a a) (`(,x ,x ,x) 3) (`(,x ,y ,z) 0))` ≈ `(let ((xs (list 'a 'a 'a))) (cond ((and (= 3 (length xs)) (eq (car xs) (cadr xs)) (eq (cadr xs) (caddr xs))) 3) ((= 3 (length xs)) 0) (t nil)))` 


- `pcase-let*` & `pcase-let` & `pcase-dolist` --


    - 


- `pcase-defmacro` --


    - `pcase-defmacro` extends `pcase`.
    - 


- <https://www.gnu.org/software/emacs/manual/html_node/elisp/pcase-Macro.html>


(pcase-defmacro my/less-than (n)
  "Matches if EXPVAL is a number less than N."
  `(pred (> ,n)))


(pcase-defmacro my/less-than-or-equal-to (n)
  "Matches if EXPVAL is a number less than or equal to N."
  `(or (pred (eql ,n))
       (my/less-than ,n)))


(defun evaluate (form env)
  (pcase form
    (`(add ,x ,y)      (+ (evaluate x env)
                          (evaluate y env)))
    (`(app ,f ,x)      (funcall (evaluate f env)
                                (evaluate x env)))
    (`(abs ,k ,body)   (lambda (v)
                         (evaluate body (cons `(,k . ,v) env))))
    ((pred numberp)    form)
    ((pred symbolp)    (cdr (assq form env)))
    (_                 (error "Syntax error: %S" form))))


---


n.b. (re-search-forward regexp nil nil nil) ≈ (re-search-forward regexp (point-max) nil 1)


(defun eg/re-search-backward (regexp count)
  (re-search-forward regexp nil t (* -1 count)))


(defun eg/re-search-line-forward (regexp count)
  (re-search-forward regexp (line-beginning-position) t count))


---
;;


(defun rx--translate (item)
  "Translate the rx-expression ITEM.  Return (REGEXP . PRECEDENCE)."
  (cond
   ((stringp item)
    (if (= (length item) 0)
        (cons nil 'seq)
      (cons (list (regexp-quote item)) (if (= (length item) 1) t 'seq))))
   ((characterp item)
    (cons (list (regexp-quote (char-to-string item))) t))
   ((symbolp item)
    (rx--translate-symbol item))
   ((consp item)
    (rx--translate-form item))
   (t (error "Bad rx expression: %S" item))))


(defun rx--translate (item)
  "Translate the rx-expression ITEM.  Return (REGEXP . PRECEDENCE)."
  (cond
   ((stringp item)
    (if (= (length item) 0)
        (cons nil 'seq)
      (cons (list (regexp-quote item)) (if (= (length item) 1) t 'seq))))
   ((characterp item)
    (cons (list (regexp-quote (char-to-string item))) t))
   ((symbolp item)
    (rx--translate-symbol item))
   ((consp item)
    (rx--translate-form item))
   (t (error "Bad rx expression: %S" item))))


(defun rx--translate-form (form)
  "Translate an rx form (list structure).  Return (REGEXP . PRECEDENCE)."
  (let ((body (cdr form)))
    (pcase (car form)
      ((or 'seq : 'and 'sequence) (rx--translate-seq body))
      ((or 'or '|)              (rx--translate-or body))
      ((or 'any 'in 'char)      (rx--translate-any nil body))
      ('not-char                (rx--translate-any t body))
      ('not                     (rx--translate-not nil body))
      ('intersection            (rx--translate-intersection nil body))


      ('repeat                  (rx--translate-repeat body))
      ('=                       (rx--translate-= body))
      ('>=                      (rx--translate->= body))
      ('**                      (rx--translate-** body))


      ((or 'zero-or-more '0+)           (rx--translate-rep "*" rx--greedy body))
      ((or 'one-or-more '1+)            (rx--translate-rep "+" rx--greedy body))
      ((or 'zero-or-one 'opt 'optional) (rx--translate-rep "?" rx--greedy body))


      ('*                       (rx--translate-rep "*" t body))
      ('+                       (rx--translate-rep "+" t body))
      ((or '\? ?\s)             (rx--translate-rep "?" t body))


      ('*?                      (rx--translate-rep "*" nil body))
      ('+?                      (rx--translate-rep "+" nil body))
      ((or '\?? ??)             (rx--translate-rep "?" nil body))


      ('minimal-match           (rx--control-greedy nil body))
      ('maximal-match           (rx--control-greedy t   body))


      ((or 'group 'submatch)     (rx--translate-group body))
      ((or 'group-n 'submatch-n) (rx--translate-group-n body))
      ('backref                  (rx--translate-backref body))


      ('syntax                  (rx--translate-syntax nil body))
      ('not-syntax              (rx--translate-syntax t body))
      ('category                (rx--translate-category nil body))


      ('literal                 (rx--translate-literal body))
      ('eval                    (rx--translate-eval body))
      ((or 'regexp 'regex)      (rx--translate-regexp body))


      (op …))))


(defun rx--translate-bounded-repetition (name body)
  (let ((min-count (car body))
        (max-count (cadr body))
        (items (cddr body)))
    (unless (and (natnump min-count)
                 (natnump max-count)
                 (<= min-count max-count))
      (error "rx `%s' range error" name))
    (rx--translate-counted-repetition min-count max-count items)))


(defconst rx--builtin-forms
  '(seq sequence : and or | any in char not-char not intersection
    repeat = >= **
    zero-or-more 0+ *
    one-or-more 1+ +
    zero-or-one opt optional \?
    *? +? \??
    minimal-match maximal-match
    group submatch group-n submatch-n backref
    syntax not-syntax category
    literal eval regexp regex)
  "List of built-in rx function-like symbols.")


(defconst rx--builtin-symbols
  (append '(nonl not-newline any anychar anything unmatchable
            bol eol line-start line-end
            bos eos string-start string-end
            bow eow word-start word-end
            symbol-start symbol-end
            point word-boundary not-word-boundary not-wordchar)
          (mapcar #'car rx--char-classes))
  "List of built-in rx variable-like symbols.")
(defun rx--translate-form (form)
  "Translate an rx form (list structure).  Return (REGEXP . PRECEDENCE)."
  (let ((body (cdr form)))
    (pcase (car form)
      ((or 'seq : 'and 'sequence) (rx--translate-seq body))
      ((or 'or '|)              (rx--translate-or body))
      ((or 'any 'in 'char)      (rx--translate-any nil body))
      ('not-char                (rx--translate-any t body))
      ('not                     (rx--translate-not nil body))
      ('intersection            (rx--translate-intersection nil body))


---


(with-output-to-string
  (prin1 "abc")
  (prin1 "def"))
=
(progn
  (prin1-to-string "abc")
  (prin1-to-string "def"))
=
"abcdef"


(with-output-to-file
(prin1 card)
(terpri)
)


---


italicize 
embolden
underline


'(:weight )  ; embolden
'(:slant )  ; italicize


(defun italicize (string)
  (let* ((OLD-FACE (get-text-property 0 'face string))
         (NEW-FACE (plist-put OLD-FACE '(italics t)))
    (propertize string
 'face NEW-FACE)))


---


:


- `(declare (pure t))` says "this function is pure, i.e. it gives you the same output if you give it the same input. pure functions can be evaluated during compilation (thus saving runtime).
- `(declare (side-effect-free t))` says "this function has no side effects, i.e. there's . uneffectful functions can safely be called more times (e.g. inlining, ) and/or fewer times (e.g. don't call it if the output is ignored, so `(progn (foo) …)` can be optimized to `(progn …)`).


(defun norm (x)
  (declare (pure t) (side-effect-free t))
  (* x x))


---


`defgeneric` —


- `(defgeneric CLASS-NAME )`


---


`defmethod` —


- `(defmethod METHOD-NAME ((ARGUMENT-NAME SPECIALIZER) …) &rest BODY)` where


    - `SPECIALIZER` can 


`SPECIALIZER`:


    - `t` — any type. e.g. `(defmethod ÷ ((x t) (y t)) …)` is `(defmethod ÷ (x y) …)`, the default function of the method.


    - `TYPE` — dispatch on TYPE.


    - `(eql VALUE)` — dispatch on a `symbolp` or `numberp`.


e.g.:


``` elisp
(require 'eieio)


(defgeneric ÷ (x y))


(defmethod ÷ ((x number) (y number)) (/ x y))


(defmethod ÷ ((x integer) (y integer)) (make-ratio :numerator x :denominator y))


(defmethod ÷ ((x (eql 0)) y) 0)


(defmethod ÷ (x (y (eql 0))) '∞)


(defmethod ÷ ((x (eql 0)) (y (eql 0))) 'NaN)
```


---


________________




(cl-union (list x) xs) ≈ (cl-adjoin x xs) ≈ (unless (cl-member x xs) (cons x xs))


________________




(cl-incf (aref i xs) n)
(cl-callf + (aref i xs) n)
(cl-setf (+ (aref i xs) n) (aref i xs))
(aref i xs (+ (aref i xs) n)))


________________




[Desktop Entry]
Version=1.0
Type=Application
Name=Foo Viewer
Comment=The best viewer for Foo objects available!
TryExec=fooview
Exec=fooview %F
Icon=fooview
MimeType=image/x-foo;
Actions=Gallery;Create;


[Desktop Action Search]
Exec=fooview --gallery
Name=Browse Gallery


[Desktop Action Update]
Exec=fooview --create-new
Name=Create a new Foo!
Icon=fooview-new


________________




queue.el
tNFA.el
heap.el
trie.el
dict-tree.el


trie.el


: finding all strings with a given prefix, finding approximate matches, finding all strings matching a regular expression, returning results in alphabetical or any other order, returning only the first few results, etc.


You create a trie using `make-trie',
create an association using `trie-insert',
retrieve an association using `trie-lookup',
and map over a trie using `trie-map', `trie-mapc', `trie-mapcar', or `trie-mapf'.


You can find completions of a prefix sequence using `trie-complete',
search for keys matching a regular expression using `trie-regexp-search',
find fuzzy matches within a given Lewenstein distance (edit distance) of a string using `trie-fuzzy-match',
and find completions of prefixes within a given distance using `trie-fuzzy-complete'.


Using `trie-stack', you can create an object that allows the contents of the trie to be used like a stack;
`trie-stack-pop' pops elements off the stack one-by-one, in "lexicographic" order,
whilst `trie-stack-push' pushes things onto the stack.
Similarly, `trie-complete-stack', `trie-regexp-stack', `trie-fuzzy-match-stack' and `trie-fuzzy-complete-stack' create "lexicographicly-ordered" stacks of query results.






heap.el


(make-heap COMPARE-FUNCTION &optional INITIAL-SIZE RESIZE-FACTOR)


(make-heap #' _)  ; A→Z
(make-heap #' _)  ; Z→A




queue.el






tNFA.el




A tagged NFA can be created from a regular expression using `tNFA-from-regexp',
and its state can be updated using `tNFA-next-state'.
You can discover whether a state is a matching state using `tNFA-match-p',
extract subgroup capture data from it using `tNFA-group-data',
check whether a state has any wildcard transitions using `tNFA-wildcard-p',
and get a list of non-wildcard transitions using `tNFA-transitions'.
Finally, `tNFA-regexp-match' uses tagged NFAs to decide whether a regexp matches a given string.




dict-tree.el


The dictionary tree data structures are a hybrid between tries and hash tables. Data is stored in a trie, but results that take particularly long to retrieve are cached in hash tables, which are automatically synchronised with the trie. The dictionary package provides persistent storage of the data structures in files, and many other convenience features.


;;


Database Indices:


• by ‘mtg-card-name’ — in a case-insensitive suffix-tree (via ‘trie’).
• by ‘mtg-card-text’ — each line of words, in a (via ‘’). match whole words with “\b…\b” regexps; match whole lines with “^…$” regexps.
• by ‘mtg-card-types’ — in a  (via ‘hash-table’).
• by ‘mtg-card-cost’ — in a … (via ‘’).
• by ‘mtg-card-value’ — in a … (via ‘’).
• by ‘mtg-card-size’ — , in a … (via ‘’).


• by ‘mtg-card-’ — in a … (via ‘’).


;;


(defun mtg0-make-card-bit-vector (&optional cards)
  (let ((n-cards (length (or card mtg-all-cards))))
    (make-bit-vector n-cards nil)))


;; the indexed databases return card-sets as ~20,000 bit bool-vectors (and print-sets as ~???,000 bit bool-vectors), which complex logical queries (AND's, OR's, NOT's) can efficiently merge (via ‘bit-vector-intersection’, ‘bit-vector-union’, ‘bit-vector-not’). 


________________




(url-retrieve-synchronously URL &optional SILENT NO-COOKIES TIMEOUT)


(url-retrieve URL CALLBACK &optional CALLBACK-ARGS SILENT NO-COOKIES)
where (apply CALLBACK STATUS CALLBACK-ARGS)


(url-queue-retrieve URL CALLBACK &optional CALLBACK-ARGS SILENT NO-COOKIES)


This function acts like url-retrieve, but with limits on the number of concurrently-running network processes. The option url-queue-parallel-processes controls the number of concurrent processes, and the option url-queue-timeout sets a timeout in seconds. To use this function, you must (require 'url-queue).


`url-automatic-caching` —
- `(defvar url-automatic-caching ¿t?)`


`url-cache-directory` —
- `(defvar url-cache-directory ( url-configuration-directory "cache"))`


`(url-cache-creation-function)` —
- `(stringp (url-cache-creation-function STRING))`
- either `url-cache-create-filename-using-md5` (fewer collisions) or `url-cache-create-filename-human-readable` (less confusion).
- e.g. `(url-cache-create-filename-human-readable "http://www.example.com/foo/bar")` ⇒ `"/home/fx/.url/cache/fx/http/com/example/www/foo/bar"`
- e.g. `(url-cache-create-filename-using-md5 "http://www.example.com/foo/bar")` ⇒ `"/home/fx/.url/cache/fx/http/com/example/www/b8a35774ad20db71c7c3409a5410e74f"`


________________




`(intern STRING &optional (OBARRAY obarray))` —
- 
- e.g.


`(intern-soft STRING &optional (OBARRAY obarray))` —
- ≈ `(intern STRING &optional OBARRAY)` if `STRING` in `OBARRAY` else `nil`.
- e.g.


OBArrays —
- e.g. `(defun make-obarray (n) "Mercer-prime-sized obarray. See ‘https://en.m.wikipedia.org/wiki/Mersenne_prime’. e.g. “(eql 8191 (length (make-obarray 13))) ”. n.b. N (→ ‘length’): 2 (3), 3 (7), 5 (31), 7 (127), 13 (8,191), 17 (131,071), 19 (524,287), 31 (2,147,483,647), …" (make-vector (- (** 2 n) 1) 0))`.


________________




`(define-error ERROR MESSAGE &optional SUPER-ERROR)` —
- 
- e.g. `(define-error 'unnatural "Number is negative and/or non-integer" 'wrong-type-argument)`


`(signal ERROR-SYMBOL ERROR-DATA)` —
- 
- e.g. `(let ((d (- x y))) (if (natnump d) d (signal 'unnatural "Difference ‘(%d-%d)’ should be a ‘natnump’." x y)))`.
- >Error signaling and handling have some resemblance to throw and catch (see Catch and Throw), but they are entirely separate facilities. An error cannot be caught by a catch, and a throw cannot be handled by an error handler (though using throw when there is no suitable catch signals an error that can be handled).


`(error STRING &rest OBJECTS)` —
- ≈ `(signal 'error \`(format-message STRING ,@(cons STRING OBJECTS)))`.
- 


`(condition-case ERROR-VARIABLE FORM &rest ERROR-HANDLERS)` —
- `(condition-case nil FORM)` ≈ `(ignore-errors FORM)`
- `(condition-case nil FORM (error data) (apply #'message data))` ≈ `(with-demoted-errors "%s" FORM)`
- e.g. rethrow `user-error`s: `(condition-case e (…) (user-error (signal (car e) (cdr e)))`
- e.g. catch `file-error`s only: `(condition-case nil (…) ((file- file-) ()) ((file-) ()))`


`(condition-case-unless-debug…)` —
- ≈ `(if debug-on-error (…) (condition-case () (…) ((user-error data) (signal 'user-error ,@data)) ((error _) ‹…debug…›))`
- 


`(with-demoted-errors …)` —
- e.g. `(with-demoted-errors "%s" …)`
- n.b. `with-demoted-errors` wraps `condition-case-unless-debug`. 


`(ignore-errors …)` —
- `(ignore-errors …)` ≈ `(ignore-error (error) …)`
- e.g. `(ignore-errors (delete-file "…"))`


`(ignore-error ERROR-OR-ERRORS …)` —
- e.g. `(ignore-error end-of-file (read "…"))`


`(wrong-type-argument PREDICATE VALUE)` —
- (wrong-type-argument #'stringp …)


e.g. `condition-case`:


    (condition-case _
        (with-temp-buffer
          (insert-file "data.json")
          (json-read))
      (file-error (…))
      (json-error (…))


e.g. `unwind-protect`:


    (unwind-protect (…))


e.g. `condition-case`:


    (defmacro with-return-failure (&rest body)
      `(condition-case nil
           (prog1 t
             ,@body)
         (error nil)))


    ;; e.g. (with-return-failure (forward-sexp))


________________




(defun mtg0-json-cards-data (&optional data)
  (let ((DATA (or data mtg0-json-cards-data)))
    (cdr (assq 'data DATA))))
byte-compile-inline-expand
(defun mtg0-json-prints-data (&optional data)
  (let ((DATA (or data mtg0-json-prints-data)))
    (cdr (assq 'data DATA))))
byte-compile-inline-expand
(defun mtg0-json-get-card (name &optional data)
  (let ((DATA (mtg0-json-cards-data data))
        (NAME (intern name)))
    (cdr (assq NAME DATA))))
byte-compile-inline-expand
(defun mtg0-json-get-print (name edition &optional ccn data)
  (let ((DATA (mtg0-json-prints-data data))
        (NAME (intern name)))
    (cdr (assq NAME DATA))))
byte-compile-inline-expand
;;e.g. (mtg0-json-get-print "Delver of Secrets // Insectile Aberration" "MID")


(defun mtg0-card (card)
  ())


________________




:
• ‘’ 
• ‘<’ left-truncate (given a width).
• ‘>’ right-truncate (given a width).
• ‘-’ right-pad (don't left-pad).
• ‘0’ pad with zeroes (not spaces).
• ‘^’ ‘upcase’.
• ‘_’ ‘downcase’.


e.g. “%>^-3k” means: truncate it down from the right (until width), uppercase it, pad it up to the right with spaces (until width), with a width of 3 characters, it is the card's k. 


see also ‘format-spec’.


________________




(assoc        k kvs #'equal)
(assoc-string k kvs :case-fold)
(cdr (assq    k kvs))


(defun copy-mtg0-card (card)
  (with-mtg0-card
    (make-mtg0-card
      :cost (copy-tree .cost t)
      :text (copy-tree .cost t)
      :name (copy- .name)
      : (copy- .)
      : (copy- .)
      : (copy- .)
      :text (copy-vector .text)
      :legality (copy-alist .legality)
      :power (mtg0-numeric-copy .power)
      :toughness (mtg0-numeric-copy .toughness)
      :loyalty (mtg0-numeric-copy .loyalty)
      : .
     )))


________________




https://raw.githubusercontent.com/nickdrozd/reazon/main/reazon.el


e.g. `append` function:


``` elisp
(defun _append (head tail)
  (let ((out
         (if (null head)
             tail
           (let* ((a (car head))
                  (d (cdr head))
                  (rec (_append d tail)))
             (cons a rec)))))
    out))


(list
 (_append '(1 2 3) '())
 (_append '() '(4 5 6))
 (_append '(1 2 3) '(4 5 6)))
```


e.g. `appendo` relation:


``` elisp
(require 'reazon)


(reazon-defrel appendo (head tail out)
  (reazon-conde
   ((reazon-== head '()) (reazon-== out tail))
   ((reazon-fresh (a d rec)
      (reazon-== head (cons a d))
      (reazon-== out (cons a rec))
      (appendo d tail rec)))))


:


``` elisp
;; M-:
(reazon-run* (head tail)
  (appendo head tail '(1 2 3 4 5 6)))


'(
 (nil
  (1 2 3 4 5 6))
 ((1)
  (2 3 4 5 6))
 ((1 2)
  (3 4 5 6))
 ((1 2 3)
  (4 5 6))
 ((1 2 3 4)
  (5 6))
 ((1 2 3 4 5)
  (6))
 ((1 2 3 4 5 6)
  nil)
)
```


________________




(copy-tree TREE t)
copy cons-trees and vector-trees.


for copying `stringp`s, prefer `(copy-sequence …)` over `(substring-no-properties 0 …)` or `(concat … "")`.


`(sort SEQUENCE PREDICATE)` —
- sort is stable.
- sort is mutable on lists and immutable on vectors.
- `PREDICATE` must be anti-symmetric and transitive; e.g. `#'<`, c.f. `#'<=` which is s transitive but symmetric.
- 


`(reverse SEQUENCE)`
- immutably reverses. c.f. `nreverse`.
- 


``` elisp
(reverse '(a c a b))
;;⇒ '(b a c a))


(reverse [a c a b])
;;⇒ [b a c a])


(reverse "ACAB")
;;⇒ "BACA"
```


`(seq-sort-by FUNCTION PREDICATE SEQUENCE)` —
- `(seq-sort PREDICATE SEQUENCE)` ≈ `(seq-sort-by #'identity PREDICATE SEQUENCE)`
- 


`(seq-group-by FUNCTION SEQUENCE)`
- .
- 


``` elisp
(seq-group-by #'evenp '(1 2 3 0))
;;⇒ '((t 2 0) (nil 1 3))


(seq-group-by #'car '((a 1) (c 2) (a 3) (b 4)))
;;⇒ '((a (a 1) (a 3)) (c (c 2)) (b (b 4)))
```


`(seq-uniq SEQ)`
- set constructor.
- 


`(seq-set-equal-p SEQ1 SEQ2 &optional (TEST #'equal))`
- (multi-?)set equality.
- 


`(seq-intersection SEQ1 SEQ2 &optional (PROJECT #'identity))`
- set intersection.
- 


`(seq-let VARIABLES SEQUENCE)`
- .
- 


``` elisp
(seq-let [x y] [1 2 3]
  (+ x y))
;;⇒ 3


(seq-let (_ c _ b) '(1 2 3 4)
  (list b c))
;;⇒ '(4 2)
```


`(seq-random-elt SEQUENCE)`
- uniform random sampling.
- 


`(seq- SEQUENCE)`
- .
- 


________________




(require 'smie)
(defvar sample-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("begin" insts "end")
            ("if" exp "then" inst "else" inst)
            (id ":=" exp)
            (exp))
      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ("(" exps ")"))
      (exps (exps "," exps) (exp)))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "+") (assoc "*")))))


________________




(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))


(use-package ess-site
  :load-path "site-lisp/ess/lisp/"
  :commands R)


________________