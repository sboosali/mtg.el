# `mtg.el`

`mtg` is a *Multi-Library Package* for editing MTG cards, searching through all MTG cards, and parsing MTG cards/decklists.

## Links

* <https://github.com/sboosali/mtg.el>
* <>
* <>

## Multi-Library Package

`mtg` is a *Multi-Library Package*, with these *Elisp Libraries*:

* `mtg.el`
* `mtg-types.el`
* `mtg-mode.el`
* `mtg-query.el`
* `mtg-company.el`

## Completion

### Examples

### `mtg-company`

*Company Backend* for *MTG Cards*.

## Search

### Search Syntax

an *MTQ Expression* is a query which matches zero-or-more *MTG Cards*.

#### Search Operators

*Match Operators*:

* `”…"` — Match the *Name(s)*. 

* `*…` — Match the *Types* (including *Card Types*, *Subtypes*, *Supertypes*). e.g. `*instant` means *“Instant cards”*. `*` is a *Prefix Operator*. Matches aliases too (see `mtg-syntax-type-aliases-alist`); e.g. `*spell` is an alias for `*instant | *sorcery`

* `%` — Match the *Colors*. e.g. `%u` means *“a blue card”*. a *Prefix Operator*.

* `#` — Match the *Mana Cost*. Structurally, a *Mana Cost* is a <https://en.m.wikipedia.org/wiki/Multiset *Multiset*>. Commas separate *Logical Conjunctions* a.k.a. *Set Intersections*. e.g. `#bp,bp` means *“cards which cost at least two Phyrexian-Black Mana”*, for example *Dismember*. Semicolons  separate *Logical Disjunctions* a.k.a. *Set Unions*. e.g. `#bp,bp;gp,gp` means *“cards which cost either: (1) at least two Phyrexian-Black Mana; or (2) at least two Phyrexian-Green Mana”*. a *Prefix Operator*.

* `@` —  Match the *Rules Text* (i.e. *Oracle Text*). e.g. `@draw` means *“cards whose text has the word ‘draw’”*. a *Prefix Operator*. *NOTE* in advanced usage, `@(…)` can surround an arbitrary *Elisp String Expression* (i.e. which evaluate to a `stringp`); including the extremely useful `rx` macro (i.e. `@(rx …)` for a human-readable *Elisp Regular-Expression*).

* `$` — Match *Edition(s)*. e.g. `$tsp` means *“Time Spiral cards”* (that is, *“cards printed in the set/edition Time Spiral”*, not in *Time Spiral block*). a *Prefix Operator*.

* `&` — Match *Rarity*. e.g. `&r` means *“rares”*. `&` is a *Prefix Operator*.

* `**` — Match the *Subtypes*. e.g. `**^go` means *“match cards which have a subtype which starts with ‘Go’”* (including *Goblin*, *Golem*, *Gorgon*, *God*, and *Goat*). e.g. `**^go,n$` means *“match cards which have a subtype which starts with ‘Go’ and ends with ‘n’”* (i.e. *Goblin* and *Gorgon*). `**` is a *Prefix Operator*. (*NOTE:* Subtypes are lexically simple; they're all single words, made up of only letters (besides a few exceptions of a hyphen or apostrophe); thus we can unambiguously parse (for example) the comma in `**^go,n$`.)

* `%%` — Match the *Color Identity*. e.g. `%%u` means *“a card with blue color identity”* (e.g. an *Island*); a.k.a., equivalent, *“a card which can be played in an EDH deck with a monoblue Commander”* (e.g. *Memnarch*). `%%` is a *Prefix Operator*.

* `##` — Match the *Converted Mana Cost*. e.g. `##3` means *“cards with converted mana cost exactly three”*, for example *Dismember*.
e.g. `##<=3` means *“cards with converted mana cost three or less”*. `##` is a *Prefix Operator*; it takes a *Number* (or any *Numerical Expression*). (*NOTE* `##3` is equivalent to `##=3`.)

* `@@` —  Match any *Keyword* in the *Keywords List* (i.e. in the *Rules Text*, but only on their own line, not as part of a complete sentence). e.g. `@@flying` means *“static flyers”, which includes both creatures and noncreatures (like *Vehicles*) which have a line like `Flying` (or `Flying, trample`, or `Defender, flying`), but which includes neither `…: ~ gains flying until end of turn` nor `~ has flying as long as …`. `@@` is a *Prefix Operator*.

* `$$` — Match the *Original Edition*. e.g. `$$an` means *“cards originally printed in Arabian Nights”*, i.e. *“cards introduced by the Arabian Nights edition, and thus which existed since near the start of the game”*. `$$` is a *Prefix Operator*. *Mnemonic*: the *single dollar-sign operator* and the *double dollar-sign operator* both match cards' *Editions*.

* `\`` — Matches cards satisfying a *Predefined Predicate*. *Customize* via `mtg-syntax-satisfaction-alist`. e.g. `` `dfc `` means *“Double-Faced Cards”*. e.g. `` `red `` is equivalent to `` %r ``.

* `~` — Matches cards belonging to a *Predefined Set*. *Customize* via `mtg-syntax-membership-alist`. e.g. `~bounce` means *“Bounce spells/effects”"; e.g. `~bounce` means *“Counterspells / Counterspell effects”".
Meaning: membership in a manually-curated (and possibly subjective) set of cards; unlike other predicates which are automatically-searched against a card's text. For example, `~bounce` selects members in the customizable variable `mtg-bounce-list`, which includes both `Unsummon` and `Commit // Memory` (but not `Sensei's Divining Top`). See `mtg-syntax-named-alist` (which has an entry like `'(bounce . mtg-bounce-list)`). Mnemonic: `~` and `\`` share the same key (`~` is `` Shift+` ``), and both take phrases which are the names of arbitrary predicates).

* `$$$` — Match *Block(s)*. e.g. `$$$tsp` means *“Time Spiral block cards”* (i.e., *“cards printed in the Time Spiral block, including Planar Chaos and Future Sight”*). `$$$` is a *Prefix Operator*. *Mnemonic*: most blocks have three sets; sets are matched by one dollar-sign, blocks are matched by three dollar-signs.

* `%%%` — Match the *Color Personality*: my own custom predicate, which extends *Color Identity* with the *Basic Land Types*. e.g. `%%%u` means *“a card with blue color personality”* (e.g. *Spire Golem*, whose *Color Personality* is *Blue* while its *Color Identity* is *Colorless*; e.g. *Dukhara Peafowl*, whose *Color Identity* is *Blue* too). `%%%` is a *Prefix Operator*. *Mnemonic*: *Color Personality* generalizes *Color Identity*; its operator has an additional percent-sign.

*Regexp Operators*:

* `^` — e.g. `^elves` means *“cards whose name starts with ‘elves’”*, for example *Elves of Deep Shadow*. a *Infix Operator* (but which may look like a *Prefix Operator*).
* `$` — e.g. `elves$` means *“cards whose name ends with ‘elves’”*, for example *Llanowar Elves*. a *Infix Operator* (but which may look like a *Postfix Operator*).

*Logical Operators*:

* `!` — *Logical Negation*. e.g. `%!u *!instant` means *“nonblue non-instants”* (for example, *Birds of Paradise*). `!` is a *Prefix Operator*, can both come before and be within most other *MTQ Expressions* and *MTQ Operators*.
* `|`  — *Logical Disjunction* a.k.a. `OR`. e.g. `%u | *instant` means *“either blue cards or instants”*.
* `&`  — *Logical Conjunction* a.k.a. `AND`. e.g. `%u & *instant` means *“blue instants”*.*NOTE* most whitespace is equivalent to ampersands; for example, `%u & *instant` is equivalent to `%u *instant`.

*Numeric Operators*:

* `>`  — 
* `<`  — 
* `=`  — 
* `>=` — 
* `<=` — 
* `!=` — 

*Query Operators*:

* `.` — the *Selection Operator*. e.g. `.name` means *“show only card names in the results list”*. e.g. `.cmc,name` means *“show only CMCs and card names, in that order”*. `.`  is a *Prefix Operator*, and may be an *Infix Operator*. `.` expects an *Ordered Set* of *Card Fields* and/or *Card Parts*.

*Grouping Characters*:

* `()` — e.g. `()`. anything within parentheses is an arbitrary *S-Expression*. it's how you embed *Elisp* into *MTG Search DSL*. For example, 
* `[]` — e.g. `[]`. 
* `{}` — e.g. `{}`. 
* `"` — e.g. `""`. 

Other operators:

* `\\` — e.g. `\}`. an *Infix Operator*.
* ` ` — e.g. ` `. an *Infix Operator*. Whitespace separates
* `,` — e.g. `,`. an *Infix Operator*.
* `;` — e.g. `;`. an *Infix Operator*.
* `/` — e.g. `/`. an *Infix Operator*.
* `|` — e.g. `|`. an *Infix Operator*.
* `&` — e.g. `&`. an *Infix Operator*.
* `+` — e.g. `+`. an *Infix Operator*.
* `_` — e.g. `_`. an *Infix Operator*.

* `?` — e.g. `?`. a *Suffix Operator*.
* `` — e.g. `$`. a *Suffix Operator*.

__Non__-Operator characters:

* `'` — 
* letters
* digits

Default *MTG Variables* (add your own via `mtg-defvar`):

* `NAME` — 
* `COST` — 
* `TYPES` — 
* `COLORS` — 
* `RULES` — 
* `POWER` — 
* `TOUGHNESS` — 
* `LOYALTY` — 
* `FACES` — 
* `SIDES` — 

* `RARITY` — 
* `EDITION` — 
* `FLAVOR` — 
* `ARTIST` — 

* `CMC` a.k.a. `CONVERTEDMANACOST` — 
* `CARDTYPES` — 
* `SUBTYPES` — 
* `SUPERTYPES` — 
* `CI` a.k.a `COLORIDENTITY` — 

* `` — 
* `` — 
* `` — 

#### Search S-Expressions

For example, the “MTQ Expression” `%u *instant` can be rewritten (more explicitly, but more verbosely) as the “S-Expression” `(and (memq 'blue COLORS) (memq 'instant TYPES))`.

#### Relates

The syntax of *MTQ* was inspired by:

* <magiccards.info> — and <scryfall.com>, websites for searching *MTG* cards. e.g. `o:draw id<=esper t:instant` means *Card-Draw Instants you can play with an Esper Commander".
* `helm-mini` — from the *Helm* Emacs Package.  e.g. `*lisp ^helm @moc` means: only buffers which are in `lisp-mode` (`*lisp`), whose names start with `"helm"` (`^helm`), and whose contents have `"moc"` (`@moc`)*.
* `elfeed` —  from the *Elfeed* Emacs Package. e.g. `@1-year-old +youtube linu[xs] -bsd #15` means *a filter that finds all entries from within the past year (`@1-year-old`) tagged “youtube” (`+youtube`) that mention Linux or Linus (`linu[sx]`), but aren’t tagged “bsd” (`-bsd`), limited to the most recent 15 entries (`#15`)*

### Extensions

`mtg-defvar` defines an *MTG Variable*. 

*MTG Variables* aren't *Elisp Variables*, but are “bound” as *Elisp Variables* within an *MTG Form*. By convention, they are uppercase (to match the default *MTG Variables*, and to distinguish themselves visually from *Elisp Variable*).

`mtg-defvar` is a macro with signature:

``` elisp
(mtg-defvar NAME FORM)
```

For example:

``` elisp
(mtg-defvar SIZE
  (+ POWER TOUGHNESS))

(length
  (mtg-query
    (> CMC SIZE)))
```

### Examples

* the `^ancestral *instant @draw` pattern — narrows to Card-Draw Instants. i.e. cards:

    - whose Card Name starts with *Ancestral*.
    - whose Card Type includes *Instant*.
    - whose Rules Text includes *draw* case-insensitive and between word-boundaries. (e.g. *Draw ...* and *“... draw.”* both match, but *drawn* doesn't match).

* the `*elf,druid 1/ elf|elves` pattern — narrows to, for example, `Llanowar Elves` and `Elves of Deep Shadow`. i.e. cards:

   - whose Card Name includes either the word *Elf* (singular) or *Elves* (plural), or both.
   - whose Card Type includes both *Elf* and *Druid*.
   - whose Power is exactly `1` (and whose Toughness can be anything.)

### Tips

For names or text with whitespace, to group them under a single operator (i.e. as one “word”) either:

* capitalize each word, strip out commas, then squish them together.

    - e.g. `Advent of the Wurm` becomes `AdventOfTheWurm` (*NOTE* `of` and `the` become capitalized.). 
    - e.g. `first strike` becomes `FirstStrike` (*NOTE* or even just `firststrike`, since by default, all keywords can be recognized by single-word aliases.)

* surround with quotations:

    - e.g. `"first strike"`.

### `mtg-helm`

## Porting Scryfall Queries

with has:indicator.

c:rg
%rg
Cards that are red and green

color>=uw -c:red
color>=uw -c:red
Cards that are at least white and blue, but not red

id<=esper t:instant
id<=esper t:instant
Instants you can play with an Esper commander

id:c t:land
%%c *land
Land cards with colorless identity



## Notes

>



























# `mtg.el`

## `mtg-mode.el`

`mtg-mode` is a *Major Mode* for rendering and editing *Magic: the Gathering* cards.

### Features

* Prettify symbols — e.g. `{2}{U}` as *②💧* (via `prettify-symbols-mode`).
* Docs (via `eldoc-mode`).

### Prettify Symbols

Render symbols either:

* as Unicode, for portability — e.g. `{2}{U}` as *②💧*.
* as Images, for prettiness — e.g. `{2}{U}` as <span><abbr class="mtg-two-generic-mana">{2}</abbr><abbr class="mtg-blue-mana">{U}</abbr></span>

Customize `mtg-prettify-symbols-style`:

* `nil` means — no prettification (display text between curly-braces literally).
* `'characters` means — display as *Unicode Characters*.
* `'images` means — display as *SVG Images*.

Customize `mtg-prettify-symbols-*-alist` to:

* Replace the default images/characters.
* Register your own *Symbols* and how to display them.

e.g. Register a custom symbol and its display character:

``` elisp
(add-to-list 'mtg-prettify-symbols-characters-alist '("U/H" . ?))
```

e.g. Register a custom symbol and its display image:

``` elisp
(add-to-list 'mtg-prettify-symbols-images-alist '("U/H" . ""))
```

## `mtg-company.el`

`mtg-company` is a *Company Backend* for completing *Magic: the Gathering* card names.

## Files

### Release Files

The tarball (`mtg.tar.gz`) includes these files:

* `./lisp/mtg-*.el[.gz]` — the *Code*.

* `./json/*.json[.gz]` — the *Data*.

e.g. Re-Install `Vintage.json.gz`:

``` elisp
$ (cd ./json && wget https://mtgjson.com/json/Vintage.json.gz)
```

e.g. Check the size of `Vintage.json.gz`:

``` elisp
$ du -h ./json

  45M ./json/Vintage.json.gz
```

*NOTE* `Vintage.json.gz` is version-controlled & distributed, `Vintage.json` isn't:

``` elisp
$ mkdir ./tmp && cd ./tmp && cp ../json/Vintage.json.gz . && uncompress ./tmp/*.json.gz)

$ du -h ./tmp

  261M ./tmp/Vintage.json.gz
```

`./json/Vintage-card-names.json.gz` is generated via `jq`: 

``` elisp
$ cat ./json/Vintage.json.gz | gzip -d | jq '[.[].cards[].name | strings] | unique' | gzip -c > ./json/Vintage-card-names.json.gz
```

### Development Files

*Build Files* include:

* `Makefile`
* `Cask`

## Links

* <https://github.com/sboosali/mtg.el>
* <https://mtgjson.com/downloads/compiled/>

## Notes

> 


















# `mtg-mode.el`

`mtg-mode` is a *Major Mode* for rendering and editing Magic the Gathering cards.

## Features

* Prettify symbols — e.g. `{2}{U}` as *②💧* (via `prettify-symbols-mode`).
* Completion — complete lists (e.g. of card names, creature types, keywords, etc) conveniently (via `completing-read` and `completion-at-point`).
* Docs — echo information (e.g. its cost, color, card type, power/toughness, etc) about the *card name at point* (via `eldoc-mode`).

## MTG Mode

## Font Lock



## Prettify Symbols

Render symbols either:

* as Unicode, for portability — e.g. `{2}{U}` as *②💧*.
* as Images, for prettiness — e.g. `{2}{U}` as <span><abbr class="mtg-two-generic-mana">{2}</abbr><abbr class="mtg-blue-mana">{U}</abbr></span>

Customize `mtg-prettify-symbols-style`:

* `nil` means — no prettification (display text between curly-braces literally).
* `'characters` means — display as *Unicode Characters*.
* `'images` means — display as *SVG Images*.

Customize `mtg-prettify-symbols-*-alist` to:

* Replace the default images/characters.
* Register your own *Symbols* and how to display them.

e.g. Register a custom symbol and its display character:

``` elisp
(add-to-list 'mtg-prettify-symbols-characters-alist '("U/H" . ?))
```

e.g. Register a custom symbol and its display image:

``` elisp
(add-to-list 'mtg-prettify-symbols-images-alist '("U/H" . ""))
```

## Notes

>




