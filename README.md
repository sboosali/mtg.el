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




