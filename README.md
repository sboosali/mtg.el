# `mtg-mode.el`

`mtg-mode` is a *Major Mode* for rendering and editing Magic the Gathering cards.

## Features

* Prettify symbols — e.g. `{2}{U}` as *②💧* (via `prettify-symbols-mode`).
* Docs (via `eldoc-mode`).

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

## Links

* <>
* <>
* <>

## Notes

> 






# `mtg-mode.el`

`mtg-mode` is a *Major Mode* for rendering and editing Magic the Gathering cards.

## Links

* <https://github.com/sboosali/mtg.el>
* <>
* <>

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




