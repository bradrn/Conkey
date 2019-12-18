# Conkey Reference

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Keys](#keys)
  - [Key syntax](#key-syntax)
  - [Dead keys](#dead-keys)
- [Diacritics](#diacritics)
  - [Precomposed Characters](#precomposed-characters)
- [Combining diacritics](#combining-diacritics)
- [Modifier letters](#modifier-letters)
- [Hooks and bars](#hooks-and-bars)
- [Letters](#letters)
  - [Misc. uncommon letters (including Zhuang tones)](#misc-uncommon-letters-including-zhuang-tones)
  - [Superscripts](#superscripts)
  - [Greek letters](#greek-letters)
  - [Clicks](#clicks)
  - [Using unusual letters with diacritics](#using-unusual-letters-with-diacritics)
- [Typographical marks](#typographical-marks)
  - [Quotation marks and guillemets](#quotation-marks-and-guillemets)
  - [Dashes](#dashes)
  - [Spaces](#spaces)
  - [Dots](#dots)
  - [IP Marks](#ip-marks)
  - [Currencies](#currencies)
  - [Arrows](#arrows)
  - [Mathematics and related symbols](#mathematics-and-related-symbols)
  - [Other symbols](#other-symbols)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Keys

### Key syntax

This Reference will use the following syntax to represent keys:

- `<glyph>` indicates that the key corresponding to `<glyph>` should be pressed.
- `SPC` indicates that the space bar should be pressed.
- `S-<glyph>` indicates that the key corresponding to `<glyph>` should be pressed at the same time as the `Shift` key.
- `G-<glyph>` indicates that the key corresponding to `<glyph>` should be pressed at the same time as the `AltGr` key (right Alt key).
- `G-S-<glyph>` indicates that the key corresponding to `<glyph>`, the `AltGr` key, and the `Shift` key should be pressed at the same time.
- `<keys1> <keys2>` indicates that `<keys1>` should be pressed and released, then `<keys2>` should be pressed and released.

For instance, `G-' a` means: “press `AltGr` and the `'` key at the same time, then release them, then type `a`”.

Keys will be described using their position on the US keyboard layout.
For reference, the US keyboard layout looks like this:

![US keyboard layout](https://upload.wikimedia.org/wikipedia/commons/5/51/KB_United_States-NoAltGr.svg)
(From https://commons.wikimedia.org/wiki/File:KB_United_States-NoAltGr.svg)

Additionally, we will often reference a shifted key using either of its two glyphs;
  for instance, `G-S-'` is the same as `G-S-"`.
Usually, to avoid confusion, the latter representation will be placed in brackets next to the former;
  for instance, the `<` character will always be represented as ‘`S-,` (`S-<`)’.
This is done because occasionally the shifted version is useful as a mnemonic,
  but traditionally is left unlisted when keyboard shortcuts are described.

(Note that this is just a slightly extended version of Emacs key syntax.)

### Dead keys

The majority of special characters are accessed from so-called ‘dead keys’:
  that is, keys which do nothing by themselves (i.e. they are ‘dead’), but output a modified character when another key is typed after it.
For instance, the acute accent is accessed by `G-'`;
  by itself this will do nothing, but if you write (say) `G-' a`, that results in the character `á`.

## Diacritics

### Precomposed Characters

Many diacritics can be accessed via dead keys to type so-called _precomposed characters_.
These are letter+diacritic combinations stored as only one character.
Examples include `ášįṗṟ`.
Note that Unicode does not include precomposed characters for all letter+diacritic combinations,
  so _combining characters_ are also supplied;
  these are diacritics which, when typed, display over or under the previous character.

The following table contains a list of diacritics available via a dead key.
The columns have the following meanings:

- **Key**: The key which should be typed to trigger the dead key.
- **Diacritic**: The diacritic triggered by the dead key, with an example of the diacritic on the placeholder character ◌.
- **Characters available**: The letters which can be typed after the dead key.
  For instance, if `a` is listed as available with the dead key G-\`, you can type ‘G-\` a’ to get the character ‘à’.
  Note that due to the limited supply of precomposed characters, not all characters are available
  e.g. there is no precomposed character for ‘a̋’.
- **Uncombined character**: When a character not listed in ‘characters available’ is typed,
  the ‘uncombined character’ is generated instead, along with the character typed.
  For instance, typing `G-; a` results in the character ‘U+02dd’ followed by the character ‘a’.
  For diacritics, the uncombined character is either a spacing modifier letter or a combining diacritic;
  the exact codepoint is thus given to avoid any misunderstandings.
  You should not rely on the uncombined character being a certain value; it is simply listed here for documentation purposes.

| Key            | Diacritic             | Characters available                       | Uncombined character |
|----------------|-----------------------|--------------------------------------------|----------------------|
| G-\`           | ◌̀ Grave accent        | `aAeEiInNoOuUwWyY`                         | U+02cb               |
| G-'            | ◌́ Acute accent        | `aAcCeEgGiIkKlLmMnNoOpPrRsSuUwWyYzZæÆøØ`   | U+02ca               |
| G-6            | ◌̂ Circumflex          | `aAcCeEgGhHiIjJoOsSuUwWyYzZ`               | U+02c6               |
| G-S-\` (G-S-~) | ◌̃ Tilde               | `aAeEiInNoOuUvVyY`                         | U+02dc               |
| G--            | ◌̄ Macron              | `aAeEgGiIoOuUyYæÆ`                         | U+02c9               |
| G-9            | ◌̆ Breve               | `aAeEgGiIoOuU`                             | U+02d8               |
| G-S-. (G-S->)  | ◌̇ Dot above           | `aAbBcCdDeEfFgGhHImMnNoOpPrRsStTwWxXyYzZ1` | U+02d9               |
| G-S-' (G-S-")  | ◌̈ Diaeresis/umlaut    | `aAeEhHiIoOtuUwWxXyY`                      | U+00a8               |
| G-0            | ◌̉ Hook above          | `aAeEiIoOuUyY`                             | U+0309               |
| G-8            | ◌̊ Ring above          | `aAuUwy`                                   | U+02da               |
| G-;            | ◌̋ Double acute accent | `oOuU`                                     | U+02dd               |
| G-5            | ◌̌ Caron               | `aAcCdDeEgGhHiIjJkKlLnNoOrRsStTuUzZʒƷ`     | U+02c7               |
| G-S-0 (G-S-))  | ◌̛ Horn                | `oOuU`                                     | U+031b               |
| G-.            | ◌̣ Dot below           | `aAbBdDeEhHiIkKlLmMnNoOrRsStTuUvVwWyYzZ`   | U+0323               |
| G-,            | ◌̦ Comma below         | `sStT`                                     | U+0326               |
| G-S-, (G-S-<)  | ◌̧ Cedilla             | `cCdDeEgGhHkKlLnNrRsStT` (see note 1)      | U+00b8               |
| G-\[           | ◌̨ Ogonek              | `aAeEiIoOuU`                               | U+02db               |
| G-7            | ◌̭ Circumflex below    | `dDeElLnNtTuU`                             | U+032d               |
| G-S-- (G-S-\_) | ◌̱ Macron below        | `bBdDhkKlLnNrRtTzZ`                        | U+02cd               |

> Note 1: Many fonts render some of these characters with commas below instead of cedillas.
> For more information visit (The Wikipedia page on Cedilla#Latvian)[https://en.wikipedia.org/wiki/Cedilla#Latvian],
>   and the following section on Marshallese.

In addition to the listed characters, a space can be typed after any of the dead keys above
  to yield the corresponding combining diacritic.
This can be used to yield further combinations of letter+diacritic.
For instance, to type ‘a̋’, the combination `G-; a` cannot be used, as from the table above `a` cannot be used with `G-;`.
However, this character can instead be generated by first typing `a`, and then typing `G-; SPC` to get the combining double acute accent.

## Combining diacritics

Certain diacritics can be accessed only as combining characters, as they do not have any precomposed characters associated with them.
These diacritics can be accessed via the `G-m` dead key:

| Character to type after dead key | Resulting diacritic                    |
|----------------------------------|----------------------------------------|
| Vertical bar/pipe (see note 2)   | ◌̍ Combining vertical line above        |
| \                                | ◌̩ Combining vertical line below        |
| "                                | ◌̎ Combining double vertical line above |
| ,                                | ◌̓ Combining comma above                |
| S-, (S-<)                        | ◌̒ Combining turned comma above         |
| .                                | ◌̔ Combining reversed comma above       |
| S-. (S->)                        | ◌̕ Combining comma above right          |
| S-8 (S-\*)                       | ◌͘ Combining dot above right            |
| \`                               | ◌̖ Combining grave accent below         |
| S-\` (S-~)                       | ◌̗ Combining acute accent below         |

> Note 2: That is, the `|` character. But GitHub can’t render that properly in a table.

For instance, typing `G-m |` gives the combining vertical line above.

## Modifier letters

Several modifier letters can also be accessed via the `G-m` dead key:

| Character to type after dead key | Resulting diacritic              |
|----------------------------------|----------------------------------|
| '                                | ◌ʼ Modifier letter apostrophe    |
| p                                | ◌ʹ Modifier letter prime         |
| ]                                | ◌ˈ Modifier letter vertical line |

(Some additional modifier letters can be found in the section on [superscripts](#superscripts).)

## Hooks and bars

Many of the more unusual Latin-script letters are derived from other letters with the addition of a bar or hook.
Although these are not strictly diacritcs, the Conkey keyboard treats them the same way.

The following hooked letters can be generated using the `G-S-[` (`G-S-{`) dead key:
```
Letter to type:    bBdDfFkKnNtTþÞvVyYƌƋ
Resulting letter:  ɓƁɖƉƒƑƙƘɲƝƭƬʈƮʋƲƴƳɗƊ
```
For instance, typing `G-S-[ V` results in ‘Ʋ’.

The following barred letters can be generated using the `G-\` dead key:
```
Letter to type:    aAbBcCdDeEgGhHiIjJlLoOpPrRtTuUyYzZλ
Resulting letter:  ⱥȺƀɃȼȻðÐɇɆ̵ǥǤħĦɨƗɉɈłŁøØᵽⱣɍɌŧŦʉɄɏŦƶƵƛ
```
(Note that `kKqQþÞ` also work, but have been omitted from the diagram because many fonts cannot render their resulting letters.)

An additional set of barred letters can be generated using the `G-S-\` (`G-S-|`) dead key:
```
Letter to type:    dDjlLoO
Resulting letter:  đĐɟ̶ƚȽɵƟ
```
(Again, `pPþÞ` also work.)

Additionally, a number of combining diacritics can be generated as well:

| Key combination | Diacritic |
|-----------------|-----------|
| `C-\ SPC`       | U+0335    |
| `C-S-\ SPC`     | U+0336    |
| `C-\ \`         | U+0337    |
| `C-\ \`         | U+0338    |

Also, as mentioned above, each dead key has a corresponding uncombined character:

| Key     | Uncombined character |
|---------|----------------------|
| `C-\`   | U+0335               |
| `C-S-\` | U+0336               |


## Letters

Conkey includes many non-English letters.
These letters may be accessed as follows:

| Key combination | Resulting letter             | Capital? |
|-----------------|------------------------------|----------|
| `G-a a`         | ɑ (Latin letter alpha)       | Yes      |
| `G-a e`         | æ                            | Yes      |
| `G-c`           | ꞌ (Saltillo)                 | Yes      |
| `G-e`           | ə (Latin letter schwa)       | Yes      |
| `G-3`           | ɛ                            | Yes      |
| `G-g`           | ɣ                            | Yes      |
| `G-h`           | ꜧ                            | Yes      |
| `G-i`           | ı (Dotless i)                | No       |
| `G-j`           | Greek letters (see below)    | N/A      |
| `G-k`           | ɔ                            | Yes      |
| `G-l`           | Misc. letters (see below)    | Yes      |
| `G-n`           | ŋ                            | Yes      |
| `G-b`           | ꞑ                            | Yes      |
| `G-o e`         | œ                            | Yes      |
| `G-o o`         | ꝏ                            | Yes      |
| `G-o u`         | ʊ (Latin letter upsilon)     | Yes      |
| `G-o w`         | ꞷ (Latin letter omega)       | Yes      |
| `G-p`           | Clicks (see below)           | No       |
| `G-q`           | ɂ (Non-unicase glottal stop) | Yes      |
| `G-s f`         | ſ                            | No       |
| `G-s s`         | ß                            | Yes      |
| `G-s h`         | ʃ                            | Yes      |
| `G-t`           | þ                            | Yes      |
| `G-u`           | ɯ                            | Yes      |
| `G-x`           | ʔ (Unicase glottal stop)     | No       |
| `G-X`           | ʻ (Hawaiian ʻokina)          | No       |
| `G-y`           | ǝ (Latin letter turned E)    | Yes      |
| `G-z`           | ʒ                            | Yes      |
| `G-1 1`         | ɩ (Latin letter iota)        | Yes      |
| `G-1 <number>`  | Zhuang tone letters          | Yes      |

(Note that the names of some of the more unusual letters are included to avoid confusion.)

Notice that a column of the above table is entitled ‘Capital?’.
This refers to the method of typing the corresponding uppercase letter:

- If ‘Capital?’ is **Yes**, then that letter has a corresponding uppercase letter.
  If the lowercase character is typed by doing `G-<lowercaseletter>`,
  then the corresponding uppercase can be typed by `G-<uppercaseletter>`.
  On the other hand, if the lowercase character is typed by doing `G-<key1> <lowercaseletter>`,
  then the corresponding uppercase can be typed by `G-<key1> <uppercaseletter>`.
  For instance, the uppercase of `G-n` ‘ŋ’ is `G-N` ‘Ŋ’,
  and the uppercase of `G-a e` ‘æ’ is `G-a E` ‘Æ’ (_not_ `G-A e` or `G-A E`).
- If `Capital?` is **No**, then that character has no corresponding uppercase letter.

### Misc. uncommon letters (including Zhuang tones)

Various uncommon letters can be accessed via the `G-l` dead key:
```
Letter to type:    bBdDgGhHjJknNoOqQvVwWyYzZ3#4$5%
Resulting letter:  ƃƂƌƋƣƢƕǶḫḪĸƞȠȣȢɋɊʌɅƿǷьЬȝȜꜫꜪꜭꜬꜯꜮ
```
(Note that `aA` also work, resulting in lower- and uppercase Egyptological Alef respectively. Not all fonts support these though.
Note also that the result of typing `zZ` is yogh ȝȜ rather than ezh ʒƷ — they look identical in some fonts.)

Zhuang tone letters can be accessed as well via the `G-1` dead key:
```
Letter to type:    123456!@#$%^
Resulting letter:  ɩƨзчƽƅƖƧЗЧƼƄ
```
(Strictly speaking, the first letter is a Latin iota, not used in Zhuang, but due to the visual resemblance it is placed under the same dead key.)

### Superscripts

Many superscript letters may be accessed via the `G-w` dead key:
```
Letter to type:    ABDEGHIJKLMNOPRTUVWabcdefghijklmnoprstuvwxyz1234567890+-
Resulting letter:  ᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁⱽᵂᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ¹²³⁴⁵⁶⁷⁸⁹⁰⁺⁻
```
A superscript ‘ᶿ’ may also be obtained by typing `G-w G-j`.



### Greek letters

Selected Greek letters can be accessed via the `G-j` dead key:
```
Letter to type:    abgGdDtlmprsf
Resulting letter:  αβγΓδΔθλμπρσφ
```

### Clicks

Click letters can be accessed via the `G-p` dead key:
```
Letter to type:    @|/c!q=vx
Resulting letter:  ʘǀǀǀǃǃǂǂǁ
```

### Using unusual letters with diacritics

Certain diacritics can accept unusual letters.
These letters can be accessed through the following keys:

| Key combination | Character                  |
|-----------------|----------------------------|
| `G-a`           | æ                          |
| `G-A`           | Æ                          |
| `G-o`           | ø                          |
| `G-O`           | Ø                          |
| `G-l`           | ƌ (note this is lowercase) |
| `G-L`           | Ƌ (note this is uppercase) |
| `G-t`           | þ                          |
| `G-T`           | Þ                          |
| `G-j`           | λ                          |

For instance, to type ‘ǽ’, use `G-' G-a`; to type ‘Ɗ’, use `G-S-[ G-L`.


## Typographical marks

Conkey allows many typographical marks to be typed easily.

Note that typographical marks will often use the same dead keys as diacritics;
  when used with letters they produce accented letters, whereas when used with punctuation they produce typographical marks.

### Quotation marks and guillemets

Single quotation marks/guillemets can be typeset via the `G-'` dead key:
```
Letter to type:    <>(),.
Resulting symbol:  ‹›‘’‚‛
```
Double quotation marks/guillemets can be typeset via the `G-S-'` (`G-S-"`) dead key:
```
Letter to type:    <>(),.
Resulting symbol:  «»“”„‟
```

Although they are not strictly guillemets, Conkey can also produce the chevrons ‘⟨’ and ‘⟩’.
Use `G-S-, G-S-,` (`G-S-< G-S-<`) for the left chevron, and `G-S-. G-S-.` (`G-S-> G-S->`) for the right chevron.

### Dashes

A variety of dashes may be produced using the `G--` dead key:

| Letter to type | Dash produced |
|----------------|---------------|
| `=`            | Minus sign    |
| `-`            | Hyphen        |
| `n`            | En dash       |
| `m`            | Em dash       |

### Spaces

Various different types of space may be produced using the `G-SPC` dead key (i.e. press the space bar and AltGr together):

| Letter to type   | Space produced                                          | Code point |
|------------------|---------------------------------------------------------|------------|
| `t`              | Tab                                                     | U+0009     |
| ` ` (i.e. space) | Non-breaking space                                      | U+00a0     |
| `n`              | En space                                                | U+2002     |
| `m`              | Em space                                                | U+2003     |
| `b`              | Three-per-em space                                      | U+2004     |
| `v`              | Four-per-em space                                       | U+2005     |
| `c`              | Six-per-em space                                        | U+2006     |
| `1`              | Figure space                                            | U+2007     |
| `.`              | Punctuation space                                       | U+2008     |
| `,`              | Thin space                                              | U+2009     |
| Vertical bar     | Hair space                                              | U+200a     |
| `+`              | Medium mathematical space                               | U+205f     |
| `!`              | Narrow no-break space                                   | U+202f     |
| `0`              | Zero width space                                        | U+200b     |
| `(`              | Zero width non-joiner                                   | U+200c     |
| `)`              | Zero width joiner                                       | U+200d     |
| `[`              | Open box `␣` (symbol for space)                         | U+2423     |
| `]`              | Shouldered open box `⍽` (symbol for non-breaking space) | U+237d     |

### Dots

Various dot-like characters may be produced using the `G-.` dead key:

| Letter to type | Dot produced          |
|----------------|-----------------------|
| `.`            | Ellipsis              |
| Vertical bar   | Interpunct/middle dot |
| `)`            | Bullet                |
| `0`            | Degree sign           |

### IP Marks

Marks designating Intellectual Property can be produced via the `G-S-p` (`G-S-P`) dead key:
```
Letter to type:    cprts
Resulting symbol:  ©℗®™℠
```

### Currencies

Selected currencies may be produced via the `G-4` dead key:

```
Letter to type:    oCcersl
Resulting symbol:  ¤¢c€₹₪£
```

### Arrows

A wide range of arrows may be produced via the `G-/` dead key:

| Character to type | Resulting arrow |
|-|-|
| `w` | ↑ |
| `a` | ← |
| `s` | ↓ |
| `d` | → |
| `z` | ↔ |
| `x` | ↕ |
| `c` | ⇐ |
| `v` | ⇒ |
| `e` | ⇈ |
| `r` | ⇉ |
| `f` | ⇊ |
| `b` | ⇋ |
| `n` | ⇌ |
| `m` | ↦ |
| `t` | ⤚ |
| `y` | ⤙ |
| `g` | ⤜ |
| `h` | ⤛ |
| `u` | ↣ |
| `i` | ↢ |


### Mathematics and related symbols

Many mathematical symbols, as well as symbols relating to the Haskell language, and blackboard bold letters,
  can be produced via the `G-=` dead key:
  
| Character to type | Resulting symbol |
|-|-|
| `x` | × |
| `/` | ÷ |
| `%` | ‰ |
| `-` | ± |
| `1` | ′ (prime) |
| `2` | ″ (double prime) |
| `3` | ‴ (triple prime) |
| `s` | √ |
| `0` | ∅ (empty set) |
| `~` | ¬ |
| `:` | ∷ |
| `*` | ★ |
| `A` | ∀ |
| `(` | ⦇ |
| `)` | ⦈ |
| `[` | ⟦ |
| `]` | ⟧ |
| `f` | ≫ |
| `d` | ≪ |
| `@` | ⊛ |
| `h` | ⋙ |
| `g` | ⋘ |
| `8` | ⁂ |
| `#` | ⧻ |
| `|` | ⫴ |
| `.` | ∘ |
| `^` | ∧ |
| `v` | ∨ |
| `=` | ≡ |
| `!` | ≠ |
| `¡` | ≢ |
| `<` | ≤ |
| `>` | ≥ |
| `'` | ⋅ |
| `+` | ⧺ |
| `e` | ∈ |
| `E` | ∉ |
| `i` | ‼ |
| `b` | ⊥ |
| `U` | ∪ |
| `I` | ∩ |
| `a` | ⊕ |
| `\` | ∖ (set difference) |
| `{` | ⊆ |
| `}` | ⊇ |
| `C` | ℂ |
| `N` | ℕ |
| `Q` | ℚ |
| `R` | ℝ |
| `Z` | ℤ |

(Note: in previous versions of Conkey, these were collectively produced by the `G-v`, `G-2` and `G-]` dead keys.
For Conkey v2.0 those dead keys were merged.)

### Other symbols

| Key combination       | Symbol produced           |
|-----------------------|---------------------------|
| `G-S-1` (`G-S-!`)     | ¡                         |
| `G-S-/` (`G-S-?`)     | ¿                         |
| `G-o .`               | ◌ (Diacritic carrier)     |
| `M-S-; :` (`M-S-: :`) | ꞉ (Modifier letter colon) |
| `M-S-; ;` (`M-S-: ;`) | ː (IPA triangular colon)  |
| `M-s p`               | ¶                         |
| (see note 3 below)    | §                         |
| `G-S-2 ?` (`G-S-@` ?) | ‽                         |
| `G-S-2 !` (`G-S-@` !) | ‽                         |
| `G-S-2 N` (`G-S-@` N) | №                         |
| `G-S-2 1` (`G-S-@` 1) | †                         |
| `G-S-2 2` (`G-S-@` 2) | ‡                         |

> Note 3: This should be `M-s |`, but as mentioned previously GitHub can’t render the vertical bar in a table.

