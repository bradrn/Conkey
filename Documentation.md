# Guide to Conkey

## How to use this guide

### Key notation

This guide represents key combinations using the following conventions ([based on Emacs key notation](https://www.gnu.org/software/emacs/manual/html_node/emacs/User-Input.html)):

- A single letter or symbol such as `l` or `-` means ‘press that key’.
  A special case is `SPC`, which means ‘press the Space bar’.
- An uppercase letter, or a symbol which is entered using the Shift key, means ‘press the appropriate key while holding Shift’.
  For instance, `A` means ‘hold Shift while pressing `a`’, and `!` means ‘hold Shift while pressing `1`’.
- `S-key` means ‘press Shift at the same time as `key`’.
- `G-key` means ‘press AltGr at the same time as `key`’
  (where AltGr is the right Alt key on Windows and Linux, and the Option key on Mac OSX).
- `key1 key2` means ‘press `key1`, then release it, then press `key2`’.

For instance, `G-_ r` means ‘press AltGr+shift+`-` at the same time, then release them, then press `r`’.
This key combination results in ‘ṟ’ when used with Conkey.

Note that Conkey is designed around the US keyboard layout, rather than the UK keyboard layout:
  e.g. `\` refers to the key next to `]`, rather than the key next to `z`.
For reference, the US keyboard layout looks like this:

![US keyboard layout](https://upload.wikimedia.org/wikipedia/commons/5/51/KB_United_States-NoAltGr.svg)
(From https://commons.wikimedia.org/wiki/File:KB_United_States-NoAltGr.svg)


### Dead keys and key sequences

A *dead key* is a key which produces no result, but instead modifies the following keystroke, usually by adding a diacritic of some sort.
For instance, the dead key `G-'` adds an acute accent to the following letter,
  so typing something like `G-' a` will generate the letter ‘á’.

In addition to dead keys, Conkey makes extensive use of other key sequences to input letters and punctuation.
For instance, the left quote ‘‘’ is produced by the key sequence `G-' (`, while the letter ‘ƣ’ is produced by the key sequence `G-f g`.
Most key sequences have some kind of mnemonic role: for instance, all key sequences starting with `G-p` produce click letters.

When using key sequences to input letters, one important detail to be aware of is capitalisation.
To capitalise a letter produced by a key sequence, the Shift key should be held down during the last key in the sequence only.
For instance, the lowercase letter ‘ƣ’ is produced by the key sequence `G-f g`,
  while the capital letter ‘Ƣ’ is produced by the key sequence `G-f G` (rather than, say, `G-F g` or `G-F G`).
Similarly, ‘ɖ’ is produced by `G-{ G-d`, while its capitalised counterpart ‘Ɖ’ is produced by `G-{ G-D`.
(However, not all letters have an uppercase form; see below for details.)

Note that dead keys only work before a restricted set of keys.
Other combinations of keys might result in unexpected characters being entered; if this occurs just delete the incorrect output.
The same applies to unsupported key sequences.

### Formatting conventions

Outside tables, the following formatting conventions are used:

- `x` (in a monospace font) refers to a key which you need to press (using the key notation described [above](#key-notation)).
- ‘x’ (in quotes) refers to a letter or symbol which is produced by Conkey.

This guide makes extensive use of tables to list the various key sequences of Conkey.
These tables will look something like this:

| Input   | Result | Uppercase? |
|:--------|:-------|:-----------|
| `G-a`   | æ      | ✔          |
| `G-d i` | ɩ      | ✔          |
| `G-d k` | ĸ      | ✘          |

The first column, marked ‘Input’, shows a key or key sequence,
  which when pressed will produce the letter shown in the second ‘Result’ column.
For instance, pressing `G-a` results in ‘æ’.
For letters (as opposed to punctuation and other symbols), the third ‘Uppercase?’ column contains ✔ if that letter has a corresponding uppercase form,
  and ✘ if no such uppercase counterpart exists.
For instance, `G-A` results in uppercase ‘Æ’, and `G-d I` results in uppercase ‘Ɩ’, but pressing `G-d K` does not give any sensible output.
(Recall that uppercase letters are entered by holding the Shift key during the last key of the sequence.)
The ‘Uppercase?’ column is omitted for tables which list symbols rather than letters, as symbols generally lack uppercases.

## Keyboard layout

### Diacritics

#### Diacritics with precomposed characters

Conkey uses a number of dead keys to enter a variety of diacritics,
  resulting in Unicode [precomposed characters](https://en.wikipedia.org/wiki/Precomposed_character).
These dead keys are enumerated in the following table:

| Diacritic          | Dead key  | Acceptable inputs                                 | Notes                                                                               |
|:-------------------|:----------|:--------------------------------------------------|:------------------------------------------------------------------------------------|
| Acute ◌́            | `G-'`     | `acegiklmnoprsuwyz` <br> `ACEGIKLMNOPRSUWYZ`      | `G-' G-a`, `G-' G-A`, `G-' G-o`, `G-' G-O` produce ‘ǽ’, ‘Ǽ’, ‘ǿ’, ‘Ǿ’ respectively. |
| Diaeresis/umlaut ◌̈ | `G-"`     | `aehiotuwxy` <br> `AEHIOUWXY`                     |                                                                                     |
| Diaeresis below ◌̤  | `G-3`     | `u` `U`                                           |                                                                                     |
| Double acute ◌̋     | `G-:`     | `ou` `OU`                                         |                                                                                     |
| Double grave ◌̏     | `G-;`     | `aeioru` `AEIORU`                                 |                                                                                     |
| Grave ◌̀            | `` G-` `` | `aeinouwy` <br> `AEINOUWY`                        |                                                                                     |
| Tilde ◌̃            | `G-~`     | `aeinouvy` <br> `AEINOUVY`                        |                                                                                     |
| Tilde below ◌̰      | `G-#`     | `eiu` `EIU`                                       |                                                                                     |
| Caron ◌̌            | `G-5`     | `acdeghijklnorstuz` <br> `ACDEGHIKLNORSTUZ`       | `G-5 G-z` and `G-5 G-Z` produce ‘ǯ’ and ‘Ǯ’ respectively.                           |
| Circumflex ◌̂       | `G-6`     | `aceghijosuwyz` <br> `ACEGHIJOSUWYZ`              |                                                                                     |
| Circumflex below ◌̭ | `G-7`     | `delntu` `DELNTU`                                 |                                                                                     |
| Ring above ◌̊       | `G-8`     | `auwy` `AU`                                       |                                                                                     |
| Breve ◌̆            | `G-9`     | `aegiou` `AEGIOU`                                 |                                                                                     |
| Hook above ◌̉       | `G-0`     | `aeiouy` `AEIOUY`                                 |                                                                                     |
| Horn ◌̛             | `G-)`     | `ou` `OU`                                         |                                                                                     |
| Macron ◌̄           | `G--`     | `aegiouy` `AEGIOUY`                               | `G-- G-a` and `G-- G-A` produce ‘ǣ’ and ‘Ǣ’ respectively.                           |
| Line below ◌̱       | `G-_`     | `bdhklnrtz` <br> `BDKLNRTZ`                       |                                                                                     |
| Dot below ◌̣        | `G-.`     | `abdehiklmnorstuvwyz` <br> `ABDEHIKLMNORSTUVWYZ`  |                                                                                     |
| Dot above ◌̇        | `G-S-.`   | `abcdefghmnoprstwxyz` <br> `ABCDEFGHIMNOPRSTWXYZ` |                                                                                     |
| Ogonek ◌̨           | `G-[`     | `aeiou` `AEIOU`                                   |                                                                                     |
| Comma below ◌̦      | `G-,`     | `st` `ST`                                         |                                                                                     |
| Cedilla ◌̧          | `G-S-,`   | `cdeghklnrst` <br> `CDEGHKLNRST`                  | Some fonts will display these letters (particularly ‘ģ’, ‘ķ’, ‘ļ’, ‘ņ’, ‘ŗ’) with a comma rather than a cedilla. See [Wikipedia](https://en.wikipedia.org/wiki/Cedilla#Latvian) for more information.                              |

Each column of the table contains information about the dead key on that row:

- ‘Diacritic’ shows the name of the diacritic and a representation of the diacritic itself.

- ‘Dead key’ shows the location of the dead key on the keyboard.

- ‘Acceptable inputs’ lists the letters which can follow this dead key.
  For clarity, these are separated into lower- and uppercase letters (sometimes on two lines).

- ‘Notes’ contains any other information which may be relevant to that dead key.

For instance, the row for the ‘ring above’ diacritic states that this diacritic is accessed via `G-8`,
and that the available combinations are `G-8 a`, `G-8 u`, `G-8 w`, `G-8 y`, `G-8 A` and `G-8 U`
(which produce ‘å’, ‘ů’, ‘ẘ’, ‘ẙ’, ‘Å’, ‘Ů’ respectively).

Additionally, every one of the above dead keys can be used to insert the corresponding [combining diacritic](https://en.wikipedia.org/wiki/Combining_character),
  by pressing the Space bar immediately after the appropriate dead key.
Combining diacritics are useful when Unicode does not include a precomposed base+diacritic combination;
  for instance, although ‘a̋’ is not present as an independent letter,
  it may still be entered by first typing `a`, and then typing `G-: SPC` to produce the combining double acute accent.

#### Combining diacritics

In addition to the combining diacritics listed above, Unicode includes many combining diacritics which do not correspond to precomposed characters.
These can be entered using key sequences starting with `G-]`, as shown in the following table:

| Input         | Result                        |
|:--------------|:------------------------------|
| `G-] \|`      | Vertical line above ◌̍         |
| `G-] \`       | Vertical line below ◌̩         |
| `G-] "`       | Double vertical line above ◌̎  |
| `G-] ^`       | Double overline ◌̿             |
| `G-] ,`       | Comma above ◌̓                 |
| `G-] <`       | Turned comma above ◌̒          |
| `G-] .`       | Reversed comma above ◌̔        |
| `G-] >`       | Comma above right ◌̕           |
| `G-] *`       | Dot above right ◌͘             |
| `` G-] ` ``   | Grave below ◌̖                 |
| `` G-] S-` `` | Acute below ◌̗                 |
| `G-] [`       | Bridge below ◌̪                |

#### Modifier letters

Unicode includes many *spacing modifier letters*:
  diacritic-like characters which are placed after the base letter rather than above or below it.
Examples include the IPA length symbol ◌꞉, or the apostrophe ◌ʼ used to indicate ejectives.
(Occasionally modifier letters are also used as independent letters;
  for instance, when an apostrophe is used as a letter for the glottal stop,
  the recommendation is to use the modifier apostrophe rather than the punctuation apostrophe.)
Like combining diacritics, modifier letters can be generated using key sequences starting with `G-]`:

| Input         | Result                                                                             |
|:--------------|:-----------------------------------------------------------------------------------|
| `G-] '`       | Modifier apostrophe ◌ʼ                                                             |
| `G-] p`       | Modifier prime ◌ʹ                                                                  |
| `G-] P`       | Modifier double prime ◌ʺ                                                           |
| `G-] &`       | Modifier letter double apostrophe ◌ˮ                                               |
| `G-] ]`       | Modifier vertical line ◌ˈ                                                          |
| `G-] G-]`     | Modifier vertical line below ◌ˌ  (i.e. IPA stress symbol)                          |
| `G-] :`       | Modifier colon ◌꞉                                                                  |
| `G-] ;`       | Modifier triangular colon ◌ː (i.e. IPA length symbol)                              |
| `G-] =`       | Modifier equals sign ◌꞊                                                            |
| `G-] -`       | Modifier macron ◌ˉ                                                                 |
| `G-] _`       | Modifier low macron ◌ˍ                                                             |
| `G-] G-'`     | Modifier acute accent ◌ˊ                                                           |
| `G-] G-‘`     | Modifier grave accent ◌ˋ                                                           |
| `G-] G-5`     | Caron ◌ˇ                                                                           |
| `G-] ~`       | Small tilde ◌˜                                                                     |
| `G-] )`       | Modifier dot vertical bar ◌ꜗ                                                       |
| `G-] /`       | Modifier dot slash ◌ꜘ                                                              |
| `G-] G--`     | Modifier dot horizontal bar ◌ꜙ                                                     |
| `G-] }`       | Modifier lower right corner ◌ꜚ                                                     |
| `G-] G-V`     | Modifier up arrow ◌ꜛ                                                               |
| `G-] G-v`     | Modifier down arrow ◌ꜜ                                                             |
| `G-] G-.`     | Sinological dot ◌ꞏ (commonly used as half-colon length symbol)                     |
| `G-] 9`       | Modifier begin low tone ˻ (this and below characters are also used for bracketing) |
| `G-] 0`       | Modifier end low tone ˼                                                            |
| `G-] G-9`     | Modifier begin high tone ˹                                                         |
| `G-] G-0`     | Modifier end low tone ˺                                                            |
| `G-] G-S-,`   | Modifier cedilla ¸                                                                 |

#### Diacritics spanning two letters

Certain diacritics span two letters rather than one —
  for example, the tie used for IPA diphthongs such as ‘a͡i’, or the line above the Menominee grapheme ‘a͞e’.
Such diacritics may be created using key sequences starting with `G-2`:

- `G-2 G-9` yields the double inverted breve below ‘◌͜◌’
- `G-2 G-0` yields the double inverted breve above ‘◌͝◌’
- `G-2 G--` yields the double macron above ‘◌͞◌’
- `G-2 _` yields the double macron below ‘◌͟◌’
- `G-2 ~` yields the double tilde above ‘◌͠◌’
- `G-2 G-6` yields the double tie above ‘◌͡◌’

These diacritics must be entered between the two individual letters they span;
  for instance, ‘a͞e’ is entered as `a G-2 G-- e`.

#### Letters with hooks and bars

Conkey includes three dead keys to enter letters with hooks (e.g. ‘ɓ’ and ‘ƒ’) and bars (e.g. ‘ð’ and ‘ʉ’).
Hooks are produced via the `G-{` dead key, which can produce the following letters:

| Input | Result (after `G-{`) | Uppercase? |
|:------|:-------|:-----------|
| `b`   | ɓ      | ✔          |
| `c`   | ƈ      | ✔          |
| `d`   | ɗ      | ✔          |
| `G-d` | ɖ      | ✔          |
| `f`   | ƒ      | ✔          |
| `g`   | ɠ      | ✔          |
| `h`   | ɦ      | ✔          |
| `j`   | ʝ      | ✔          |
| `G-j` | ᶁ      | ✘          |
| `k`   | ƙ      | ✔          |
| `l`   | ɭ      | ✘          |
| `G-l` | ꞎ      | ✘          |
| `n`   | ɲ      | ✔          |
| `p`   | ƥ      | ✔          |
| `q`   | ɋ      | ✔          |
| `r`   | ɽ      | ✔          |
| `G-r` | ɚ      | ✘          |
| `s`   | ʂ      | ✔          |
| `G-s` | ȿ      | ✔          |
| `t`   | ƭ      | ✔          |
| `G-t` | ʈ      | ✔          |
| `u`   | ꭒ      | ✘          |
| `v`   | ʋ      | ✔          |
| `w`   | ⱳ      | ✔          |
| `y`   | ƴ      | ✔          |
| `z`   | ȥ      | ✔          |
| `G-z` | ɀ      | ✔          |

For instance, ‘ɓ’ can be entered using `G-{ b`.

Bars are produced with the `G-\` and `G-|` dead keys —
  more than one is necessary as some barred characters have up to four variations.
The key combinations are listed in the following table:

| Input   | Result (after `G-\`) | Uppercase? | Result (after `G-\|`)   | Uppercase? |
|:--------|:-----------    |:----       |:----        |:----|
| `a`     | ⱥ              |  ✔         |             |     |
| `b`     | ƀ              |  ✔         |             |     |
| `c`     | ȼ              |  ✔         | ꞓ           |  ✔  |
| `d`     | ð              |  ✔         | đ           |  ✔  |
| `e`     | ɇ              |  ✔         |             |     |
| `g`     | ǥ              |  ✔         |             |     |
| `h`     | ħ              |  ✔         |             |     |
| `i`     | ɨ              |  ✔         |             |     |
| `j`     | ɉ              |  ✔         | ɟ           |  ✘  |
| `k`     | ꝁ              |  ✔         |             |     |
| `l`     | ł              |  ✔         | ƚ           |  ✔  |
| `G-l`   | ɫ              |  ✔         | ⱡ           |  ✔  |
| `o`     | ø              |  ✔         | ɵ           |  ✔  |
| `p`     | ᵽ              |  ✔         | ꝑ           |  ✔  |
| `q`     | ꝗ              |  ✔         |             |     |
| `r`     | ɍ              |  ✔         |             |     |
| `s`     | ꞩ              |  ✔         |             |     |
| `t`     | ŧ              |  ✔         | ᵺ           |  ✘  |
| `G-t`   | ꝥ              |  ✔         | ꝧ           |  ✔  |
| `u`     | ʉ              |  ✔         |             |     |
| `y`     | ɏ              |  ✔         |             |     |
| `z`     | ƶ              |  ✔         |             |     |
| `G-j`   | ƛ              |  ✘         |             |     |

For instance, ‘ø’ is produced by typing `G-\ o`, and ‘ɵ’ is produced by typing `G-| o`,
  while ‘ⱡ’ and ‘Ⱡ’ are produced with `G-| G-l` and `G-| G-L` respectively.

Combining diacritics for bars are also provided.
‘◌̵’ and ‘◌̷’ are produced by `C-\ SPC` and `C-\ \` respectively,
  while the longer bars ‘◌̶’ and ‘◌̸’ are produced by `C-| SPC` and `C-| \` respectively.
Note that the use of these diacritics is discouraged when a corresponding precomposed character (in the table above) is available,
  since many fonts have poor support for the overlaid bar diacritics.


### Letters

#### Non-English letters

Conkey includes many Latin-script letters which are not used in English.
These letters may be accessed using one- and two-key sequences as follows:

- The most common letters may be entered using a single key with an AltGr modifier.
  This category includes letters currently used by European languages (e.g. ‘ß’),
    particularly widespread non-European letters (e.g. ‘ɛ’),
    and letters which are widely used by conlangers (e.g. ‘ɬ’).
  These letters are listed in the following table:

  | Input | Result | Uppercase? |
  |:------|:-------|:-----------|
  | `G-a` | æ      |  ✔         |
  | `G-e` | ɛ      |  ✔         |
  | `G-r` | ə      |  ✔         |
  | `G-g` | ɣ      |  ✔         |
  | `G-i` | ı      |  ✘         |
  | `G-l` | ɬ      |  ✔         |
  | `G-n` | ŋ      |  ✔         |
  | `G-o` | ɔ      |  ✔         |
  | `G-k` | œ      |  ✔         |
  | `G-s` | ß      |  ✔         |
  | `G-t` | þ      |  ✔         |
  | `G-u` | ɯ      |  ✔         |
  | `G-v` | ʌ      |  ✔         |
  | `G-z` | ʒ      |  ✔         |
  | `G-x` | ʔ      |  ✘         |
  | `G-c` | ʕ      |  ✘         |
  | `G-q` | ɂ      |  ✔         |
  | `G-b` | ꞌ      |  ✔         |
  | `G-X` | ʻ      |  ✘         |

- Letters used in current and former orthographies of Eurasia (e.g. ‘ƃ’ and ‘ƣ’) —
    including historic European letters (e.g. ‘ſ’), but excluding letters used primarily for the transcription of Semitic languages (e.g. ‘ḫ’) —
    can be accessed via two-key sequences starting with `G-f`.
  These letters are listed in the following table:

  | Input     | Result | Uppercase? |
  |:----------|:-------|:-----------|
  | `G-f a`   | ɐ      |  ✔         |
  | `G-f b`   | ƃ      |  ✔         |
  | `G-f G-b` | ʙ      |  ✘         |
  | `G-f G-c` | ᴄ      |  ✘         |
  | `G-f d`   | ƌ      |  ✔         |
  | `G-f e`   | ɜ      |  ✔         |
  | `G-f g`   | ƣ      |  ✔         |
  | `G-f h`   | ꜧ      |  ✔         |
  | `G-f G-h` | ʜ      |  ✘         |
  | `G-f j`   | ƕ      |  ✔         |
  | `G-f k`   | ⱪ      |  ✔         |
  | `G-f K`   | ᴋ      |  ✘         |
  | `G-f n`   | ꞑ      |  ✔         |
  | `G-f G-n` | ɴ      |  ✔         |
  | `G-f o`   | ɒ      |  ✔         |
  | `G-f G-q` | ꞯ      |  ✔         |
  | `G-f s`   | ſ      |  ✘         |
  | `G-f G-s` | s      |  ✘         |
  | `G-f u`   | ы      |  ✔         |
  | `G-f G-u` | ᴜ      |  ✘         |
  | `G-f w`   | ƿ      |  ✔         |
  | `G-f x`   | ⱨ      |  ✔         |
  | `G-f y`   | ȝ      |  ✔         |
  | `G-f z`   | ⱬ      |  ✔         |
  | `G-f '`   | ь      |  ✔         |

- All other letters (primarily those used by African languages) can be entered by two-key sequences starting with `G-d`.
  These letters are listed in the following table:

  | Input     | Result | Uppercase? |
  |:----------|:-------|:-----------|
  | `G-d a`   | ɑ      |  ✔         |
  | `G-d b`   | ꞵ     |  ✔         |
  | `G-d e`   | ǝ      |  ✔         |
  | `G-d h`   | ḫ      |  ✔         |
  | `G-d i`   | ɩ      |  ✔         |
  | `G-d G-i` | ɪ      |  ✔         |
  | `G-d k`   | ĸ      |  ✘         |
  | `G-d n`   | ƞ      |  ✔         |
  | `G-d s`   | ʃ      |  ✔         |
  | `G-d o`   | ꝏ      |  ✔         |
  | `G-d u`   | ʊ      |  ✔         |
  | `G-d w`   | ꞷ      |  ✔         |
  | `G-d y`   | ɥ      |  ✔         |
  | `G-d z`   | ƹ      |  ✔         |
  | `G-d 3`   | ꜫ      |  ✔         |
  | `G-d 4`   | ꜭ      |  ✔         |
  | `G-d 5`   | ꜯ      |  ✔         |
  | `G-d 8`   | ȣ      |  ✔         |
  | `G-d c`   | ꜥ      |  ✔         |
  | `G-d x`   | ꜣ      |  ✔         |
  | `G-d (`   | ʿ      |  ✘         |
  | `G-d )`   | ʾ      |  ✘         |


#### Click letters

Click letters can be produced using key sequences starting with `G-p`:

- ‘ʘ’ can be produced using `G-p @`
- ‘ǀ’ can be produced using `G-p |` or `G-p \` or `G-p c`
- ‘ǃ’ can be produced using `G-p !` or `G-p q`
- ‘ǂ’ can be produced using `G-p =` or `G-p v`
- ‘ǁ’ can be produced using `G-p x`
- ‘ʇ’ can be produced using `G-p t` (or `G-p T` for uppercase ‘Ʇ’)
- ‘ʖ’ can be produced using `G-p s`
- ‘ʗ’ can be produced using `G-p S-c`
- ‘ʞ’ can be produced using `G-p k` (or `G-p K` for uppercase ‘Ʞ’)

#### Greek letters

Greek letters can be produced using key sequences starting with `G-j`.
These key sequences are shown in the following table:

| Input   | Result | Uppercase? |
|:--------|:-------|:-----------|
| `G-j a` | α      | ✔          |
| `G-j b` | β      | ✔          |
| `G-j g` | γ      | ✔          |
| `G-j d` | δ      | ✔          |
| `G-j e` | ε      | ✔          |
| `G-j z` | ζ      | ✔          |
| `G-j h` | η      | ✔          |
| `G-j j` | θ      | ✔          |
| `G-j i` | ι      | ✔          |
| `G-j k` | κ      | ✔          |
| `G-j l` | λ      | ✔          |
| `G-j m` | μ      | ✔          |
| `G-j n` | ν      | ✔          |
| `G-j q` | ξ      | ✔          |
| `G-j o` | ο      | ✔          |
| `G-j p` | π      | ✔          |
| `G-j r` | ρ      | ✔          |
| `G-j s` | σ      | ✔          |
| `G-j c` | ς      | ✘          |
| `G-j t` | τ      | ✔          |
| `G-j u` | υ      | ✔          |
| `G-j f` | φ      | ✔          |
| `G-j x` | χ      | ✔          |
| `G-j v` | ψ      | ✔          |
| `G-j w` | ω      | ✔          |

Several Greek letters have alternate forms (‘ε’/‘ϵ’, ‘θ’/‘ϑ’, ‘Θ’/‘ϴ’ ‘κ’/‘ϰ’, ‘π’/‘ϖ’, ‘ρ’/‘ϱ’, ‘φ’/‘ϕ’),
  often used in mathematical texts.
These variants can be accessed by adding AltGr to the second key in the key sequence:
  for instance ‘φ’ is `G-j f`, whereas ‘ϕ’ is `G-j G-f`.
Note that these variant characters do not have uppercase forms, with the exception of the already-mentioned `G-j G-J` ‘ϴ’.

#### Superscripts and subscripts

Superscript letters and symbols can be entered using key sequences of the form `G-w <char>`, where `<char>` is one of the following:

- Any lowercase English letter other than `q`
- Any uppercase English letter other than `C`, `F`, `Q`, `S`, `X`, `Y` or `Z`
- Any number
- `+` or `-`
- `G-j` (which yields a superscript ‘ᶿ’), `G-r` (yielding ‘ᵊ’), `G-x` (yielding ‘ˀ’) or `G-c` (yielding ‘ˁ’)

For instance, a superscript ‘ʷ’ can be produced by `G-w w`.

Similarly, subscripts may be entered using key sequences of the form `G-W <char>`, where `<char>` is one of the following:

- Any number
- Any lowercase letter in the following list: `aehijklmnoprstuvx`
- `+` or `-`

#### Zhuang tone letters

Zhuang tone letters can be accessed via key sequences starting with `G-1`, shown in the following table:

| Input   | Result | Uppercase? |
|:--------|:-------|:-----------|
| `G-1 2` | ƨ      | ✔          |
| `G-1 3` | ɜ      | ✔          |
| `G-1 4` | ч      | ✔          |
| `G-1 5` | ƽ      | ✔          |
| `G-1 6` | ƅ      | ✔          |

### Symbols

#### Quotation marks, guillemots and chevrons


Quotation marks and guillemots can be entered using key sequences
  starting with `G-'` (for single quotes/guillemots) and `G-"` (for double quotes/guillemots):

|      | `G-’`…   | `G-"`… |
|:-----|:---------|:-------|
| …`(` | ‘        | “      |
| …`)` | ’        | ”      |
| …`,` | ‚        | „      |
| …`.` | ‛        | ‟      |
| …`<` | ‹        | «      |
| …`>` | ›        | »      |

For instance, `G-" )` produces the double quotation mark ‘”’, and `G-' <` produces the single guillemot ‘‹’.
Additionally, the chevrons ‘⟨’ and ‘⟩’ may be entered using the key sequences `G-< G-<` and `G-> G->` respectively.

#### Dashes

Dashes can be produced using key sequences starting with `G--`:
  the sequences `G-- m`, `G-- n`, `G-- -` and `G-- =`
  produce an em dash, en dash, hyphen and minus sign respectively.
(Plain `-` produces a *hyphen-minus*, an ASCII symbol which can act as both a hyphen and a minus.)
Additionally, `G-- S-=` produces a plain double hyphen ‘⹀’, and `G-- /` produces an oblique double hyphen ‘⸗’.

#### Dots

Various different kinds of dots can be produced using key sequences starting with `G-.`:

- `G-. .` produces an ellipsis ‘…’
- `G-. |` produces a middot or interpunct ‘·’
- `G-. )` produces a circular bullet point ‘•’
- `G-. >` produces a triangular bullet point ‘‣’
- `G-. 0` produces a degree sign ‘°’
- `G-. @` produces a diacritic carrier sign ‘◌’
  (used as a base to display diacritics, as in some tables above)

#### Other textual symbols and punctuation marks (including currencies and IP marks)

Aside from the punctuation marks already covered above,
Conkey can produce a wide variety of other punctuation marks and symbols,
  mostly via key sequences starting with `G-@`:
  
| Input              | Result                  |
|:-------------------|:------------------------|
| `G-!`              | ¡                       |
| `G-?`              | ¿                       |
| `G-&`              | ⁊ (i.e. Tironian et)   |
| `G-@ !`            | ‽                       |
| `G-@ ?`            | ⸘                       |
| `G-@ N`            | №                       |
| `G-@ A`            | ª                       |
| `G-@ O`            | º                       |
| `G-@ p`            | ¶                       |
| `G-@ s`            | §                       |
| `G-@ 1`            | †                       |
| `G-@ 2`            | ‡                       |
| `G-@ \|`           | ‖                       |
| `G-@ z`            | ◊                       |
| *Legal symbols:*   |                         |
| `G-@ C`            | ©                       |
| `G-@ P`            | ℗                       |
| `G-@ R`            | ®                       |
| `G-@ G-s`          | ℠                       |
| `G-@ T`            | ™                       |
| *Checks and crosses:* |                      |
| `G-@ y`            | ✓                       |
| `G-@ n`            | ✗                       |
| `G-@ G-Y`          | ✔                       |
| `G-@ G-N`          | ✘                       |
| `G-@ 0`            | ☐                       |
| `G-@ G-y`          | ☑                       |
| `G-@ G-n`          | ☒                       |
| *Currency:*        |                         |
| `G-@ c`            | c (i.e. cent symbol)    |
| `G-@ G-c`          | ¢                       |
| `G-@ e`            | €                       |
| `G-@ l` or `G-@ #` | £                       |
| `G-@ o`            | ¤                       |
| `G-@ r`            | ₹                       |
| `G-@ S`            | ₪                       |
| `G-@ Y`            | ¥                       |

#### Spaces

Conkey allows many different varieties of space to be produced via key sequences starting with `G-SPC` (i.e. AltGr+spacebar).
The types of space which can be produced using this dead key, along with their Unicode code points, are shown in the following table:

| Input      | Space produced                                        | Code point |
|:-----------|:------------------------------------------------------|:-----------|
| `G-SPC t`  | Tab                                                   | `U+0009`   |
| `G-SPC ~`  | Non-breaking space                                    | `U+00a0`   |
| `G-SPC n`  | En space                                              | `U+2002`   |
| `G-SPC m`  | Em space                                              | `U+2003`   |
| `G-SPC b`  | Three-per-em space                                    | `U+2004`   |
| `G-SPC v`  | Four-per-em space                                     | `U+2005`   |
| `G-SPC c`  | Six-per-em space                                      | `U+2006`   |
| `G-SPC 1`  | Figure space                                          | `U+2007`   |
| `G-SPC .`  | Punctuation space                                     | `U+2008`   |
| `G-SPC ,`  | Thin space                                            | `U+2009`   |
| `G-SPC \|` | Hair space                                            | `U+200a`   |
| `G-SPC +`  | Medium mathematical space                             | `U+205f`   |
| `G-SPC !`  | Narrow no-break space                                 | `U+202f`   |
| `G-SPC 0`  | Zero width space                                      | `U+200b`   |
| `G-SPC (`  | Zero width non-joiner                                 | `U+200c`   |
| `G-SPC )`  | Zero width joiner                                     | `U+200d`   |
| `G-SPC [`  | Open box ‘␣’ (symbol for space)                       | `U+2423`   |
| `G-SPC ]`  | Shouldered open box ⍽ (symbol for non-breaking space) | `U+237d`   |

#### Variation selectors

Some Unicode characters have several typographic variations.
These may be inputted using the three Unicode variation selectors:
`G-$ 1` for VS1 (general variation selector), `G-$ e` for VS15 (to enforce rendering as text), and `G-$ f` for VS16 (to enforce rendering as emoji).
For more information see the Unicode documentation, particularly <http://unicode.org/faq/vs.html>.

#### Arrows

Arrows are produced by key sequences starting with `G-/`.
These sequences are arranged in a roughly mnemonic way around the keyboard:
  for instance, the most common arrows ‘↑’, ‘←’, ‘↓’ and ‘→’
  are produced by typing `G-/ w`, `G-/ a`, `G-/ s` and `G-/ d` respectively.
The full list of key sequences is as follows:

| Input   | Result |
|:--------|:----|
| `G-/ w` | ↑   |
| `G-/ a` | ←   |
| `G-/ s` | ↓   |
| `G-/ d` | →   |
| `G-/ W` | ▲   |
| `G-/ A` | ◀   |
| `G-/ S` | ▼   |
| `G-/ D` | ▶   |
| `G-/ z` | ↔   |
| `G-/ x` | ↕   |
| `G-/ c` | ⇐   |
| `G-/ v` | ⇒   |
| `G-/ e` | ⇈   |
| `G-/ r` | ⇉   |
| `G-/ f` | ⇊   |
| `G-/ b` | ⇋   |
| `G-/ n` | ⇌   |
| `G-/ m` | ↦   |
| `G-/ t` | ⤚   |
| `G-/ y` | ⤙   |
| `G-/ g` | ⤜   |
| `G-/ h` | ⤛   |
| `G-/ u` | ↣   |
| `G-/ i` | ↢   |
| `G-/ j` | ⇔   |
| `G-/ k` | ⇕   |

#### Box-drawing characters

Common box-drawing characters are accessible through key sequences starting with `G-m`, as follows:

| Input     | Result |
|:----------|:----|
| `G-m -`   | ─   |
| `G-m \|`  | │   |
| `G-m [`   | ┌   |
| `G-m ]`   | ┐   |
| `G-m \`   | └   |
| `G-m /`   | ┘   |
| `G-m +`   | ┼   |
| `G-m <`   | ┤   |
| `G-m >`   | ├   |
| `G-m T`   | ┬   |
| `G-m _`   | ┴   |

#### Fractions

Fractions may be entered using key sequences starting with `G-/`:

| Input              | Result                  |
|:-------------------|:------------------------|
| `G-/ /`            | ⁄ (i.e. fraction slash) |
| `G-/ 1`            | ⅟                       |
| `G-/ 2`            | ½                       |
| `G-/ 3`            | ⅓                       |
| `G-/ 4`            | ¼                       |
| `G-/ 5`            | ⅕                       |
| `G-/ 6`            | ⅙                       |
| `G-/ 8`            | ⅛                       |
| `G-/ 0`            | ⅒                       |
| `G-/ 8`            | ⅔                       |
| `G-/ 7`            | ¾                       |

#### Mathematical and related symbols

Mathematical and related symbols can be produced by key sequences starting with `G-=`, as follows:
These include logical symbols, Unicode versions of Haskell syntax and operators, and other similar notation.
The full list of key sequences is as follows:

| Input     | Result |
|:--------  |:----   |
| *Basic algebra and calculus:* | |
| `G-= x`   | ×      | 
| `G-= '`   | ⋅      | 
| `G-= /`   | ÷      | 
| `G-= -`   | ±      | 
| `G-= s`   | √      | 
| `G-= p`   | ∝      | 
| `G-= %`   | ‰      | 
| `G-= 1`   | ′      | 
| `G-= 2`   | ″      | 
| `G-= 3`   | ‴      | 
| `G-= l`   | ℓ      |
| `G-= d`   | ∂      |
| `G-= D`   | ∇      |
| `G-= G-p` | ☉      |
| *Equality and inequality:* | |
| `G-= =`   | ≡      | 
| `G-= !`   | ≠      | 
| `G-= G-!` | ≢      | 
| `G-= q`   | ≈      | 
| `G-= P`   | ∼      | 
| `G-= <`   | ≤      | 
| `G-= >`   | ≥      | 
| `G-= .`   | ∘      | 
| *Logical symbols:* | |
| `G-= A`   | ∀      | 
| `G-= E`   | ∃      | 
| `G-= ~`   | ¬      | 
| `G-= ^`   | ∧      | 
| `G-= v`   | ∨      | 
| `G-=&`    | ∴      | 
| *Set theory:* | |
| `G-= 0`   | ∅      | 
| `G-= e`   | ∈      | 
| `G-= G-e` | ∉      | 
| `G-= U`   | ∪      | 
| `G-= I`   | ∩      | 
| `G-= {`   | ⊆      | 
| `G-= }`   | ⊇      | 
| `G-= G-{` | ⊂      | 
| `G-= G-}` | ⊃      | 
| `G-= \`   | ∖      | 
| `G-= G-q` | ℵ      | 
| `G-= n`   | ∞      | 
| *Haskell:* | |
| `G-= :`   | ∷      |
| `G-= *`   | ★      |
| `G-= b`   | ⊥      |
| `G-= t`   | ⊤      |
| `G-= (`   | ⦇      |
| `G-= )`   | ⦈      |
| `G-= [`   | ⟦      |
| `G-= ]`   | ⟧      |
| `G-= h`   | ≫      |
| `G-= g`   | ≪      |
| `G-= H`   | ⋙      |
| `G-= G`   | ⋘      |
| `G-= 8`   | ⁂      |
| `G-= \|`  | ⫴ |
| `G-= #`   | ⧻      |
| `G-= +`   | ⧺      |
| `G-= i`   | ‼      |
| `G-= @`   | ⊛      |
| `G-= a`   | ⊕      |
| `G-= G-x` | ⊗      |
| *Blackboard bold and calligraphic:* | |
| `G-= G-R` | ℜ      |
| `G-= G-I` | ℑ      |
| `G-= C`   | ℂ      |
| `G-= N`   | ℕ      |
| `G-= Q`   | ℚ      |
| `G-= R`   | ℝ      |
| `G-= Z`   | ℤ      |
| `G-= i`   | ⅈ      |
| `G-= j`   | ⅉ      |
