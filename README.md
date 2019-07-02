# Conkey

Conkey is a Windows keyboard layout aimed at conlangers.
As such, it focuses extensively on unusual diacritics and letters, aiming to cover more of these than any other keyboard.
Conkey also contains a large amount of typographical symbols.

For documentation, see [`Keys.md`](Keys.md).

## Samples

The following language samples were typed using only Conkey:

> **Nuu-Chah-Nulth** ([source](https://www.omniglot.com/writing/nuuchahnulth.htm)):
> ʔUyaaƛaḥ hawiiʔaƛii maapt̓ał c̓išaaʔatḥ ʔuukʷił yuułuʔiłʔatḥ ʔaḥʔaaʔaƛsi n̓ačuʔałʔaƛsi hiikʷis.
>
> **Khoekhoe** ([source](https://www.omniglot.com/writing/khoekhoe.htm)):
> ǂKam !ũi-aob gye ǁẽib di gūna ǃhomi ǃna gye ǃũi hã i.
>
> **Azerbaijani** 1922 alphabet ([source](https://www.omniglot.com/writing/azeri.htm)):
> Butun insanlar ləƶaqət və huqyqlarьna gɵrə azad və bərabər doƣylyrlar.
>
> **Zhuang** 1957 alphabet ([source](https://www.omniglot.com/writing/zhuang.htm)):
> Bouч bouч ma dəŋƨ laзƃɯn couƅ miƨ cɯyouƨ, cinƅyenƨ cəuƽ genƨli bouчbouч giŋƨdəŋз.
>
> **Polish** ([source](https://en.wikipedia.org/wiki/Quotation_mark#Polish)):  
> Mag skłonił się. […]  
> — Jak siē nazywa ta wieś, panie? — zapytał przybysz. Kowal wzruszył ramionami.  
> — Głupi Osioł.
>
> **Various typographical marks** (source: various Wikipedia articles)  
> „Hast du den Artiken ‚EU-Erweiterung‛ gelesen?‟  
> « Voulez‐vous un sandwich, Henri ? »  
> What are those‽  
>
> **Mathematics**:
> ∀ a,b,c,x ∈ ℂ. (ax² + bx + c = 0) ⇒ (x = (−b ± √(b² - 4ac)) ÷ 2a)
>
> **Haskell**:
> ```haskell
> (∘) ∷ ∀ α β γ. (β → γ) → (α → β) → (α → γ)
> f ∘ g = \x → g (f x)
> ```

(Yes, I know those last two aren’t strictly languages, but they’re good as a demonstration…)

## Design Principles

Conkey is based around the following design principles:

- It is based on a **US keyboard layout**.
  (A UK version is not planned, but I am more than happy for someone else to make one if they are interested.)
- All special characters are to be accessed using **AltGr only**. The base US layout is **inviolate**, and the **control key is left unused**.
- **Mnemonics** to be used whenever possible. Although many characters have no obvious mnemonic, when one does exist it should be used.
- Conkey supports **only Latin scripts** (not including the IPA).
  Characters from other scripts should only be added if they have been used at some point in a Latin-based script.

Any future development should conform to these principles.

## Development

Conkey uses the [Microsoft Keyboard Layout Creator](https://support.microsoft.com/en-au/help/823010/the-microsoft-keyboard-layout-creator) (MSKLC) for development.
This is highly recommended for any complex work on Conkey.
However, if you want to make simple modifications only, the [keyboard layout description file](Conkey.klc) has a fairly simple plaintext format which can easily be modified.

A Mac or Linux version is not planned; however, anyone interested is welcome to try porting Conkey to those platforms.
