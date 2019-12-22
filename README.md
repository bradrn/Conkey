# Conkey

Conkey is a Windows keyboard layout aimed at conlangers.
As such, it focuses extensively on unusual diacritics and letters, aiming to cover more of these than any other keyboard.
Conkey also contains a large amount of typographical symbols.

For documentation, see [`Keys.md`](Keys.md).

## Installation

To install, go to the [latest release](https://github.com/bradrn/Conkey/releases/tag/v1.0.0) and download the file named `installer.zip`.
Once this file has been downloaded, unzip it and run the file named `setup.exe`.

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
> g ∘ f = \x → g (f x)
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

A Mac ~~or Linux version~~ is not planned; however, anyone interested is welcome to try porting Conkey to those platforms.  
**There is now a Linux version!** Build it using the instructions below.

## Installation

Install Conkey using the files from the [latest release](https://github.com/bradrn/Conkey/releases/latest).
Alternately, get the files by [building Conkey yourself](#building).

## Windows

Install by unzipping `installer.zip` and running the `setup.exe` file within.
If you already have a previous version of Conkey installed,
  it may be a good idea to uninstall that first using the Control Panel.

## Linux

(Special thanks go to all the members of the [ZBB board](https://www.verduria.org/) for all their help in getting the Ubuntu version to work!
I don’t think I could have ported Conkey to Ubuntu without your help.)

Conkey for Linux is distributed as a `.mim` file, for use with the M17N library.
It has been tested with Ubuntu and IBus, but should work on other systems as well.

The following installation instructions have been tested with Ubuntu:

1. Run `sudo apt install ibus-m17n` to install IBus and M17N.
2. Make a new directory `~/.m17n.d` with `mkdir ~/.m17n.d`, if it does not already exist.
3. Download `latn-conk.mim` from the releases page, and copy it to `~/.m17n.d`.
4. Run `ibus restart`.
5. The new Conkey keyboard should now be available from the Settings page (under the `Other` language).

## Building

### Windows

Conkey uses MSKLC for development, as mentioned above.
To build the keyboard, use the `Project → Build DLL and Setup Package` menu item.
(Note that I find I have to uninstall Conkey before I can run this, since otherwise I get an error.)
This should create a `setup.exe` executable plus a number of other files.

### Linux

As mentioned above, Conkey is developed on Windows, using MSKLC.
However, this repository contains a Haskell program `ms2mim`,
  which converts a MSKLC file to a `.mim` file for use with M17N.
The [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) is recommended for building this program.
Use the following commands:

```
$ cd ms2mim
$ stack build
$ stack exec -- ms2mim ../Conkey.klc ../latn-conk.mim
```

This should generate a file `latn-conk.mim` for use in Linux.
