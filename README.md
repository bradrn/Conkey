# Conkey

Conkey is a keyboard layout aimed at conlangers.
As such, it focuses extensively on unusual diacritics and letters, aiming to cover more of these than any other keyboard.
Conkey also contains a large amount of typographical symbols.

For documentation, see [`Documentation.md`](https://github.com/bradrn/Conkey/blob/master/Documentation.md).

For intallation instructions, see https://github.com/bradrn/Conkey#installation.

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
> **Dan** Liberian alphabet ([source](http://std.dkuug.dk/jtc1/SC2/WG2/docs/n3481.pdf)):
> … Yi Dɛn aaꞵi, ɥ́ gɥ́ lɵ́ gbée pə saalɵɛ … Ƃi, ɥ zòɔpúú-mɛ̀nùa Kín káɛ, ɥ wɔkɔ̀ …
>
> **Ik** linguistic orthography (source: Schrock 2014):
> Kɔnɔ kaɪnɪ́ɛ́ nɔɔ iánée Máyɛ́ɛ Ɗiwᵃ, mɪta noo kíʝá ɔtáɪ́, enese ɛ́ba arútétikᵉ.
>
> **Chukchi** (source: Dunn 1999):
> Cawcəwatˀm ewət ŋˀocˀəqaɣte enaralˀət. Cakəɣetˀm ətlenjuqej nəppəluqin. Naqam ŋˀocˀəqaɣte ənpənacɣəqaɣte ətlˀat.
>
> **Polish** ([source](https://en.wikipedia.org/wiki/Quotation_mark#Polish)):  
> Mag skłonił się. […]  
> — Jak się nazywa ta wieś, panie? — zapytał przybysz. Kowal wzruszył ramionami.  
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

A version for Linux is available. A Mac OSX version is not planned;
  however, anyone interested is welcome to try porting Conkey to Mac OSX.

**There is now a Mac OSX version!** Build and install it using the instructions below.

If you are interested, ports of Conkey to other platforms would be welcomed;
  refer to the instructions [here](https://github.com/bradrn/Conkey/blob/master/ms2mim/README.md) for details.

## Installation

Install Conkey using the files from the [latest release](https://github.com/bradrn/Conkey/releases/latest).
Alternately, get the files by [building Conkey yourself](#building).

### Windows

Install by unzipping `installer.zip` and running the `setup.exe` file within.
If you already have a previous version of Conkey installed,
  it may be a good idea to uninstall that first using the Control Panel.

### Linux

Conkey is available on Linux as both an [m17n](https://www.nongnu.org/m17n/) keyboard and an [XCompose](https://wiki.debian.org/XCompose) file.
The former is best-supported, but somewhat more difficult to install and may not work on all systems;
  the latter should work more reliably, but may be slightly restricted in how it can be used.
I recommend trying to install the m17n version first, then using the XCompose file if that doesn’t work.

#### m17n

(Special thanks go to all the members of the [ZBB board](https://www.verduria.org/) for all their help in getting this version to work!
I don’t think I could have ported Conkey to Linux without your help.)

It is probably easiest to use the m17n keyboard together with the [IBus](https://github.com/ibus/ibus) input method framework,
  which supports m17n via its `ibus-m17n` input method.
The following installation instructions have been tested with Ubuntu:

1. Run `sudo apt install ibus-m17n` to install IBus and M17N.
2. Make a new directory `~/.m17n.d` with `mkdir ~/.m17n.d`, if it does not already exist.
3. Download `latn-conk.mim` from the releases page, and copy it to `~/.m17n.d`. (You will need to delete the previous version of Conkey if it is already installed.)
4. Run `ibus restart`. You may also need to log out and back in, or reboot, particularly if you already have a previous version of Conkey installed.
5. The new Conkey keyboard should now be available from the Settings page (under the `Other` language).

#### XCompose

Due to the nature of XCompose this input method has some peculiarities compared to the others.
In particular, it is distributed in two separate forms.
One (`.XCompose-modifiers`) uses the modifier key AltGr, just like the other versions of Conkey.
Unfortunately this does not always work:
  though Compose(5) suggests that XCompose should accept modifier keys without any problems,
  this fails to work on my machine ([and reportedly others](https://unix.stackexchange.com/questions/207067/modifier-keys-in-compose-sequence#comment1239515_343370)).
Thus, another version (`.XCompose-multikey`) is provided in which AltGr is replaced by the [compose key](https://en.wikipedia.org/wiki/Compose_key).
As long as the compose key is set to the right Alt key, this should for the most part behave similarly to the other versions of Conkey.
However, some differences can be observed, most prominently:

- Unlike AltGr, the compose key does not need to be held at the same time as the other keys: it may be released before the next key is pressed
- In key sequences containing two consecutive instances of AltGr (e.g. `G-' G-a` for ‘ǽ’, or `G-\ G-j` for ‘ƛ’),
  the compose key must be released and then pressed again before the second key
- Key sequences containing `S-SPC`, i.e. shift and space keys pressed at the same time, are not supported — though as of the time of writing Conkey does not use these sequences anyway

I thus recommend trying `.XCompose-modifiers` first, then trying `.XCompose-multikey` if the former makes things impossible to type.

The following installation instructions have been tested with NixOS:

1. Download `.XCompose-modifiers` or `.XCompose-multikey` from the releases page, and copy it to `~/.XCompose`.
2. If `.XCompose-multikey` is used, set the compose key to right Alt (or any other key of your choosing) by setting XKB option `compose:ralt` (e.g. in `service.xserver.xkbOptions` for NixOS)
3. Log out and then in to enable the new keybindings

For more on customising XKB, please refer to e.g. <https://wiki.archlinux.org/title/Xorg/Keyboard_configuration>.

### Mac OSX

(Thanks go to @akamchinjir for providing the instructions below.)

Conkey for Mac OSX is provided as a zipped `.bundle`.
To install Conkey:

1. Unzip the zipped directory.
2. Within the zip file, there should be a `Conkey.bundle` directory. Copy this directory into `~/Library/Keyboard Layouts`.
3. Log out and then back in. The new keyboard should now be available in the System Keyboard Preferences dialog.

The `~/Library` file may be hidden in Finder.
You can still navigate to it by choosing “Go to Folder” in the “Go” menu and then entering `~/Library` directly.
Or you can make it permanently visible by entering `chflags nohidden ~/Library` in the terminal.
If the `~/Library` folder does not already contain a “Keyboard Layouts” folder, just create it.

It should also work if you put the `.bundle` directory in `/System/Library`.

## Building

### Windows

Conkey uses MSKLC for development, as mentioned above.
To build the keyboard, use the `Project → Build DLL and Setup Package` menu item.
(Note that I find I have to uninstall Conkey before I can run this, since otherwise I get an error.)
This should create a `setup.exe` executable plus a number of other files.

### Linux

#### `ms2mim`

The `ms2mim` utility, included in this repository, is used to build keyboards for Linux and Mac OSX.
To build this utility, first install Haskell, preferentially using [GHCup](https://www.haskell.org/ghcup/).
Then change to the correct directory with `cd ms2mim`, and build the utility with `cabal build`.

#### m17n

After building `ms2mim`, the following command may then be used to generate a `latn-conk.mim` file
  in the current directory:

```
$ cabal exec -- ms2mim ../Conkey.klc latn-conk.mim --mim
```

This file may then be installed as per the instructions above.

#### XCompose

After building `ms2mim`, the following commands may be used
  to generate `.XCompose-modifiers` and `.XCompose-multikey` files respectively:

```
$ cabal exec -- ms2mim ../Conkey.klc ~/.XCompose-modifiers --xc --filter
$ cabal exec -- ms2mim ../Conkey.klc ~/.XCompose-multikey --xc --filter --multikey
```

### Mac OSX

To build Conkey for Mac OSX, first follow the instructions above to build `ms2mim`.
Then run the following command:

```
$ stack exec -- ms2mim ../Conkey.klc conkey.in --int --osx
```

This will generate an intermediate file `conkey.in` in the current directory.
To convert this intermediate file to a `.bundle` directory suitable for installation,
  it is recommended to use [akamchinjir/osxkb](https://github.com/akamchinjir/osxkb).
First download that repository, and build it using the [given instructions](https://github.com/akamchinjir/osxkb/blob/master/INSTALL).
Next you will need to create a configuration file using the instructions in [the README](https://github.com/akamchinjir/osxkb).
Ensure that the `datafile` field contains the path to the `conkey.in` intermediate file you created previously.
Alternately, you may use the `conkey.config` file provided with this repository.
Finally, run `osxkb path-to-the-configuration-file.conf`;
  this will generate a `.bundle` directory which may be installed using the instructions above.
