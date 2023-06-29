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

Conkey is available in versions for both Linux and Mac OSX.
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

Installing Conkey on Linux requires several different components to be configured correctly:

- [**XKB**](https://en.wikipedia.org/wiki/X_keyboard_extension)
    manages the mapping between the physical keys on your keyboard and the keycodes received by your computer.
  Conkey requires this mapping to be set up so that you can use a key as the ‘AltGr’ modifier — also known as the ‘Level 3 Shift’ in XKB parlance.
- An **input method** (IM) provides the means to input letters, numbers and other characters.
  A number of these exist: I recommend [IBus](https://en.wikipedia.org/wiki/Intelligent_Input_Bus),
    but Conkey has also been tested with [Fcitx 5](https://fcitx-im.org/wiki/Fcitx_5) (which seems to work better on Wayland).
- The IM requires an **input method engine** (IME) to process individual keyboard layouts.
  Conkey requires [`m17n`](https://www.nongnu.org/m17n/) as its IME.
- Finally, your **desktop environment** (GNOME, KDE, Xfce, etc.) needs to autostart the IM on login so that it can interpret your typing.

Naturally, the exact configuration will differ depending on how your system is set up.
The following guidelines should work for most systems:

- First, remap a key on your keyboard to act as a modifier ‘AltGr’.
  (The usual choice for this is the right Alt key.)
  On most desktop environments, this can be configured in the settings, in a section named ‘Keyboard’ or similar.

  Otherwise, you can set an XKB option such as `lv3:ralt_switch`,
    either by running a command such as `setxkbmap -option lv3:ralt-switch` or by using a [configuration file](https://wiki.archlinux.org/title/Xorg/Keyboard_configuration#Using_X_configuration_files).
  (At least on my machine, the possible XKB options are listed in `/usr/share/X11/xkb/rules/base.lst`).
  Other possibilities are to [make a custom keyboard layout](https://wiki.archlinux.org/title/X_keyboard_extension#Editing_the_layout)
    with a key mapped to `ISO_Level3_Shift`,
    or to use [`xmodmap`](https://wiki.archlinux.org/title/Xmodmap)
      (e.g. to remap right-Alt to AltGr, run `xmodmap -e 'keycode 108 = ISO_Level3_Shift ISO_Level3_Shift ISO_Level3_Shift ISO_Level3_Shift'`).

  Note: remapping a key to AltGr means that it will no longer work in its previous function!
  This is why Right Alt is often a good choice — few programs require an Right Alt key specifically.

- Next, install an IM.
  If you’re using GNOME you might already have IBus installed.
  If not, installing IBus or Fcitx 5 should be straightforward:
    for details, consult the instructions for your distribution.
  (e.g. on Arch Linux, to install IBus use `pacman -S ibus` and set the [appropriate environment variables](https://wiki.archlinux.org/title/IBus#Integration).)
  You will also need to install `m17n` integration — usually this is in a package called `ibus-m17n`, `fcitx5-m17n` or similar.

- Ensure that your desktop environment launches the IM on login.
  GNOME may do this automatically;
    otherwise set up a command such as `ibus-daemon -drxR` or `fcitx5 -rd` to be run on login.

- Get the Conkey keyboard layout by downloading `latn-conk.mim` from [the latest release](https://github.com/bradrn/Conkey/releases/tag/v5.0.0).
  Create a directory called `.m17n.d` in your home directory (if it does not already exist), and place `latn-conk.mim` in it.

- Finally, log out and back in.
  It should now be possible to add Conkey as a keyboard layout in the IM settings.
  You can use the AltGr key you set up earlier to input diacritics, letters and symbols.

  Note: on IBus, for AltGr to work, you may need to select ‘Use system keyboard layout’ (in the ‘Advanced’ tab of the preferences).
  The equivalent setting on Fcitx5 should be enabled by default.

If the above instructions do not work, then you can also try the XCompose file mentioned [below](https://github.com/bradrn/Conkey#xcompose).

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

### XCompose

Some platforms provide a [compose key](https://en.wikipedia.org/wiki/Compose_key) to convert key sequences into alternate characters.
Though this does not provide a full ‘keyboard layout’ *per se*, Conkey has been adapted to work with XCompose,
  with the ‘compose key’ / ‘multi key’ taking the place of AltGr.

The following installation instructions should work for Linux:

1. Download the XCompose file from [the latest release](https://github.com/bradrn/Conkey/releases/tag/v5.0.0);
     rename it to `.XCompose` and move it into your home directory.
2. Enable the compose key, e.g. in the ‘Keyboard’ settings of your desktop environment, or by setting XKB option `compose:ralt`.
   (See the Linux section above for more on this).
3. Log out and then in to enable the new keybindings.

The same `.XCompose` file should also be compatible with [WinCompose](https://github.com/SamHocevar/wincompose) for Windows.
However, I have not tested this.
Follow the instructions in the linked documentation to install Conkey using this method.

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
