# Conkey

Conkey is a keyboard layout aimed at conlangers.
As such, it focuses extensively on unusual diacritics and letters, aiming to cover more of these than any other keyboard.
Conkey also contains a large amount of typographical symbols.

For documentation, see `Documentation.pdf`, from the [latest release](https://github.com/bradrn/Conkey/releases/latest).

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

### Linux (M17N)

(Special thanks go to all the members of the [ZBB board](https://www.verduria.org/) for all their help in getting this version to work!
I don’t think I could have ported Conkey to Linux without your help.)

Conkey for Linux is distributed as a `.mim` file, for use with the M17N library.

The following installation instructions have been tested with Ubuntu:

1. Run `sudo apt install ibus-m17n` to install IBus and M17N.
2. Make a new directory `~/.m17n.d` with `mkdir ~/.m17n.d`, if it does not already exist.
3. Download `latn-conk.mim` from the releases page, and copy it to `~/.m17n.d`. (You will need to delete the previous version of Conkey if it is already installed.)
4. Run `ibus restart`. You may also need to log out and back in, or reboot, particularly if you already have a previous version of Conkey installed.
5. The new Conkey keyboard should now be available from the Settings page (under the `Other` language).

### Mac OSX

(Thanks go to @akamchinjir for providing the instructions below.)

Conkey for Mac OSX is provided as a zipped `.bundle`.
To install Conkey:

1. Unzip the zipped directory.
2. Copy the newly-unzipped `.bundle` directory into `~/Library/Keyboard Layouts`.
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

### Linux (M17N)

Conkey can be used on Linux utilising the M17N library.
A `.mim` file for use with this library can be generated
  using the `ms2mim` program, included with this repository.

The [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) is recommended for building `ms2mim`.
The following commands can be used to build it:

```
$ cd ms2mim
$ stack build
```

After it has been built the following command may then be used to generate a `latn-conk.mim` file
  in the current directory:

```
$ stack exec -- ms2mim ../Conkey.klc latn-conk.mim --mim
```

This file may then be installed as per the instructions above.

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