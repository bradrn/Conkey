# ms2mim

This is a small program to convert MSKLC keyboard layouts to various other formats.
Currently, it can convert to `mim` files (for use with M17N),
  as well as two different varieties of intermediate files (a ‘generic’ one as well as one for OSX).
(Originally it could only convert to `mim`, hence the name `ms2mim`.)

Usage is as follows:

```
$ ms2mim path-to-msklc-file.klc path-to-output-file [--mim|--int] [--osx]
```

Where:

- `--mim` specifies the output format as an M17N file;
- `--int` specifies the output format as an intermediate file;
- `--int` and `--osx` together specify the output format as an intermediate file, altered slightly for OSX.

If you forget an argument, or put them in the wrong order, `ms2mim` will most probably crash.
(I said it was small, not well-written!)

## Extending to other platforms

If you want to port Conkey to another platform, you will need to add your platform as an output format for `ms2mim`.
There are two ways of doing this.
If you know Haskell, the recommended approach is to write a conversion module,
  containing a function
  which takes in a `Convert.Intermediate.Intermediate`
  and outputs a `String` (or other appropriate type) containing the converted keyboard.
Then alter `main` to add the appropriate command-line argument.
Refer to the the existing conversion code for more details on how to do this.

Alternately, if you don’t know Haskell,
  you can still port Conkey to another platform using the intermediate file facility.
As mentioned above, with the `--int` option, `ms2mim` will output an ‘intermediate file’.
The format of this file is very simple, as shown in the following sample:

```
æ G-a
Æ G-A
ǽ G-' G-a
Ǽ G-' G-A
ɓ G-{ b
Ɓ G-{ B
← G-/ a
→ G-/ d
```

Here, the first part of each line (before the first space) gives the output character;
  the rest of each line contains the key (or key sequence) used to generate that character.
`G-` is used to represent AltGr, and `C-` is used for Control (though this does not occur often in Conkey);
  the Shift key is not represented explicitly,
  but may be deduced if needed from the key given.
`G-{`, for instance, is the same as AltGr+Shift+`[`.
The space key, represented `[SPC]` in the intermediate file, is a special case:
  the combination Shift+Space is represented as `S-[SPC]`.

(The output from `--mim --osx` is very similar,
  but contains some small differences which make parsing slightly easier on OSX:
  `G-` is replaced with `O-`, and lines with `S-[SPC]` are deleted)