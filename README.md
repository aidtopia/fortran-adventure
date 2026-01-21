# An Adventure in Fortran

This project is a translator that rewrites code written in a subset of Fortran IV into a portable C program in order to recreate the authentic experience of [Adventure](https://en.wikipedia.org/wiki/Colossal_Cave_Adventure) as played in the late 1970s.

The goal is to implement just enough of Fortran IV (and the DEC PDP-10 runtime environment) to be able to translate Adventure without making any changes to the source file(s).  Adventure uses only a fraction of the features of Fortran, which makes this a tractable effort.

## Status

There are several versions of Adventure that could be considered original.  The one I had always believed to be the original was WOOD0350v2, so that was the version I chose to translate.  It went so well, I investigated whether I could extend the translator to handle its closest relatives, including WOOD0350v1, which was probably the version I'd actually played as a kid, and the one I now consider to be original.

Here's the status of those efforts (using [The Adventure Family Tree](https://mipmip.org/advfamily/advfamily.html)'s nomenclature for identifying versions).

| Version | Translates | Compiles | Executes | Saves State |
|:--------|:-----------|:---------|:---------|:------------|
|[**WOOD0350 Ver. 2**](https://mipmip.org/advfamily/advfamily.html#WOOD0350)| YES | YES<sup>_b_</sup>| YES | YES<sup>_j_</sup> |
|[WOOD0350 Ver. 1](https://mipmip.org/advfamily/advfamily.html#WOOD0350)| YES<sup>_c_</sup> | YES<sup>_b_</sup> | YES<sup>_e_</sup> | YES<sup>_j_</sup> |
|[CROW0000 (F4, 1977-03-31)](https://mipmip.org/advfamily/advfamily.html#CROW0000)| YES<sup>_g_</sup> | YES<sup>h</sup> | YES<sup>_e, i_</sup> | YES<sup>_j_</sup> |
|[BLKT0350](https://mipmip.org/advfamily/advfamily.html#BLKT0350)<sup>_k_</sup> | no | no | no | no |
|[SUPN0350](https://mipmip.org/advfamily/advfamily.html#SUPN0350)<sup>_k_</sup> | no | no | no | no |

* <sup>_a_</sup> <ins>fixed</ins> <del>The compiler warns about two variables that are declared but unused.  These can be ignored.  The problem is understood but not currently a high priority.</del>
* <sup>_b_</sup> When optimization is enabled, the C compiler does more flow analysis and discovers a few instances of unreachable code, causing warnings.  The problem is understood and may soon be solved by more rigorous cross-referencing in the translator.
* <sup>_c_</sup> The translator warns about a type specification statement used later than it should be.  This is technically a bug in the Fortran source, but it's harmless in this case.
* <sup>_d_</sup> <ins>fixed</ins> <del>The tools\test.bat file does not properly handle the fact that this version is split into multiple source files, so you have to invoke the translator manually with both source files in the same command.</del>
* <sup>_e_</sup> The game expects its data file to be named `TEXT` (no directory path, no extension).  Launch the program with the command line option `-fTEXT=<path>` where `<path>` is the path to the actual data file.
* <sup>_f_</sup> I'm working on a plan to enable this.
* <sup>_g_</sup> Translator will warn about use of REAL, but it works.
* <sup>_h_</sup> Compiler warns of unreachable code because the source has unreachable code.
* <sup>_i_</sup> Commands must be ALL CAPS.  You can launch with the `-CAPS` option to have keyboard input transliterated to uppercase.
* <sup>_j_</sup> When the program is ending, the user is given an option to save a "core image" to a file.  The `-c<file>` option allows loading a previously saved core image.
* <sup>_k_</sup> Out of scope, for now.  It appears the translator's everything's-a-36-bit-integer model would not be sufficient for these versions.

## How to try it out

I recognize this looks a bit cumbersome.  ... [[keep reading](docs/howto.md)]

## Motivation

As far as I can tell, the version of Adventure I played in the late 1970s has never been ported to a modern system with 100% fidelity.  ... [[keep reading](docs/motivation.md)]

## Contributing

This is primarily a personal project, but I'm open to contributions from others.  ... [[keep reading](CONTRIB.md)]
