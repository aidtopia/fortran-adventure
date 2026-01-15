# An Adventure in Fortran

This project is a translator that rewrites code written in a subset of Fortran IV into a portable C program in order to recreate the authentic experience of [Adventure](https://en.wikipedia.org/wiki/Colossal_Cave_Adventure) as played in the late 1970s.

The goal is to implement just enough of Fortran IV (and the DEC PDP-10 runtime environment) to be able to translate Adventure without making any changes to the source file(s).  Adventure uses only a fraction of the features of Fortran, which makes this a tractable effort.

## Status

It turns out that there are several versions of Adventure that could be considered original.

The one I've always considered the original is WOOD0350v2.  Initially, that was the only version I was hoping to translate.  That went so well, I investigated whether I could extend the translator to handle its closest relatives.  Here's the status of those efforts (using [The Adventure Family Tree](https://mipmip.org/advfamily/advfamily.html)'s nomenclature for identifying versions).

| Version | Translates | Compiles | Executes | Saves State |
|:--------|:-----------|:---------|:---------|:------------|
|[**WOOD0350 Ver. 2**](https://mipmip.org/advfamily/advfamily.html#WOOD0350)| YES | YES<sup>_b_</sup>| YES | not yet<sup>_f_</sup> |
|[WOOD0350 Ver. 1](https://mipmip.org/advfamily/advfamily.html#WOOD0350)| YES<sup>_c_</sup> | YES<sup>_b_</sup> | YES<sup>_e_</sup> | not yet<sup>_f_</sup> |
|[CROW0000 (F4, 1977-03-31)](https://mipmip.org/advfamily/advfamily.html#CROW0000)| YES<sup>_g_</sup> | YES<sup>h</sup> | YES<sup>e, i</sup> | not yet<sup>_f_</sup> |

* <sup>_a_</sup> <del>The compiler warns about two variables that are declared but unused.  These can be ignored.  The problem is understood but not currently a high priority.</del> <ins>fixed</ins>
* <sup>_b_</sup> The optimizer (which does more flow analysis) warns of a few instances of unreachable code.  Some subroutines do not return (by design), but the translator doesn't realize that.  The problem is understood but not currently a high priority.
* <sup>_c_</sup> The translator warns about a type specification statement used later than it should be.  This is technically a bug in the Fortran source, but it's harmless in this case.
* <sup>_d_</sup> <ins>fixed</ins> <del>The tools\test.bat file does not properly handle the fact that this version is split into multiple source files, so you have to invoke the translator manually with both source files in the same command.</del>
* <sup>_e_</sup> The game expects its data file to be named `TEXT` (no directory path, no extension).  Launch the program with the command line option `-fTEXT=<path>` where `<path>` is the path to the actual data file.
* <sup>_f_</sup> I'm working on a plan to enable this.
* <sup>_g_</sup> Translator will warn about use of REAL, but it works.
* <sup>_h_</sup> Compiler warns of unreachable code because the source has unreachable code.
* <sup>_i_</sup> Commands must be ALL CAPS.  You can launch with the `-CAPS` option to have keyboard input transliterated to uppercase.

For now, I'm considering [BLKT0350](https://mipmip.org/advfamily/advfamily.html#BLKT0350) and [SUPN0350](https://mipmip.org/advfamily/advfamily.html#CSUPN0350) to be out-of-scope for this project.  It appears the translator's everything's-a-36-bit-integer model would not be sufficient for those version.

## How to try it out

I recognize this looks a bit cumbersome.  ... [[keep reading](docs/howto.md)]

## Motivation

As far as I can tell, the version of Adventure I played in the late 1970s has never been ported to a modern system with 100% fidelity.  ... [[keep reading](docs/motivation.md)]

## Contributing

This is primarily a personal project, but I'm open to contributions from others.  ... [[keep reading](CONTRIB.md)]
