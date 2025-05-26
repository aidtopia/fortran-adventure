# An Adventure in Fortran

This project is a translator that rewrites code written in a subset of Fortran IV (from the DEC PDP-10 era) as a portable C program in order to recreate the authentic experience of [Adventure](https://en.wikipedia.org/wiki/Colossal_Cave_Adventure) as played in the late 1970s.

The goal was to implement just enough of Fortran IV (and the DEC PDP-10 runtime environment) to be able to translate Adventure into standard, portable C.  Simplifications in its model of Fortran were baked in, so it's _not_ a good basis for creating a more full-featured Fortran translator.  For example, all variables are assumed to be INTEGER (or interchangeable with INTEGER).  Adding support for REAL, COMPLEX, and CHARACTER types would require some fundamental changes to the approach.

## Status

Using [The Adventure Family Tree](https://mipmip.org/advfamily/advfamily.html)'s nomenclature for identifying versions of Adventure ...

| Version | Translates | Compiles | Executes | Saves State |
|:--------|:-----------|:---------|:---------|:------------|
|[WOOD0350 Ver. 2](https://mipmip.org/advfamily/advfamily.html#WOOD0350)| YES | YES<sup>_a, b_</sup>| YES | not yet<sup>_f_</sup> |
|[WOOD0350 Ver. 1](https://mipmip.org/advfamily/advfamily.html#WOOD0350)| YES<sup>_c, d_</sup> | YES<sup>_a, b_</sup> | YES<sup>_e_</sup> | not yet<sup>_f_</sup> |
|[CROW0000_f4_1977-03-31](https://mipmip.org/advfamily/advfamily.html#CROW0000)| not yet<sup>_g_</sup> | -- | -- | -- |

* <sup>_a_</sup> The compiler warns about two variables that are declared but unused.  These can be ignored.  The problem is understood but not currently a high priority.
* <sup>_b_</sup> The optimizer (which does more flow analysis) warns of a few instances of unreachable code.  Some subroutines do not return (by design), but the translator doesn't realize that.  The calls sites are bookended by code to ensure the temporary variable stack remains balanced, resulting in unreachable code.  These can be safely ignored.  The problem is understood but not currently a high priority.
* <sup>_c_</sup> The translator warns about a type specification statement used later than it should be.  This is technically a bug in the Fortran source, but it's harmless in this case.
* <sup>_d_</sup> The tools\test.bat file does not properly handle the fact that this version is split into multiple source files, so you have to invoke the translator manually with both source files in the same command.
* <sup>_e_</sup> The game expects its data file to be named `TEXT` (no directory path, no extension).  Launch the program with the command line option `-fTEXT=<path>` where `<path>` is the path to the actual data file.
* <sup>_f_</sup> I'm working on a plan to enable this.
* <sup>_g_</sup> This version uses a floating point random number generator, so I'm going to have to add some support for REAL variables and constants.

---

This repository does _not_ include the Adventure sources and data files.  Those are already widely available.  If you want to get your hands on the sources, I suggest starting with the aforementioned [Adventure Family Tree](https://mipmip.org/advfamily/advfamily.html) and using its links to the [Interactive Fiction Database](https://ifdb.org/).

## Motivation

As far as I can tell, the version of Adventure I played in the late 1970s has never been ported to a modern system with 100% fidelity. (See Note 1.)

Many ports have been based on extended versions (more rooms, puzzles, and treasures).  Many have modernized aspects of the experience, like replacing the ALL CAPS text with mixed case, adding a status bar, etc.  Most have made simplifying changes, like the elimination of wizard mode.

I don't really mind some changes, like mixed-case text.  Then again, a status bar changes the feel of the game more than I would have expected.  And I wanted to be able to again experience the challenge of proving I'm a wizard in order to play when the cave is closed.

Even the best ports have introduced subtle differences and sometimes even bugs.  So any visible change is&mdash;to me&mdash; a red flag that something could be wrong.

My particular pet peeves are omissions or bugs that break some of the methods of navigating the world.  There were various ways to navigate in portions, both above ground and below.  But all anyone remembers is that you had to use compass directions.  Part of the problem is that the other forms of travel were often broken in the ports.  Subsequent "text adventure games" and "interactive fiction", regardless of genre, solidified the compass as the one true way to navigate and that has always disappointed me.  It's difficult to demonstrate the alternatives in the game that started the category because it's one of the most common ways the ports behave differently than the original.

Why aren't there ports based on the original, canonical source?

Those who have wanted to port the game have been warned away from using the early versions of the source because it makes "extensive" use of peculiar features of DEC Fortran IV on the PDP-10.  And so many ports are based on prior ports that may have already introduced changes and/or bugs.

I will not be turned away.

I want to play the game as it was when I first played it.

Rather than attempt yet another port (and thus inject my own bugs), I set out to write <strike>an interpreter</strike> _a translator_ for the minimal subset of Fortran IV necessary to run the Adventure from the original unaltered source code and data file. (See Note 2.)

And that means I have to deal with hazards of the aforementioned peculiar features.

So let's [dive in](docs/peculiar.md).

---

**Notes**

1. As I've worked on this project, I've learned that some of ports have been updated, fixing at least some of the telltale bugs I'm used to spotting in the ports.  So it's possible that there may be one out there that is a faithful port.  At this point, I don't care.  I'm having fun.

2. A couple weeks into this project, I came across a [project](https://github.com/swenson/adventwure/tree/main) that uses a PDP-10 Fortran IV interpreter written in Python.  Its existence was part of the impetus to switch my projects from interpreter to translator.


