## Motivation

As far as I can tell, the version of Adventure I played in the late 1970s has never been ported to a modern system with 100% fidelity. (See Note 1.)

Many ports have been based on extended versions (more rooms, puzzles, and treasures).  Many have modernized aspects of the experience, like replacing the ALL CAPS text with mixed case, adding a status bar, etc.  Most have made simplifying changes, like the elimination of wizard mode.

I don't really mind some changes, like mixed-case text.  Then again, a status bar changes the feel of the game more than I would have expected.  And I wanted to once again experience the challenge of proving I'm a wizard in order to play when the cave is closed.

Even the best ports have introduced subtle differences and sometimes even bugs.  So any visible change is&mdash;to me&mdash;a red flag.

My particular pet peeves are omissions or bugs that break some of the methods of navigating the world.  There were various ways to navigate in portions, both above ground and below.  But all anyone remembers is that you had to use compass directions.  Part of the problem is that the other forms of travel were often broken in the ports.  Subsequent text adventure games and works of interactive fiction, regardless of genre, solidified the compass as the one true way to navigate, and that has always disappointed me.  It's difficult to demonstrate the alternatives in the game that started the category because it's one of the most common ways the ports behave differently than the original.

Why aren't there ports based on the original, canonical source?

Those who have wanted to port the game have been warned away from using the early versions of the source because it makes "extensive" use of peculiar features of DEC Fortran IV on the PDP-10.

I would not be turned away.

I wanted to play the game as it was when I first played it.

My theory is that mechanical translation is more likely than a manual translation to produce program that behaves identically to the original.

So rather than attempt yet another port (and thus inject my own bugs), I set out to write <strike>an interpreter</strike> _a translator_ for the minimal subset of Fortran IV necessary to run the Adventure from the original unaltered source code and data file. (See Note 2.)

And that meant I had to face with hazards of the aforementioned peculiar features.

So let's [dive in](peculiar.md).

---

**Notes**

1. As I've worked on this project, I've learned that some ports have been updated, fixing at least some of the telltale bugs I'm used to spotting.  So it's possible that there may be one out there that is a faithful port.  At this point, I don't care.  I'm having fun.

2. A couple weeks into this project, I came across a [project](https://github.com/swenson/adventwure/tree/main) that uses a PDP-10 Fortran IV interpreter written in Python.  Its existence was part of the impetus to switch my projects from interpreter to translator.
