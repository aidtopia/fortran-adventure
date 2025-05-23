## Now that It's Right

I'm completely delighted that the translator produces a working, playable version of the game that doesn't have the bugs that I'm used to seeing in other ports (like the inability to follow the stream to the grate at the beginning of the game).

I spent another day improving the translator so that it doesn't declare (most) variables that aren't used and doesn't create labels that are never targeted by a goto.

There are two unnecessary variable declarations left.  Both are parameters in a statement function definitions.  Other than the compiler warnings, these don't cause any harm.  Eliminating those is going to take some more thought.

### What's Next

- [ ] Move the project to a public repository
- [ ] Reimplement statement functions to avoid unnecessary variables
- [ ] Play an entire game [I've gotten really close]
- [ ] Provide a command line option to map the hardcoded file name to another
- [ ] Try the ADVEN.F4 version
- [ ] Enable save and restore by emulating core images
- [ ] Enable a transcript
- [ ] Write better notes about how Adventure works


## ADVEN.F4

I've been developing and testing my translator with a version called ADVENT.FOR, whose source code seemed the most similiar to the version I read as a kid.  But I am slightly suspicious that it's not actually the original, in part because some of the messages in the FORMAT statements are in mixed case even though the accompanying data file is ALL CAPS.  (My translator up-cases the source, so the output is still ALL CAPS.)

I'm now starting to try another version that seems like it could be closer to the original.  It's called ADVEN.F4.  Here's a log of how that goes.

### Overly strict phase enforcement

ADVENT.F4 fails on a LOGICAL statement, apparently because the opening declarations are in a slightly different order that violates the order expected by the translator phases.

I modified that parser to accept the out-of-order statement(s), but it issues a warning.

### Separate source files

ADVEN.F4 contains just the main program.  A second source file (ADVN2.F4) has all of the FUNCTIONs and SUBROUTINEs.  Each one translates to a syntactically correct C file.  However, each is missing declarations for what it needs from the others, and each is structured like complete programs (with a `main` function), so even ignoring the missing declarations, they cannot be linked.

One option is to just concatenate the sources, which probably would work just fine, but having the produce separate translation units that can be linked might make for a fun challenge.
