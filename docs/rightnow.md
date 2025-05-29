## Now that It's Right

I'm completely delighted that the translator produces a working, playable version of the game that doesn't have the bugs that I'm used to seeing in other ports (like the inability to follow the stream to the grate at the beginning of the game).

I spent another day improving the translator so that it doesn't declare (most) variables that aren't used and doesn't create labels that are never targeted by a goto.

There are two unnecessary variable declarations left.  Both are parameters in a statement function definitions.  Other than the compiler warnings, these don't cause any harm.  Eliminating those is going to take some more thought.

### What's Next

- [x] Move the project to a public repository
- [ ] Reimplement statement functions to avoid unnecessary variables
- [x] Play an entire game [I didn't get all the treasures before lamp died]
- [x] Provide a command line option to map the hardcoded file name to another
- [x] Provide command line options to override DATE and TIME
- [x] WOOD0350v2 (a.k.a., advent.for)
- [x] WOOD0350v1 (a.k.a., adven.f4 and advn2.f4)
- [ ] CROW0000_f4_1977-03-31 (a.k.a., adv.f4) [very close, I think]
- [ ] Enable save and restore by emulating core images
- [ ] Write better notes about how Adventure works


## WOOD0350v1

I've been developing and testing my translator with the version dubbed WORD0350v2, whose source code (ADVENT.FOR) seemed the most similiar to the version I read as a kid.  But I am slightly suspicious that it's not actually the original, in part because some of the messages in the FORMAT statements are in mixed case even though the accompanying data file is ALL CAPS.  (My translator up-cases the source, so the output is still ALL CAPS.)

I learned of another version that seemed like it could be closer to the original.  [I've come to learn this is known as WOOD0350v1.]  It's called ADVEN.F4.  Here's a log of the changes the translator required to get this version working.

### Overly strict phase enforcement

ADVENT.F4 fails on a LOGICAL statement, apparently because the opening declarations are in a slightly different order that violates the order expected by the translator phases.

I modified that parser to accept the out-of-order statement(s), but it issues a warning.

### Separate source files

ADVEN.F4 contains just the main program.  A second source file (ADVN2.F4) has all of the FUNCTIONs and SUBROUTINEs.  Each one translates to a syntactically correct C file.  However, each is missing declarations for what it needs from the others, and each is structured like complete programs (with a `main` function), so even ignoring the missing declarations, they cannot be linked.

I made the translator accept multiple source files on the command line, and it treats all files as one long program.  This worked fine.  (If I ever want to display source file names and line numbers in diagnostic messages, this will make it a bit more awkward.)

Having the translator produce separate translation units that can be linked might make for a fun challenge&mdash;someday.

## CROW0000_f4_1977-03-31

Buoyed by my success so far, I wondered whether I could go back even farther.  There are some copies of Will Crowther's cave adventure from _before_ Don Woods's extensions.  These are known as CROW0000.  There are three versions from March 1977.  For each of those, there is a Fortran IV version and a Fortran 77 one.

Here's what it takes to get those to work.

### Indexing in DATA statements

The translator needed to handle index control in DATA statements.  For example:

```fortran
      DATA(JSPKT(I),I=1,16)/24,29,0,31,0,31,38,38,42,42,43,46,77,71,
     & 73,75/
```
[From CROW0000; continuation style changed to avoid confusion]

Like an input-output list, arrays can be initialized using DO-loop like indexing expressions, as long as the values are compile-time constants.  The WOOD0350 versions didn't use that functionality, so I hadn't provided it.

The translator uses distinct types for variable lists in DATA statements and item lists in input-output statements, because the former is a restricted form that must be evaluated at compile time while the latter is the more general form to be computed at run time.  Despite the syntactic similarity, it proved easier to treat them as fundamentally different.

As I tried to incorporate the additional functionality required by CROW0000, I tried again to unify variable lists and IO item lists, but in the end I kept them separate.  Unfortunately, that means there's nearly identical parsing code for each.

In the end, I got the indexing working in DATA statements.

### RAN and REAL

When I embarked on the project, I decided that, since the Adventure code&mdash;as I knew it at the time&mdash;uses only INTEGER, LOGICAL, and packed ASCII data, I could largely ignore data types and simply treat all values as INTEGERs in machine words.  (I still don't regret that decision.)

But CROW0000 relies on `RAN`, a system-provided random number generator that returns REAL values between 0.0 and 1.0.  The WOOD0350 versions provide their own INTEGER version of RAN, noting: 

```fortran
      INTEGER FUNCTION RAN(RANGE)

C  SINCE THE RAN FUNCTION IN LIB40 SEEMS TO BE A REAL LOSE, WE'LL USE ONE OF
C  OUR OWN.  IT'S BEEN RUN THROUGH MANY OF THE TESTS IN KNUTH VOL. 2 ...
```
[Did Don Woods mean "real _loser_" or was he saying something about the fact that the system-provided RAN returns a REAL?]

The random numbers are compared against REAL constants.  But that's it.  The program does not use REAL arithmetic.  It does not read or write REAL values.  It does not even store a REAL value in a variable.

I don't know anything about the floating point representation on the PDP-10, but that's fine.  Modern hardware uses IEEE 754 for floating point types.  Ignoring NaNs (and infinities?), the bit pattern of a floating point value using this representation can be treated as a signed integer (of the same size) for comparisons.

```c
void compare(float a, float b) {
    // assume neither a nor b is a NaN or an infinity
    assert(sizeof(float) == sizeof(int32_t));
    int32_t x = *(int32_t*)(&a);
    int32_t y = *(int32_t*)(&b);
    // all of these assertions will be true
    assert((x < y) == (a < b));
    assert((x > y) == (a > b));
    assert((x <= y) == (a <= b));
    // ... and so on ...
}
```

So, as long as we're careful about sign extension, it should be sufficient to stash the floating point representation in a machine word.

Apparently, RAN was a real loser, so the quality of our version doesn't need to be great.  We'll just wrap `rand` from the C run-time library.

So, to accommodate CROW0000, the translator needs:

[x] parse simple REAL constants
[x] allow REAL type specification statements
[x] provide an implementation of `RAN`

To avoid breaking WOOD0350, I had to modify the code that provides the "built-in" functions to avoid providing an implementation for a subprogram that the source code implements itself.

Hey, it works!

