# Interpreting Adventure

## Motivation

What I consider the canonical version of Adventure has, as far as I can tell, never been ported to a modern system with 100% fidelity. (See Note 1.)

Many ports have been based on extended versions (more rooms, puzzles, and treasures).  Many have replaced the ALL CAPS text with mixed case.  Some have even added an inverse video status bar at the top of the screeen.  Most have made simplifying changes, like the elimination of wizard mode.  And some ports have been based on earlier ports, like a photocopy of a photocopy.

Even the best of these ports have subtle differences, and sometimes even bugs--other than the bugs in the original.  (My particular pet peeves are omissions or bugs that break some of the methods of navigating the world.)

Some ports are actually rewrites made in systems for creating text adventures.  These use the creative elements from the original but re-implement the logic with the facilities of the authoring system, substituting some of the game logic with standard libraries and switching out Adventure's primitive parser for a more modern one.  It's the same game, but a different experience.

Why aren't there ports based on the original, canonical source?

Those who have wanted to port the game have been warned away from the original because it makes "extensive" use of peculiar features of DEC Fortran IV on the PDP-10.

I will not be turned away.

I want to play the game as it was when I first played it.

Rather than attempt yet another port, I'm setting out to write <strike>an interpreter</strike> _translator_ for the minimal subset of Fortran IV necessary to run the Adventure from the original unaltered source code and data file. (See Note 2.)

And that means I have to deal with the aforementioned peculiar hazards.

So let's dive in.

## Peculiar Hazards

### Expressions

#### Alternate Truths

In Fortran IV on the PDP-10:

* INTEGER is a signed value stored as two's complement in a 36-bit machine word.

* LOGICAL is either `.TRUE.` or `.FALSE.` stored respectively as -1 or 0 in a 36-bit machine word.

* LITERAL is packed character data, which will be discussed in detail in a subsequent section.

Adventure uses only these types.

INTEGERs and LOGICALs appear to be interchangeable.  When an INTEGER is treated as a LOGICAL, the _sign_ of the value determine its truth.  This is different than C, in which the "non-zeroness" of the value determines its truth.

(As we'll see later, Adventure's literals are also treated as INTEGERs.)

The relational operators produce LOGICAL values.  In something like an if-statement, this translates directly to C:

| Fortran IV                       | C99                     |
| :------------------------------- | :---------------------- |
| `      INTEGER A, B`             | `int a = 42;`           |
| `      DATA A,B/42,13/`          | `int b = 13;`           |
| `      IF (A .GT. B) GO TO 100`  | `if (a > b) goto L100;` |

But we must be careful if the result of a comparison is stored in a variable, especially since that variable could be treated an integer.

| Fortran IV                       | C99 - INCORRECT!        |
| :------------------------------- | :---------------------- |
| `      LOGICAL X`                | `bool x;`               |
| `      X = A .GT. B`             | `x = a > b;`            |
| `      A = A + X`                | `a = a + x;`            |

This translation is incorrect.  In the Fortran case, `A + X` _subtracts_ 1 from `A`, but in C, it _adds_ 1.

So we must ensure the relational operators produce `.TRUE.` and `.FALSE.` (i.e., -1 and 0).

When testing the truth of an expression in the translation (e.g., to evaluate an if-statement), we will have to be certain we convert from Fortran truth (negative) to C truth (non-zero).

#### Short Circuits

In C, operators like `&&` and `||` use short-circuit evaluation, but Fortran's `.AND.` and `.OR.` do not.

If the subexpression on the right side has side effects, the behavior could be different between the original Fortran and a naive C translation.

In fact, I have the impression that Fortran doesn't distinguish between the logical and bitwise versions of the logic operators at all.  Since it uses the sign bit for truth, the effect is indistinguishable.

#### Expression Plan

To avoid problems with the different representations of truth, short circuiting, and operator precedence, the C code will not produce expressions with inline operators.  Instead, each will be implemented as a function call.

### Packed Character Data

Adventure makes extensive use of packing short character strings into a single machine word using format A5.  The 'A' stands for ASCII and the '5' means five characters are packed into the machine word.  In general, 'Ax' is an i/o conversion specification for input and output of x characters.

ASCII charaters are 7 bits, so A5 packing requires 35 bits. Words on the PDP-10 are 36 bits.

The first character of A5 occupies the seven highest bits of the word.  Each subsequent character occupies the next seven lower bits, and the least-significant bit is unused (presumably 0).

| character | bitmask ("octal) |
+-----------+------------------+
|    1st    |  "774000000000   |
|    2nd    |  "003760000000   |
|    3rd    |  "000017700000   |
|    4th    |  "000000077400   |
|    5th    |  "000000000376   |

When storing fewer than 5 characters using A5, the value is padded on the right with spaces.  So the first character always occupies the highest bits.

The A1 format puts the single ASCII character into the _high_ bits of the word.  Some of the DEC documentation calls this "left-justified".  Originally, I thought that referred to the bits being left-shifted, which would leave all of the lower bits set to zero.  Later I learned that it actually means the single character is padded with four spaces.

Padding with spaces also applies to literal constants in the source.  The literal `'A'` is equal to `'A    '`.

Observation:  For any Ax format, if the first character has an ASCII value of 64 (0x40 or "100) or higher, the machine word will test negative.  The Adventure code relies on that in at least one spot.

The largest integers I see in Adventure are in the tens or hundreds of thousands, so I was planning to use 32-bit integers for the "machine words" in my Fortran interpreter.  For the packed text, I was going to cheat by implementing the A5 formats as five SIXBIT characters per word.  (DEC's SIXBIT encoding has all of the characters necessary for all of the text in Adventure.)  I figured that since the interpretter would handle both the input and output formats, it would essentially be transparent to Adventure.

But that was before I spotted Adventure's A5TOA1 subroutine.  With my SIXBIT scheme, I'd have to re-implement that function, which is inconsistent with my goal of leaving the original source unchanged.  So it seems I need larger machine words.

The first higher size available to me is 64-bits.  I'm not concerned about wasted memory--we're interpretting a program that used to run in 1977.  For arithmetic and logical operations, 64-bit two's complement should be fine.

Plan:

* Use 64-bit integers for machine words.

* Implement the Ax formats as though the words are 36 bits wide.  The bitmasks in the table above should select corresponding characters.

* When packing Ax data, be sure to sign-extend so that tests for negative values will work.

The following extract from ADVENT.FOR by Will Crowther and Don Woods illustrates the program's reliance on the PDP-10's method of packing character data into machine words.

```fortran
    SUBROUTINE A5TOA1(A,B,C,CHARS,LENG)

C  A AND B CONTAIN A 1- TO 9-CHARACTER WORD IN A5 FORMAT, C CONTAINS ANOTHER
C  WORD AND/OR PUNCTUATION.  THEY ARE UNPACKED TO ONE CHARACTER PER WORD IN THE
C  ARRAY "CHARS", WITH EXACTLY ONE BLANK BETWEEN B AND C (OR NONE, IF C >= 0).
C  THE INDEX OF THE LAST NON-BLANK CHAR IN CHARS IS RETURNED IN LENG.

    IMPLICIT INTEGER(A-Z)
    DIMENSION CHARS(20),WORDS(3)
    DATA MASK,BLANK/"774000000000,' '/

    WORDS(1)=A
    WORDS(2)=B
    WORDS(3)=C
    POSN=1
    DO 1 WORD=1,3
    IF(WORD.EQ.2.AND.POSN.NE.6)GOTO 1
    IF(WORD.EQ.3.AND.C.LT.0)POSN=POSN+1
    DO 2 CH=1,5
    CHARS(POSN)=(WORDS(WORD).AND.MASK)+(BLANK-(BLANK.AND.MASK))
    IF(CHARS(POSN).EQ.BLANK)GOTO 1
    LENG=POSN
    WORDS(WORD)=SHIFT(WORDS(WORD),7)
2   POSN=POSN+1
1   CONTINUE
    RETURN
    END
```

Take particular note of using the sign of the variable `C` to determine whether to insert a space.  In the A5 packing, the sign of the machine word is determined by the high bit of the ASCII value of the first character.  If, for example, that character is `'A'` (ASCII 0b100'0001), then `C` will be negative and the code will insert a space.  If, on the other hand, it's a period (`'.'` = 0b010'1110) or a comma (`','` = 0b010'1100), then `C` will be positive, so no extra space will be inserted.  Looking at a table of ASCII, one can see that all of the letters have their high bit set, and most of the punctuation marks do not.  So this clever hack works well when concatenating English words and punctuation.

But for that hack to work without altering the code, the interpreter will needs to ensure that the sign of A5 packed text works as it does on the PDP-10.

This next extract shows how Adventure obfuscates the in-memory vocabulary words.  The obfuscation works by performing a bitwise exclusive-or on each A5-encoded vocabulary word with `'PHROG'`.  The comment is not quite right:  `'/7-08' .XOR. 'PHROG'` would yield -2 because the low bit in A5 is always 0.

```fortran
C  ....  NOTE THAT '/7-08' HAD BETTER NOT BE IN THE LIST, SINCE
C  IT COULD HASH TO -1.

1040	DO 1042 TABNDX=1,TABSIZ
1043	READ(1,1041)KTAB(TABNDX),ATAB(TABNDX)
1041	FORMAT(G,A5)
	IF(KTAB(TABNDX).EQ.0)GOTO 1043
C  ABOVE KLUGE IS TO AVOID AFOREMENTIONED F40 BUG
	IF(KTAB(TABNDX).EQ.-1)GOTO 1002
1042	ATAB(TABNDX)=ATAB(TABNDX).XOR.'PHROG'
```

### Pass by Reference

Arguments passed to Fortran SUBROUTINEs and FUNCTIONs are passed by reference.  In C, arguments are passed by value unless the code is written to pass by pointer.

My plan is to have the C code pass pointers rather than the values for all arguments.

I considered mapping a FUNCTION return value to an output argument in C, but, in the end, I just used the actual return value.  However, Fortran, the return value is set before the RETURN statement, so I needed a place to stash it.  Thus the C translation of a hypothetical Fortran `FUNCTION FOO` is called `fnFOO`, and it contains a local variable `vFOO` for holding the return value until it's needed.

Here's a dastardly hack:  Declare every INTEGER and LOGICAL variable as an array of one element.  When one of these single-element arrays are used as an argument to a subprogram, the array name will decay into a pointer, which is what we want when passing by reference.

```C
// Before
void subFOO(word_t *vPARAM1) {
    word_t vLOCAL = 13;
    *vPARAM1 = *vPARAM1 + vLOCAL;  // must deref vPARAM1 but not vLOCAL
}

// After
void subFOO(word_t *vPARAM1) {
    word_t vLOCAL[1] = {13};
    vPARAM1[0] = vPARAM1[0] + vLOCAL[0];  // args and locals treated identically
}

```

The remaining problem is how to pass constants and temporaries as arguments.  Consider:

```fortran
      GRATE=VOCAB(0+'GRATE',1)
```

A complete expression parser could dynamically create and destroy temporary variables (perhaps on a stack) in order to pass these arguments by value.  Translating expressions like these directly to C is not possible.

So here's another hack:  a circular buffer of memory locations for stashing temporaries.  When a constant or the result of evaluating an expression (which may be as simple as a constant) needs to be passed by reference, the next word in the buffer is allocated, the value is copied there, and the address of the temporary becomes the reference.  Since the temporaries are all simple values that don't require destructors, they can just be abandoned.  Since the buffer is finite, there will be no memory leaks.  If the circular buffer is large enough, no "live" temporaries will be overwritten.

NOTE:  This hack was not sufficient, which I didn't discover for quite a while.


### System Libraries

Beyond some essential "elemental functions" like `IABS` and `MOD0`, Adventure uses a few subprograms from the system library.

#### `OPEN`

The `OPEN` syntax used to access the data file seems to be a weird hybrid between a special statement and a library subroutine.  Adventure hardcodes the file name (as either `'ADVEN'` or `'ADVENT'` depending on the version).  Note the absence of a directory path or even a file extension.  The translation will have to provide some reasonable default behavior for locating the file to be opened.

The `OPEN` syntax is not documented in the older Fortran programmer's guides, but there's an online Oracle reference that describes it for Fortran 77, which I found useful.  Adventure's use of `OPEN` is straightforward, though some of the keywords are named slightly differently, so the translator had to acknowledge that.

NOTE:  Another version of the code uses a system subroutine called IFILE, which should be easy to map to an OPEN statement.

NOTE:  I want to provide a command line option to allow the user to map the (restricted) file name used by the program code to an actual file on the system.

#### `TIME` and `DATE`

Most ports eliminate the "wizard mode" code that's intended to limit access to the program during business hours.  I want to preserve this functionality if possible, so I'll need to provide `TIME` and `DATE` subroutines that work like the ones that came with the DEC Fortran implementation.

These "return" the time and date as short character strings, and the Adventure code relies on the specific format as it extracts the fields it requires.

### FORMATTED I/O

This was more challenging than I expected, and there are a couple behaviors I had to guess about.  I got some additional info on retrocomputing.stackexchange.com.  But it's possible there are lurking bugs due to imperfect emulation.

Adventure doesn't use all the facilities of Fortran formatting I/O, but some of the trickier-to-implement aspects are used, sometimes just in one spot.

### Somewhat Nonstandard Fortran

#### Fixed Layout with Tabs

In this era, Fortran required programs to use the fixed layout, with specific column positions reserved for statement numbers and continuation line indicators.  The DEC compiler offered some flexibility to use tab characters, which the Adventure code took advantage of.  The earlier programmer's guide described only part of it, making it seem as though the Adventure code was just plain wrong.  But the later guide gave a more complete picture, so the translator's line handling now works according to that description, and no exceptions are needed for parsing the Adventure code.

#### DATA Initialization in COMMON Blocks

### Core Images

A core image is a snapshot of a process's memory space saved to disk.  The image could be loaded again later, allowing a process to resume after an interruption.

Adventure relied the user's ability to create a core image from the game.

#### Hours of operation

As written, Adventure initializes itself from scratch, pauses until the user acknowledges that initialization is complete, and then begins the game.

As I understand it, the idea is that a system administrator could run it once to initialize all the memory, and then save a snapshot of memory (a core image) while it's paused.  The core image would then be made available to the users of the system in leui of the compiled program.  When the user starts the core image, the game would be ready to play.  This also eliminates the need for the users to have access to a separate data file.

Furthermore, the administrator could, before saving the image, enter a "wizard" mode to customize a welcome message and to set the hours that users may play.  For example, a company might restrict playing during business hours while allowing their employees to play the game in the evening or weekends and holidays.

(As the player gets close to the final puzzle, they will receive notices about the cave closing.  This is a feature of the game, distinct from actual restrictions on hours of play.)

I'm not sure there's a good way for the Fortran translator to offer this functionality, at least, no without changing the Adventure source code.  An interpreter could emulate a snapshot feature that works this way, so I'll keep that in mind for the future.

Nonetheless, even without the ability to tailor an installation, I didn't want to lose the interface for doing so.  Thus, unlike most ports, the translated game has wizard mode.

#### Saved games

Core images were also used to save the state of play so that an adventurer could play again later and pick up where they left off.

Again an interpreter could have provided the functionality, but I've written a translator.


### Notes

1. As I've worked on this project, I've learned that some of ports have been updated, fixing at least some of the telltale bugs I'm used to spotting in the ports.  So it's possible that there may be one out there that is a faithful port.  At this point, I don't care.  I'm having fun.

2. A couple weeks into this project, I came across a [project](https://github.com/swenson/adventwure/tree/main) that uses a PDP-10 Fortran IV interpreter written in Python, but I'm not sure it works with the version I consider canonical.  That was written long before I decided to make an interpreter.  Its existence was part of the impetus to switch from making an interpreter to a translator.

## Where it Went Wrong

As I worked on the project, I mostly had to study the produced C code to determine whether it was right.  (The generated code isn't particularly easy to read.)  Even with stubs in place, the translated code could not compile until I reached "feature complete."  One or two variables or statement labels were undefined until the last stub was replaced.

To my delight, it then compiled and linked.  I had expected that there would be more problems to fix before it got that far.  I'd been using a batch file to run the translator, and then--if it succeeded--compile and link the translated code.  I'd forgotten that it would then execute the code if the link succeeded, so I was shocked when the program just started running.

Of course, it crashed almost immediately after printing the 'INITIALIZING...' message.

So far, debugging has been surprisingly easy.  Perhaps because all the details are fresh in my head.

I (temporarily) modified basic_statement to insert a print command showing the statement numbers whenever a statement with a number is about to be executed.  This has been extremely useful in quickly narrowing down where a bug occurs, since the statement numbers are easily correlated between the Fortran source and the translated C.

Here's a log of the bugs I had to fix.

### DO loop index variables

For some reason, I'd made the translator turn DO loops into for-loops using a private index variable whose value would be copied into the variable intended to be used.  I'm not sure whether that actually caused a problem, but it's certainly possible.  If the body of a loop alters the index, that could change the iteration order.  Anyway, that was easy to fix.  (Though there's now some obsolete code in the the translator I could delete.)

### Opening the data file

Of course the OPEN wasn't working because it couldn't find the data file.  For now, I'm testing by first setting the current directory to the one with the file.

But it also needed an extension.  The translator now slaps ".DAT" onto the end of the file name.

I'm on Windows with a filesystem that's not sensitive to case:  `ADVENT.DAT` will match `advent.dat`.  On most other filesystems, this will be a problem.  The file name is hardcoded in the Adventure source, which I don't want to change.  I'm considering having options cooked into the translated program so that the user could provide a mapping between the requested file name and the file path.

With this fix in, the initialization code processed the first three sections of the data file.

### Illogical return type

The translated code uses a bunch of utility functions for papering over some of the differences in the language.  In particular, there's `truth` and `logical` for converting between LOGICAL Fortran values and C bools.  I messed up the return type on `logical`, so it wasn't doing the right thing.

With this fix in, two or three sections of the data file were read in before it jumped to Adventure's BUG subroutine to report a problem with the data file.

### Uninitialized COMMON variables

Adventure uses a DATA statement to initialize at least one variable in a COMMON block.

Although the translator had been faithfully including those initializations until I changed DATA statements to use a variable list rather than an io list.  Without that initialization, the code thinks a table is full before it puts the first item into it.

Some of the DEC documentation says initialization of COMMON can be done only in a BLOCK DATA subprogram, and my parser had tried to enforce that.  But since the Adventure code was violating the rule, I'd commented out the enforcement.

Once I restored the initialization, the entire data file was processed and Adventure spit out the summary!

```
TABLE SPACE USED:
  9616 OF   9650 WORDS OF MESSAGES
   742 OF    750 TRAVEL OPTIONS
   296 OF    300 VOCABULARY WORDS
   140 OF    150 LOCATIONS
    53 OF    100 OBJECTS
    31 OF     35 ACTION VERBS
   201 OF    205 RTEXT MESSAGES
    10 OF     12 CLASS MESSAGES
     9 OF     20 HINTS
    32 OF     35 MAGIC MESSAGES
```

### Adventure has a buffer underrun?

The next crash had indications that a variable (or the table entry it was copied from) had been stomped, presumably by an out-of-bounds access.

Indeed, I found a buffer underrun, but it wasn't a translator bug--it's cooked right into Adventure's initialization:

```fortran
1033  TRAVEL(TRVS-1)=-TRAVEL(TRVS-1)
```

On the first iteration of the loop that contains that line, `TRVS` is 1, so `TRAVEL(TRVS-1)` is `TRAVEL(0)`.  Unfortunately the `TRAVEL` array is 1-based, so this flips the sign of the machine word that precedes the array.

Even classic software has bugs.

This is _not_ the cause of the crash I was chasing.  In my case, the memory that was stomped was padding between variables (which, I believe, the compiler inserted for alignment), so it had no ill effect.

If there hadn't been padding, it would have (in my case) nailed the last word in the `HINTED` array, which is initialized afterwards, so the damage would still have gone unnoticed.  Even if `HINTED` had already been intialized, it would have been set to `.FALSE.`, which is zero, so negating it wouldn't have changed its value.

Anyway, I won't change the Adventure code, and I don't see a simple way to fix this in the translator.  Had I written an interpretter, I'd like to think I would have baked bounds-checking into the expression evaluator.  Fortran IV, like C, is not a memory safe language.

So I'm just keeping in mind that Adventure has at least one out-of-bounds access as I pursue any other bugs.

### More uninitialized variables

The bug I'd been chasing was actually caused by the assumption that all variables (even locals) are initialized to zero if they're not explicitly initialized to something else.  Maybe Fortran guarantees that, or maybe it was an implementation feature.  Adventure does explicitly zero out some values upon initialization.

In my case, the variables were initialized to 0xCCCCCCCC'CCCCCCCC, a magic number MSVC uses in debug builds to help identifier uninitialized local variables.  (The smashed number I saw did not have that value, because several operations had been performed on it by the point I was detecting the problem.)

So that was a trivial fix.

### Bitwise logic

The translator was attempting a logical NOT operation on a crazy constant.  So it was pretty clear that should have been a _bitwise_ NOT.  And, indeed, the program got farther when I switched it.

So I'm now treating all of these logical operators as bitwise:  `.AND.`, `.OR.`, `.NOT.`, `.XOR.`.  I don't understand `.EQV.` well enough to make a change to that yet, and Adventure doesn't use it.

Conditions in IF-statements are always translated with a `truth` wrapper, so it would be pretty unusual for this to cause a problem.  If it does, I could check the symbol table when parsing the expression and select the logical operators when the operands are LOGICAL.

Achievement Unlocked:  `WELCOME TO ADVENTURE!!  WOULD YOU LIKE INSTRUCTIONS?`

### Y2K?

At the same point I was having problems with the logical NOT operator, there was also a negative value generated for the year because 2025 is truncated to 25 and the program subtracts 77 (from 1977).  That's (apparently) not a deal breaker, yet.

I believe I can muck with my implementation of the DATE function to provide non-digits for the year that would cause Adventure to produce positive values over a hundred.  That would get us to at least 2077 without changing the Adventure source code.  But I'll keep that in my pocket for now.

### Signs of an extended problem

Representing 36-bit machine words with 64-bit integers requires special attention to sign extension, as mentioned in the portion about A5 text.  Despite lots of planning to deal with that for packed character data, there are places where Adventure uses octal constants to represent bitmasks, and those were not extended.  So I added a constructor for the translator's constant_t that does sign extension on the value type.  That should help with all constants.

I didn't expect that to totally solve the problem, but it does solve the couple of repro cases I encountered that caused the parser to sometimes not recognize the second word in a two-word command.

### X marks the spot

I hadn't implemented the 'X' specifications for FORMAT.  They are used to specify the carriage control in most of the FORMATs in the maintenance mode.  It's almost as if a different person wrote that part of the code.  Anyway, implementing 'X' involved some rewriting of `io_output`, which, I think, solved another problem I hadn't noted before.  Anyway, the challenge word to prove your a wizard is now printed.

### More sign extension woes

I had trouble passing the wizard challenge, which I thought was due to some failures to do the sign extension.  While there were some cases where the generated code failed to do sign extension, that may not have been the cause of my problem.

I made a spreadsheet to generate the correct response from the wizard challenge.

I've now played a couple hours with only one incident:  Adventure detected a problem in the travel table as I tried to move among the rooms West End, East End, and High N/S crossover of Low E/W passage.  It reported the problem (error 28), and ended the program gracefully.  I don't remember the exact command sequence, and I haven't been able to reproduce the problem.  I suspect this may have been been a problem with the temporaries in the circular buffer.  See the very next subsection.

### Random bugs randomly

I started to wonder why I rarely encountered a dwarf in the game.  I did a little debugging and found two bugs caused by my translator.

First, Adventure's random number generator was computing a fresh seed value each time it was called.  It expected the local variable with its state to be preserved from call-to-call.  I would have thought it would use COMMON for that, but it doesn't.  I concluded that all local variables should, in C terms, be function static.  I guess that's why Fortran IV doesn't allow recursion.  Anyway, it was a simple fix for the translator.

But it didn't really explain why the dwarves weren't being activated.  As I was verifying the static locals fix, I saw that the initial invocation of RAN was not producing a value in the requested range.

This second bug was that my circular buffer for passing constants and temporaries by reference was overflowing.  Upon the first invocation, RAN runs a loop a many times (the exact number depends on the date, which was 1600-1700 times when I was debugging).  Each loop iteration burned a couple temp slots.  Since the variable holding the requested range was also in a temp slot, it was overwritten many times.  Knowing that the call depth was very limited, I had thought 256 temp slots would have been more than sufficient for holding a handful of values for the duration of the evaluation of any expression.  I hadn't thought through the loops, though.  I solved this by turning the buffer into a stack, and wrapping most expression
evaluations that pushes and pops frames.

## Now that It's Right

I'm completely delighted that the tranlastor produces a working, playable version of the game that doesn't have the bugs that I'm used to seeing in other ports (like the inability to follow the stream to the grate at the beginning of the game).

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
