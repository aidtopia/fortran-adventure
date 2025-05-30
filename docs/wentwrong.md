## Where it Went Wrong

As I worked on the project, I mostly had to study the produced C code to determine whether it was right.  (The generated code isn't particularly easy to read.)  Even with stubs in place, the translated code could not compile until I reached "feature complete."  One or two variables or statement labels were undefined until the last stub was replaced.

To my delight, it then compiled and linked.  I had expected that there would be more problems to fix before it got that far.  I'd been using a batch file to run the translator, and then&mdash;if it succeeded&mdash;compile and link the translated code.  I'd forgotten that it would then execute the code if the link succeeded, so I was shocked when the program just started running.

Of course, it crashed almost immediately after printing the 'INITIALIZING...' message.

So far, debugging has been surprisingly easy.  Perhaps because all the details are fresh in my head.

I (temporarily) modified basic_statement to insert a print command showing the statement numbers whenever a statement with a number is about to be executed.  This has been extremely useful in quickly narrowing down where a bug occurs, since the statement numbers are easily correlated between the Fortran source and the translated C.

Here's a log of the bugs I had to find and fix.

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

[**UPDATE**: The Y2K fix is in and enabled by default.  It can be disabled with the command line option `-y2k-`.  There are also command line options to override the date, the time, or both.]

### Signs of an extended problem

Representing 36-bit machine words with 64-bit integers requires special attention to sign extension, as mentioned in the portion about A5 text.  Despite lots of planning to deal with that for packed character data, there are places where Adventure uses octal constants to represent bitmasks, and those were not extended.  So I added a constructor for the translator's constant_t that does sign extension on the value type.  That should help with all constants.

I didn't expect that to totally solve the problem, but it does solve the couple of repro cases I encountered that caused the parser to sometimes not recognize the second word in a two-word command.

### X marks the spot

I hadn't implemented the 'X' specifications for FORMAT.  They are used to specify the carriage control in most of the FORMATs in the maintenance mode.  It's almost as if a different person wrote that part of the code.  Anyway, implementing 'X' involved some rewriting of `io_output`, which, I think, solved another problem I hadn't noted before.  Anyway, the challenge word to prove you're a wizard now appears.

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

---

And just like that. I was able to play Adventure just as I remembered it.

So what's next [now that it's right](rightnow.md)?
