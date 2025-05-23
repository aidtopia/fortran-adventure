## Peculiar Hazards

Here we look at the peculiar features of the Fortran IV implementation used by 1970s versions of Adventure, and how the translator could deal with them.

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
| :-------: | ---------------: |
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

### FORMATted I/O

This was more challenging than I expected, and there are a couple behaviors I had to guess about.  I got some additional info on retrocomputing.stackexchange.com.  But it's possible there are lurking bugs due to imperfect emulation.

Adventure doesn't use all the facilities of Fortran formatting I/O, but some of the trickier-to-implement aspects are used, sometimes just in one spot.

### Somewhat Nonstandard Fortran

#### Fixed layout with tabs

In this era, Fortran required programs to use the fixed layout, with specific column positions reserved for statement numbers and continuation line indicators.  The DEC compiler offered some flexibility to use tab characters, which the Adventure code took advantage of.  The earlier programmer's guide described only part of it, making it seem as though the Adventure code was just plain wrong.  But the later guide gave a more complete picture, so the translator's line handling now works according to that description, and no exceptions are needed for parsing the Adventure code.

#### DATA initialization in COMMON blocks

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

That said, I'm considering some changes that might make it feasible to snapshot all the data, which might make it possible to emulate a core dump.

---

With all that planning, the project progressed amazingly quickly and smoothly, so you might be surprised to read [Where it Went Wrong](wentwrong.md).
