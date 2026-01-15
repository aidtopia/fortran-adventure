## How to Try It Out

I recognize this looks a bit cumbersome.  I have plans to streamline the process.

### 1. Clone the repository

```
git clone https://github.com/aidtopia/fortran-adventure.git
cd fortran-adventure
```

### 2. Build the translator

Choose your own path: [Windows with Visual Studio](#windows-with-visual-studio) or [Another system](#another-system).

- - -

#### Windows with Visual Studio

Start a [VS Developer Command Prompt](https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022).  A Developer Command Prompt is just a regular command prompt with PATH and other environment environment variables set to access the compilers and tools that come with Visual Studio.

You can build the translator using the IDE...

```
start fortran\fortran.sln
```

**or** you can use MSBUILD like this:

```
msbuild fortran\fortran.sln -p:Configuration=Release
```

- - -

#### Another system

If you use another toolchain, like clang or gcc, you'll have to cobble together a script or makefile to compile and link all of the C++ files in the `fortran` subdirectory.

**NOTE:**  The translator uses some features from C++23.  For gcc and clang, make sure you're using a fairly recent version and specify `-std:c++23` among the options on the compiler command line.

- - -

### 3. Get the Adventure code

I haven't included Adventure sources in this repository, so you'll have to download them.  I suggest starting with the aforementioned [Adventure Family Tree](https://mipmip.org/advfamily/advfamily.html) and using its links to the [Interactive Fiction Database](https://ifdb.org/).

The example below downloads the WOOD0350v2 version, which is a good one to start with.

```
curl -so advent-original.tar.gz http://ifarchive.org/if-archive/games/source/advent-original.tar.gz
```

That file is an archive that bundles together a few files; we need to extract them:

```
tar -xf advent-original.tar.gz
```

The extracted files are placed in a subdirectory called `advent`, which contains the source and data file.  I suggest renaming the directory.

```
rename advent WOOD0350v2
```

### 4. Run the translator

In the directory with the Adventure sources, run the translator (`fortran`), and the name(s) of the source file(s).

```
cd WOOD0350v2
..\fortran\x64\Release\fortran.exe advent.for
```

### 5. Compile the translated program

The translator should have created a subdirectory called `target`, which will contain the C source file and a dump of the symbol table.

Compile the C code with your favorite compiler.  The example below uses `cl` (which is the MS Visual C compiler includes in Visual Studio).  The generated C code should be compliant with the C11 standard and thus very portable.  Any competent C compiler should do.

```
cd target
cl /nologo /std:c11 /W4 /Od /ZI ADVENT.c
```

### 6. Run Adventure!

Make sure Adventure's data file is in the current directory, and start the program you just built.

```
cd ..
target\ADVENT.exe
```

### 7. Provide a data file mapping

If you've followed this example, the program should begin and you can skip to [step 8](#8-go-already).

If, however, you're working with a different version of Adventure, the program might say it was unable to open the data file and then exit.  For example, if you were using WOOD0350v**1**, you'd see:

> ```
> INITIALISING...
> The program failed to open a file named "TEXT".
> You can restart the program with a file name mapping using the
> command line option -f, like this:
> 
>     -fTEXT=<path>
> ```

In this case, you'd run it again like this:

```
target\ADVEN.exe -fTEXT=adven.dat
```

### 8. Go already!

You should see messages about initialization and loading the data file, followed by `INIT DONE`.

> ```
> INITIALIZING...
> TABLE SPACE USED:
>   9616 OF   9650 WORDS OF MESSAGES
>    742 OF    750 TRAVEL OPTIONS
>    296 OF    300 VOCABULARY WORDS
>    140 OF    150 LOCATIONS
>     53 OF    100 OBJECTS
>     31 OF     35 ACTION VERBS
>    201 OF    205 RTEXT MESSAGES
>     10 OF     12 CLASS MESSAGES
>      9 OF     20 HINTS
>     32 OF     35 MAGIC MESSAGES
> 
> 
> INIT DONE
> ```

At this point, the program is paused, for reasons that made sense when you were using a PDP-10 in the 1970s.  Type `G` (for go) or `X` (for exit) and press return (a.k.a., enter).

```
G
```

### 9. Prove you're a wizard ... or not

If you start Adventure during business hours, you may be told that the cave is closed.

> ```
> I'M TERRIBLY SORRY, BUT COLOSSAL CAVE IS CLOSED.  OUR HOURS ARE:
> 
>          MON - FRI:   0:00 TO  8:00
>                      18:00 TO 24:00
>          SAT - SUN:  OPEN ALL DAY
>          HOLIDAYS:   OPEN ALL DAY
> ```

Most novice adventurers will want to use the [time travel option](#time-travel-option).

If time travel feels like cheating, you'll have to [prove you're a wizard](#wizard-option).

- - -

#### Time Travel Option

The translator added some command line options to the program that can be used to work around compatibility problems and a Y2K bug.  These options can be used to override the date and time that Adventure sees when it asks for the system time.  I suggest setting the date to midnight, January 1, 1977, like this:

```
target\ADVENT.exe -t0:00 -d1-JAN-1977
```

There are several advantages to this approach:

* The cave will be open (because it's midnight on a Sunday).
* It avoids Y2K bugs.
* It simplifies the wizard challenge if you choose to try out the administrator options.
* It seeds the random number generator consistently.

- - -

#### Wizard Option

The wizard.xlsx Excel spreadsheet in the `tools` directory has a synopsis of the answers you must give and it will compute the necessary response to the final challenge.

In the following examples, the user's responses are in lowercase.

> ```
> ONLY WIZARDS ARE PERMITTED WITHIN THE CAVE RIGHT NOW.
> 
> ARE YOU A WIZARD?
> 
> yes
> 
> PROVE IT!  SAY THE MAGIC WORD!
> 
> dwarf
> 
> THAT IS NOT WHAT I THOUGHT IT WAS.  DO YOU KNOW WHAT I THOUGHT IT WAS?
> 
> no
> 
> IFXOU
> ```

In the above example, the `IFXOU` is a randomly selected challenge word.  The user must apply an algorithm to the challenge word to generate the correct response word.  The spreadsheet does the work for you.

![wizard spreadsheet in action](wizard_challenge.png)

> ```
> dsphm
> 
> OH DEAR, YOU REALLY *ARE* A WIZARD!  SORRY TO HAVE BOTHERED YOU . . .
> ```

By the way, the `11111` shown under `MAGNUM` is the default "magic number" coded into the program.  If you ever manage to change that number (not currently possible without changing the source code), you just have to put the new magic number in that spot on the spreadsheet.

- - -

### 10. Welcome to Adventure!!

Be aware that it is not yet possible to save your game state.  I'm working on that.  In the meantime, don't blame me if a dwarf knifes you.

> ```
> WELCOME TO ADVENTURE!!  WOULD YOU LIKE INSTRUCTIONS?
> 
> no
> 
> YOU ARE STANDING AT THE END OF A ROAD BEFORE A SMALL BRICK BUILDING.
> AROUND YOU IS A FOREST.  A SMALL STREAM FLOWS OUT OF THE BUILDING AND
> DOWN A GULLY.
> 
> enter building
> 
> YOU ARE INSIDE A BUILDING, A WELL HOUSE FOR A LARGE SPRING.
> ```
