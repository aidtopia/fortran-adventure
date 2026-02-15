# ASA Carriage Control

Fortran initially developed when punched cards were the primary input method and line printers were the primary output devices.  To print a line, the text would be sent to the printer driver, which would generate the necessary signals to make the printer print those characters.  To print another line of text, the driver had to generate additional signals to return the carriage (a.k.a., the print head) to the first column and advance the paper to the next line.

The convention for early versions of Fortran (and Cobol and PL/I) was to send an extra "carriage control" character before each text record that told the driver the spacing to apply before printing the text.  These control characters were regular characters, like '1' or '+' or (most commonly) a space, but they were interpretted by the printer driver as a control character rather that a printable one.

Here's an example from Adventure:

>```fortran
>67    FORMAT(/' THERE ARE ',I1,' THREATENING LITTLE DWARVES IN THE'
>     1 ,' ROOM WITH YOU.')
>```

The format begins with `/`, which prints an empty record, forcing a blank line.  Notice that the text field that follows begins with a space.  That space is the first character of a record, so it's the carriage control character.  It simply means to move the paper to the next line.  The output will begin with "`THERE`", not "&nbsp;`THERE`".

Note that these carriage control characters specified how far to move the print position down the page, they did _not_ affect the horizontal position of the print head.  When the end of the record was reached, the printer driver knew to reset the horizontal print position to the first column.

The American Standards Association (ASA) (an earlier name for the American National Standards Institute) promulgated a standard for carriage control characters to ensure that various Fortran implementations (and those of other languages) used a common set.  It mostly tried to codify existing practice.

The first draft of ASCII arrived shortly thereafter.  It reserved a range of character values for special controls.  Among those were carriage return (CR), line feed (LF), form feed (FF), and others.

A carriage return (CR) returns the print head or cursor to the first column of the current line.  A line feed (LF) advances to the next line without changing the horizonal position of the cursor.

Today, we think of a CR-LF sequence as a line _terminator_.  Unix and its descendents treat LF as "new line" character that has the combined behavior of CR and the traditional LF.  On those systems, lines are terminated with just LF.  A Unix driver for a line printer or teletype would inject a CR in the stream to the printer just before an LF.

In Adventure, however, the pattern was different.

* Text records _end_ with a CR.

* The _first_ character of a text record determines how far to advance the print position vertically _before_ printing the rest of the text.

So, in practice, a printer will typically receive one or more LFs _before_ the line and a CR _after_ it.  When printing several consecutive lines, the difference between LF-*text*-CR and *text*-CR-LF is unimportant.

But when output and input are interleaved, as with a teletype or video display terminal, the ordering matters.

## The Problem

Adventure has a baked-in hack to make the vertical spacing between interleaved output and input attractive during game play (and paper-saving in maintenance mode).  But that hack is predicated on the old fashioned ASA carriage control.  If we try to use the modern *text*-CR-LF pattern, or even CR-LF-*text*, for output, the vertical spacing where input meets output becomes a mess.

Any human porting Adventure to a modern system would simply throw out Adventure's hack and use the regular input and output facilities of the target system.  But our goal is to recreate the original behavior by mechanically translating all of the original unmodified Fortran source code.

Our translator provides the Fortran services that Adventure relies on.  To make Adventure work right, the i/o subsystem must emulate ASA carriage control.

That's a simple matter of sending CR and LF at the right moments.  `<foreshadowing>`How hard can it be?`</foreshadowing>`

### Newlines

In C, file streams can be opened in either text or binary modes.  Sending a newline (`\n`) to a file stream in text mode will cause the local system's line terminator to be emitted.  For Windows that means CR-LF.  Likewise, when reading from a file stream in text mode, the pair of characters CR-LF will be read as a single `\n`.

The pitfall is that `\n` uses the same character code as LF.  So if you try to write a bare LF, the output actually receives CRLF.  And if you send CR-LF, the output gets CR-CR-LF.  Likewise, you cannot read a bare LF in text mode.

The solution is simply to open the streams in binary mode, which doesn't do any translation of line terminators.  Unfortunately, `stdout` and `stdin` are pre-opened in text mode before `main` is called, and there's no standard and portable way in C to re-open `stdout` and `stdin`.

On Windows, we resort to a Windows-specific API called `_setmode`.

On Posix systems, we don't bother since there is no difference between text and binary modes.

### Input Echoing and Line Editors

Suppose Adventure is waiting for the user to enter a command, and they type `T`.  The system can read that character from `stdin`, but the user doesn't see the `T` on the teletype or screen.  To provide that feedback, the character is sent back out to `stdout`.  This is called input echoing.

For an even better user experience, there's usually a line editor that not only echos the input back to the user, but also allows them to edit it with backspace, arrow keys, insertion, deletion, etc.

Typically the line editor is part of the terminal program.  When the application reads from `stdin`, the terminal program's line editor steps in and holds the user's text in a buffer until they press Enter.  At that point the entire line become available to the application until it has all been read.

Under ASA carriage control, the cursor is positioned at the first column of the last line of the message.  When the user types, the line editor's echos would overwrite the beginning of last line of the message.  This is where Adventure's hack is handle.  During gameplay, Adventure prints a blank line just before accepting user input.

Now the player types their command and presses the enter key.  The "enter" key on their teletype keyboard is likely labeled Return (or even Carriage Return) because it generates a CR.  The CR is as appened to the line buffer and echoed back to the output.  The echo moves the print position to the first column but it does not advance to the next line.

When the response is printed, it will start with a LF, so it won't overwrite the user's command on the teletype.  Will Crowther or Don Woods wanted a little vertical space there, so Adventure also prints a blank link before displaying a message.

Now here's the problem:  Modern line editors typically echo the Enter key with CR-LF (or equivalent).  They leave the print position at the beginning of the _next_ line.  That breaks Adventure's expectation.  When it issues the blank line to create a little extra space after the user input, it ends up creating two blank lines.  That's a behavior change.

A quick hack to solve this would be to issue a cursor up immediately after reading a line of user input.  That's not quite the same though.  It wouldn't work if `stdout` were directed to a file or other program to capture a transcript.  It wouldn't be possible on most hardcopy terminals.  And it won't unscroll on a video terminal (or windowed terminal emulator), so the user might lose a line of lookback that the actual program would not have taken away.

Since I'm committed to not modifying the Adventure source code, I had to implement my own line editor in the translated program.

First, I had to disable the system- or terminal-provided line editor and echoing.

On Windows, that's accomplished with another Windows-specific API called SetConsoleMode.

On Posix, that's TBD.

Next, I had to read characters from stdin, buffer them and echo them to stdout.  I included a little bit of line editing functionality, with left and right arrows, backspace, delete, insert, and overstrike.  I used a couple non-standard but commonly supported escape sequences to maintain the cursor position when the rest of the line needs to be updated.

#### Windows

That _almost_ worked on Windows.  The problem what that when the user pressed Enter, the console host held generation of the CR until the user typed another character.  In the end, I had to switch from a standard what to read an input character (like `fgetc()` or `getchar()`) to a Windows-specific API called `_getch()` in order to avoid delay of the deliver of the CR.

Switching to `_getch()` required additional work because (1) `_getch()` returns editing keys (like arrows and Delete) using nonstandard sequences rather than the ANSI escape sequences, and (2) `_getch()` only reads from the keyboard, even if `stdin` is redirected from a file or pipe.

Note that `_getch()` doesn't not support UTF-8 or other Unicode transfer formats.  That's not a problem for Adventure.

#### Posix

I believe the custom line editor should work on _Posix_ systems, but I haven't tested it.

There's likely some additional work to bypass the terminal emulator's own line editor and input echoing and to ensure that it doesn't inject CR before a LF.
