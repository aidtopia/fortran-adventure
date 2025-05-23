# Fortran IV Features

Adventure uses a simple subset of Fortran IV.

For example, Adventure does not use REAL, COMPLEX, or DOUBLE PRECISION values.
It does use LITERALs, but only short ones that fit into a machine word and are
interchangeable with INTEGERs.  LITERALs are specified with the simpler-to-parse
quotes and not with Hollerith records.

All arguments are passed by reference.  There's no recursion.  All but one array
is one-dimensional, and the outlier is a simple two-dimensional one.  All
dimensions are 1-based.

Many statement types never occur.  Other are used only in their simplest forms.
For example, DATA statements in Adventure don't use index controls.

Here I'm tracking the Fortran IV features that must be implemented to run
Adventure.  The list evolves as I learn more about the code, but I think this is
pretty close to complete.  Checked items have been implemented.

- Source File Handling
  - [x] blank lines
  - [x] comment lines
  - [x] form feeds
  - [x] non-standard use of horizontal tabs
  - [x] initial lines
  - [x] continuation lines
  - [x] nearly all blanks (spaces) in source line are irrelevant

- Data Types
  - [x] INTEGER
  - [x] LOGICAL
  - [x] LITERAL (text)

- Constants
  - [x] integer constant
  - [x] octal constant
  - [x] logical constant
  - [x] literal constant (i.e., text)

- Other Building Blocks
  - [x] symbol table
  - [x] input output lists
  - [x] data lists
  - [x] common data blocks
  - [x] expressions
    - [x] arithmetic
    - [x] comparisons (some issues with bool != LOGICAL)
    - [x] .AND.
    - [x] .OR.
    - [x] .XOR.  (.EQV. is not needed for Adventure)

- Statement Types
  - Non-executable Statements
    - Specification Statements
      - Storage Specification Statements
        - [x] COMMON
        - [x] DIMENSION
        - [x] EXTERNAL
      - Data Specification Statements
        - [x] DATA
      - Type Specification Statements
        - [x] IMPLICIT
        - [x] INTEGER
        - [x] LOGICAL
      - Format Specification Statements
        - [x] FORMAT
    - Subprogram Statements
      - [x] function definition statements [preprocessor macros for the win!]
      - [x] FUNCTION
      - [x] SUBROUTINE
      - [x] END
  - Executable Statements
    - Assignment
      - [x] to scalar
      - [x] to array subscript
      - [x] from constant
      - [x] arithmetic expression
      - [x] logical expression
    - Control Statements
      - [x] CALL
      - [x] CONTINUE
      - [x] DO
      - [x] GO TO
        - [x] unconditional
        - [x] computed
      - [x] IF
        - [x] logical
        - [x] numeric [Adventure uses just one]
      - [x] PAUSE
      - [x] RETURN
      - [x] STOP
    - I/O Statements
      - [x] ACCEPT
      - [x] OPEN
      - [x] READ
      - [x] TYPE
