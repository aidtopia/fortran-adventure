## Statement Function Parameters

Fortran statement functions were causing the C code to contain declarations for unused variables.

### The Problem

The problem arises from the parameters used in the definition of the statement function.  Although statement functions are similar to `FUNCTION` subprograms, the implementation treats them more as a macro defined in the current subprogram.  In fact, the translator converts them to function-style C preprocessor macros.

Here's an example from Adventure:

```fortran
      LIQ2(PBOTL)=(1-PBOTL)*WATER+(PBOTL/2)*(WATER+OIL)
```

A side effect of parsing an expression, such as the one to the right of the equals sign, is that any unrecognized identifier in the expression is added to the symbol table.  Since Fortran doesn't require variables or even subprograms to be declared before they're referenced, adding them as they're encountered avoids two-pass compilation.

When the expression parser saw `PBOTL` on the right side, it ensured that `PBOTL` was in the symbol table.  Without other clues, the symbol is set up as a local variable.  The type would later be inferred per the implicit rules.

The translator doesn't define unreferenced variables in the C code, but that doesn't help here.  `PBOTL` _is_ referenced in the expression that defines the statement function.

As a result, when the translator generated the code for the subprogram that contained this definition, it included `vPOTBL` as a local variable.  The C compiler recognizes that it was defined but never referenced and emits a warning.

(Until recently, the compiler wasn't warning about these because the defined-but-unreferenced variables were not initialized.  But recent changes resurfaced the warnings, because now the variables are represented in the C code as pointers initialized to a particular location in the memory array.)

Not every parameter of every statement function resulted in a warning.  In fact, most didn't.  Here's another example from Adventure:

```fortran
      TOTING(OBJ)=PLACE(OBJ).EQ.-1
```

In this case, `OBJ` did not result in an unreferenced variable because `OBJ` is _also_ a local variable in the subprogram.  `OBJ` is essentially two different symbols from two scopes:  the subprogram and the statement function definition.  The statement-function `OBJ` shadows the `OBJ` local variable.  The symbol table cannot represent shadowing, so these were conflated to a single symbol.  The generated code works because the `OBJ` in the resulting macro is just a placeholder that's replaced textually, so there's no actual shadowing or aliasing.

Although the effect on the translation of Adventure is harmless, the C compiler warnings point to a lurking problem.  If the statement function parameter name collides with anything other than a scalar local, translation might fail altogether.

### Rejected Solutions

I considered and dismissed these solutions:

* Generalize the symbol table to handle nested scopes.  This would be a very general approach to a very narrow problem.  For the versions of Fortran used in the Adventure code, statement function definition parameter names are the only symbols that can shadow others.  In theory the shadowing could cause significant problems (e.g., if the shadowed local symbol represented an array or subprogram).  But in the Adventure code, the only problem is a couple warnings from the C compiler.

* Fake nested scopes at the unit level.  Currently, each subprogram is represented as a unit, and the unit for the main subprogram contains the units for other `FUNCTION` and `SUBROUTINE` subprograms.  This is largely a convient way to bundle an entire program together.  If any unit could contain subunits, we could possibly fake nested scopes.

* Translate statement function definitions into separate subprograms.  I'm not sure how this would work, since the statement functions reference symbols in the containing subprogram (like `WATER`, `OIL`, and `PLACE` in the examples above).  Even if I implemented them as closures (capturing the necessary variables and subprograms by reference), I'd then face shadowing and aliasing at the subprogram level because two different subprograms might define two different versions of `LIQ2`.  That's solvable, but it feels like just moving the problem to a different place.

### The Hack

For now, I've:

* Extended `symbolkind` with `fake` to represent a function statement parameter name.

* Extended `symbol_table` with a `remove` method that removes a symbol from its index.

* Changed `parse_statement_function_definition` to:
  
  * Add fake symbols for (most) parameters immediately before parsing the expression

  * "Remove" the fake symbols immediately after parsing the expression.

While that does eliminate the unreferenced variable warnings from the C compiler, I'm not happy about any of those changes.

* `fake` is a terrible name.

* `symbol_table::remove` removes the symbol from its internal _index_ but not from the table itself.

  As a result, dumps of the symbol table show multiple instances of the same name, with some identified as fakes.
  
  And I'm already grumpy about `symbol_table`'s clumsy interface, so I don't like having to add functionality for a very specific case.

* I cannot add a fake symbol for a parameter if there's already a symbol with that name.

  That's the case where the shadow is actually an alias of the wrong thing.  And while this harmless for Adventure, all I've done is eliminate the warning that points toward the lurking aliasing bug.

* The add-parse-remove dance cries out for RAII.

The need for add-parse-remove suggests tight-coupling problems in the parser.

### Hack 2

I iterated on the hack like this:

* Give `symbolkind::fake` a better name.  Using `fsparam` now.

* Remove `symbol_table::remove` restoring `symbol_table` to its previous design.  Well, it does need a `clear` method now.

* Extend `unit` to have its own table of shadow variables.  Modify the `unit` methods that are normally delegated to the symbol table to first check the shadow table.  We don't actually need (or want) to intercept every access of the symbol table, just certain ones.

* Broke out `parser::parse_statement_function_expression` to make clear that the parameters are added and then immediately removed.

This solves the C compiler warnings but also eliminates the lurking risk of aliasing, as the symbol for the parameter is distinct from any other symbol in the table.
