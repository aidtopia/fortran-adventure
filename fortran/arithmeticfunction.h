#ifndef AID_FORTRAN_ARITHMETICFUNCTION_H
#define AID_FORTRAN_ARITHMETICFUNCTION_H

#include "expression.h"
#include "symbols.h"

namespace aid::fortran {

// A Fortran arithmetic function is essentially a function-shaped macro defined
// within a subprogram.  It's scoped to the containing subprogram and has access
// to other symbols defined within the subprogram.  It must be instantiated at
// each invocation.

struct arithmetic_function_t {
    symbol_name name;
    parameter_list_t params;
    expression_t definition;
};

}

#endif
