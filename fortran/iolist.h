#ifndef AID_FORTRAN_IOLIST_H
#define AID_FORTRAN_IOLIST_H

#include "indexcontrol.h"
#include "expressions.h"
#include "symbols.h"

#include <vector>

namespace aid::fortran {

// Input-output lists are used in DATA statements and formatted i/o.

struct io_list_item {
    // In an input operation or DATA statement, `variable` generates a reference
    // to the variable to be set.  In an output operation, `variable` generates
    // a reference to the variable holding the value to be output.
    expression_t variable;

    // If `variable` references a scalar (or a specific element of an array),
    // the index_control is not used.
    //
    // In the general case, there could be nested index controls for each
    // dimension of an array.  But Adventure has just one instance of explicit
    // index control, and that's for a single dimension.
    index_control_t index_control;
};

using io_list_t = std::vector<io_list_item>;

// DATA statements use a restricted form of i/o lists, but trying to use the
// general io_list_t for DATA was more trouble than it was worth.  Besides,
// Adventure's DATA statements don't even use all of the restricted form.
using variable_list_t = std::vector<symbol_name>;

}

#endif
