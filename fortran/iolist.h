#ifndef AID_FORTRAN_IOLIST_H
#define AID_FORTRAN_IOLIST_H

#include "indexcontrol.h"
#include "expressions.h"
#include "symbols.h"

#include <vector>

namespace aid::fortran {

// Input-output lists are used in formatted i/o.

struct io_list_item {
    symbol_name variable;
    argument_list_t indices;  // empty if variable is scalar

    // If `variable` references a scalar (or a specific element of an array),
    // the index_control is not used.
    //
    // In the general case, there could be nested index controls for each
    // dimension of an array.  None of the Adventure versions tackled so far
    // require more than one dimension.
    index_control_t index_control;
};

using io_list_t = std::vector<io_list_item>;

// DATA statements use a restricted form of i/o lists, but trying to use the
// general io_list_t for DATA was more trouble than it was worth, so I made a
// similar-but-different variable_list_t.
struct subscript_t {
    symbol_name    induction;
    machine_word_t coefficient;
    machine_word_t offset;
};

using subscript_list_t = std::vector<subscript_t>;

struct variable_list_item_t {
    symbol_name              variable;
    subscript_list_t         subscripts;  // empty if variable is scalar
    constant_index_control_t index_control;
};

using variable_list_t = std::vector<variable_list_item_t>;

}

#endif
