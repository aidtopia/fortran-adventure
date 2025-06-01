#ifndef AID_FORTRAN_INDEXCONTROL_H
#define AID_FORTRAN_INDEXCONTROL_H

#include "expressions.h"
#include "symbols.h"

namespace aid::fortran {

struct index_control_t {
    symbol_name index;
    expression_t init;
    expression_t limit;
    expression_t step;
};

// For DATA statements, the index control must be constants.
struct constant_index_control_t {
    symbol_name index;
    machine_word_t init;
    machine_word_t limit;
    machine_word_t step;
};

}

#endif
