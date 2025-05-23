#ifndef AID_FORTRAN_INDEXCONTROL_H
#define AID_FORTRAN_INDEXCONTROL_H

#include "expressions.h"
#include "symbols.h"

namespace aid::fortran {

struct index_control_t {
    symbol_name index;
    expression_t init;
    expression_t final;
    expression_t step;
};

}

#endif

