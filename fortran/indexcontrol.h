#ifndef AID_EXPERIMENTAL_FORTRAN_INDEXCONTROL_H
#define AID_EXPERIMENTAL_FORTRAN_INDEXCONTROL_H

#include "aid/experimental/fortran/expressions.h"
#include "aid/experimental/fortran/symbols.h"

namespace aid::fortran {

struct index_control_t {
    symbol_name index;
    expression_t init;
    expression_t final;
    expression_t step;
};

}

#endif

