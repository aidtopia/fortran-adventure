#ifndef AID_EXPERIMENTAL_FORTRAN_FIELDLIST_H
#define AID_EXPERIMENTAL_FORTRAN_FIELDLIST_H

#include "aid/experimental/fortran/symbols.h"

#include <string>

namespace aid::fortran {

// We're simply keeping the entire FORMAT field list as a string to be
// interpreted while executing the input or output operation.
using field_list_t = std::string;

}

#endif
