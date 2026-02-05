#ifndef AID_FORTRAN_COMDATS_H
#define AID_FORTRAN_COMDATS_H

#include "symbols.h"

#include <map>

namespace aid::fortran {

struct comdat_address_range { unsigned base, size; };

using comdat_table = std::map<symbol_name, comdat_address_range>;

class program;

comdat_table common_blocks(unsigned &base_address, program const &prog);

}

#endif
