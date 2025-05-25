#ifndef AID_FORTRAN_PROGRAM_H
#define AID_FORTRAN_PROGRAM_H

#include "unit.h"

#include <vector>

namespace aid::fortran {

class program : public unit {
    public:
        program() {}
        ~program() {}

        void add_subprogram(unit &&subprogram);
        void print_symbol_table(std::ostream &out) const override;

        std::vector<unit const *> extract_subprograms() const;

    private:
        std::vector<unit> m_subprograms;
};

}

#endif
