#ifndef AID_FORTRAN_PROGRAM_H
#define AID_FORTRAN_PROGRAM_H

#include "unit.h"

#include <filesystem>
#include <span>
#include <vector>

namespace aid::fortran {

// TODO:  BIG LONG-TERM
// Don't derive program from unit.  It should contain units, one of
// which should be marked as the main program.

class program : public unit {
    public:
        program() {}
        ~program() {}
        program(program &&rhs) noexcept = default;
        program &operator=(program &&rhs) noexcept = default;
        program(program const &rhs) = delete;
        program &operator=(program const &rhs) = delete;

        void set_source_files(std::span<std::filesystem::path> files);
        std::vector<std::filesystem::path> const &get_source_files() const;

        void add_subprogram(unit &&subprogram);

        // Move symbol table printing to free functions that accept either a
        // single unit or a program.  The unit and program interfaces should
        // provide enough access to the symbol tables to let them be free.
        void print_symbol_table(std::ostream &out) const override;

        // TODO:  Replace "extract" with "select" or something like that.  And
        // it probably should return pointers to the units.
        std::vector<unit const *> extract_subprograms() const;

        void mark_referenced() override;

    private:
        unit *find_subprogram(symbol_name name);

        std::vector<unit> m_subprograms;
        std::vector<std::filesystem::path> m_source_files;
};

}

#endif
