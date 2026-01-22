#ifndef AID_FORTRAN_PROGRAM_H
#define AID_FORTRAN_PROGRAM_H

#include "unit.h"

#include <filesystem>
#include <map>
#include <span>
#include <vector>

namespace aid::fortran {

// Represents a collection of subprograms.  (Fortran's SUBROUTINEs, FUNCTIONs,
// and PROGRAMs are all "subprograms".)  The first subprogram is the
// PROGRAM, the entry point that will be called by the runtime.
class program {
    public:
        program() {}

        ~program() = default;
        program(program &&rhs) noexcept = default;
        program &operator=(program &&rhs) noexcept = default;
        program(program const &rhs) = delete;
        program &operator=(program const &rhs) = delete;

        bool empty() const;
        bool has_main_subprogram() const;

        symbol_name name() const;
        void set_name(symbol_name const &name);

        void set_source_files(std::span<std::filesystem::path> files);
        std::vector<std::filesystem::path> const &get_source_files() const;

        void add_main_subprogram(unit &&subprogram);
        void add_subprogram(unit &&subprogram);

        // Move symbol table printing to free functions that accept either a
        // single unit or a program.  The unit and program interfaces should
        // provide enough access to the symbol tables to let them be free.
        void print_symbol_table(std::ostream &out) const;

        void mark_referenced();

        // Iteration through the subprograms.
        using units = std::vector<unit>;
        using iterator = units::iterator;
        using const_iterator = units::const_iterator;
        const_iterator cbegin() const { return m_subprograms.cbegin(); }
        const_iterator cend()   const { return m_subprograms.cend(); }
        const_iterator  begin() const { return cbegin(); }
        const_iterator  end()   const { return cend(); }
        iterator        begin()       { return m_subprograms.begin(); }
        iterator        end()         { return m_subprograms.end(); }

    private:
        unit *find_subprogram(symbol_name name);

        units m_subprograms;
        std::vector<std::filesystem::path> m_source_files;
};

std::map<symbol_name, std::size_t> common_block_sizes(program const &prog);

}

#endif
