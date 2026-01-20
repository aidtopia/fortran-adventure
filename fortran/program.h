#ifndef AID_FORTRAN_PROGRAM_H
#define AID_FORTRAN_PROGRAM_H

#include "unit.h"

#include <filesystem>
#include <span>
#include <vector>

namespace aid::fortran {

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
        void print_symbol_table(std::ostream &out) const override;

        std::vector<unit const *> extract_subprograms() const;

    private:
        std::vector<unit> m_subprograms;
        std::vector<std::filesystem::path> m_source_files;
};

}

#endif
