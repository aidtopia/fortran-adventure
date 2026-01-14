#include "program.h"

#include <memory>

namespace aid::fortran {

void program::set_source_files(std::span<std::filesystem::path> files) {
    m_source_files.reserve(m_source_files.size() + files.size());
    for (auto const &path : files) {
        m_source_files.push_back(path);
    }
}

std::vector<std::filesystem::path> const &program::get_source_files() const {
    return m_source_files;
}

void program::add_subprogram(unit &&subprogram) {
    m_subprograms.push_back(std::move(subprogram));
}

void program::print_symbol_table(std::ostream &out) const {
    unit::print_symbol_table(out);
    for (auto const &sub : m_subprograms) {
        sub.print_symbol_table(out);
    }
}

std::vector<unit const *> program::extract_subprograms() const {
    auto subprograms = std::vector<unit const *>{};
    subprograms.reserve(m_subprograms.size());
    for (auto const &subprogram : m_subprograms) {
        subprograms.push_back(&subprogram);
    }
    return subprograms;
}

}
