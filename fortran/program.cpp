#include "program.h"

#include <memory>

namespace aid::fortran {

void program::add_subprogram(unit &&subprogram) {
    m_subprograms.push_back(std::move(subprogram));
}

void program::print_symbol_table() const {
    unit::print_symbol_table();
    for (auto const &sub : m_subprograms) {
        sub.print_symbol_table();
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
