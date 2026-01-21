#include "program.h"

#include <memory>
#include <set>

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

void program::mark_referenced() {
    unit::mark_referenced();
    auto is_called = [] (symbol_info const &symbol) {
        return symbol.kind == symbolkind::subprogram && symbol.referenced;
    };

    auto to_process = std::set<symbol_name>{};
    for (auto const &callee : extract_symbols(is_called)) {
        to_process.insert(callee.name);
    }
    auto processed = std::set<symbol_name>{};

    while (!to_process.empty()) {
        auto const this_round = to_process;  // copy to avoid iter invalidation
        for (auto name : this_round) {
            to_process.erase(name);
            if (auto sub = find_subprogram(name); sub) {
                sub->mark_referenced();
                // Add newly referenced callees.
                for (auto const &callee : sub->extract_symbols(is_called)) {
                    if (!processed.contains(callee.name)) {
                        to_process.insert(callee.name);
                    }
                }
                processed.insert(sub->unit_name());
            }
        }
    }
}

unit * program::find_subprogram(symbol_name name) {
    auto const it = std::find_if(m_subprograms.begin(), m_subprograms.end(),
        [&name] (unit const &u) { return u.unit_name() == name; });
    if (it == m_subprograms.end()) return nullptr;
    return &*it;
}


}
