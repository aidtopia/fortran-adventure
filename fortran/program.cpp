#include "program.h"

#include <algorithm>
#include <cassert>
#include <memory>
#include <set>

namespace aid::fortran {

bool program::has_main_subprogram() const {
    if (m_subprograms.empty()) return false;
    if (m_subprograms.front().empty()) return false;
    return true;
}

symbol_name program::name() const {
    return m_subprograms.empty() ? symbol_name{} : m_subprograms[0].unit_name();
}

void program::set_name(symbol_name const &name) {
    if (m_subprograms.empty()) m_subprograms.emplace_back();
    m_subprograms.front().set_unit_name(name);
}

void program::set_source_files(std::span<std::filesystem::path> files) {
    m_source_files.reserve(m_source_files.size() + files.size());
    for (auto const &path : files) {
        m_source_files.push_back(path);
    }
}

std::vector<std::filesystem::path> const &program::get_source_files() const {
    return m_source_files;
}

void program::add_main_subprogram(unit &&subprogram) {
    if (m_subprograms.empty()) {
        m_subprograms.push_back(std::move(subprogram));
    } else {
        assert(m_subprograms[0].empty());
        m_subprograms[0] = std::move(subprogram);
    }
}

void program::add_subprogram(unit &&subprogram) {
    // If this will be the first subprogram added, then we need to reserve
    // the first slot for the PROGRAM.
    if (m_subprograms.empty()) m_subprograms.emplace_back();
    m_subprograms.push_back(std::move(subprogram));
}

void program::print_symbol_table(std::ostream &out) const {
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
    auto is_called = [] (symbol_info const &symbol) {
        return
            symbol.referenced && (
                symbol.kind == symbolkind::subprogram ||
                symbol.kind == symbolkind::external
            );
    };

    auto processed = std::set<symbol_name>{};
    auto to_process = std::set<symbol_name>{};
    if (!m_subprograms.empty()) to_process.insert(m_subprograms[0].unit_name());

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

unit *program::find_subprogram(symbol_name name) {
    auto const it = std::find_if(m_subprograms.begin(), m_subprograms.end(),
        [&name] (unit const &u) { return u.unit_name() == name; });
    if (it == m_subprograms.end()) return nullptr;
    return &*it;
}

std::map<symbol_name, std::size_t> common_block_sizes(program const &prog) {
    // Within a subprogram, a set of variables in the same common block
    // determines the required size of that block.  If two or more subprograms
    // require different sizes for the same block, the first one loaded
    // should determine the size.  If a later subprogram needs a larger size,
    // it's officially an error, but we'll just use the largest.
    auto comdats = std::map<symbol_name, std::size_t>{};

    auto update = [&comdats](symbol_name const &block, std::size_t size) {
        auto const prior_size = comdats.contains(block) ? comdats[block] : 0uz;
        comdats[block] = std::max(size, prior_size);
    };

    for (auto const &sub : prog) {
        auto const commons = sub.extract_symbols(is_common, by_block_index);
        auto block = symbol_name{};
        auto size = 0uz;
        for (auto const &var : commons) {
            if (var.comdat != block) {
                update(block, size);
                block = var.comdat;
                size = 0uz;
            }
            size += array_size(var.shape);
        }
        if (size > 0uz) update(block, size);
    }
    return comdats;
}

}
