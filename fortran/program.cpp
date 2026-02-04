#include "program.h"

#include <algorithm>
#include <cassert>
#include <memory>
#include <set>

namespace aid::fortran {

bool program::empty() const {
    return m_subprograms.empty();
}

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
    // If this will be the first SUBROUTINE or FUNCTION added, then we need to
    // reserve the first slot for the PROGRAM.
    if (m_subprograms.empty()) m_subprograms.emplace_back();
    m_subprograms.push_back(std::move(subprogram));
}

unit *program::find_subprogram(symbol_name name) {
    auto const it = std::find_if(m_subprograms.begin(), m_subprograms.end(),
        [&name] (unit const &u) { return u.unit_name() == name; });
    if (it == m_subprograms.end()) return nullptr;
    return &*it;
}

void mark_reachable(program &prog) {
    auto is_called = [] (symbol_info const &symbol) {
        return
            symbol.referenced && (
                symbol.kind == symbolkind::subprogram ||
                symbol.kind == symbolkind::external
            );
    };

    auto processed = std::set<symbol_name>{};
    auto to_process = std::set<symbol_name>{};
    if (!prog.empty()) to_process.insert(prog.entry_point().unit_name());

    while (!to_process.empty()) {
        auto const this_round = to_process;  // copy to avoid iter invalidation
        for (auto name : this_round) {
            to_process.erase(name);
            processed.insert(name);
            if (auto sub = prog.find_subprogram(name); sub) {
                sub->mark_reachable();
                // Add newly referenced callees.
                for (auto const &callee : sub->extract_symbols(is_called)) {
                    if (!processed.contains(callee.name)) {
                        to_process.insert(callee.name);
                    }
                }
            }
        }
    }
}

unsigned assign_addresses(program &prog) {
    auto memsize = 1u;  // first slot reserved like NULL
    auto const comdat_sizes = common_block_sizes(prog);
    auto comdat_bases = std::map<symbol_name, unsigned>{};
    for (auto const &[block, size] : comdat_sizes) {
        auto const base = memsize;
        memsize += size;
        comdat_bases[block] = base;
    }

    // In Fortran IV, recursion and re-entrancy are not allowed because local
    // variables are not allocated on a stack.  All variables in the program
    // have static storage.
    for (auto &sub : prog) {
        if (!sub.is_reachable()) continue;
        if (auto retvals = sub.extract_symbols(is_return_value);
            !retvals.empty()
        ) {
            assert(retvals.size() == 1);
            auto &retval = retvals.front();
            assert(retval.referenced && "function doesn't return a value!?");
            retval.address = memsize;
            memsize += core_size(retval);
            sub.update_symbol(std::move(retval));
        }
        if (auto commons = sub.extract_symbols(is_common, by_block_index);
            !commons.empty()
        ) {
            auto block = symbol_name{};
            auto base = 0u;
            auto offset = 0u;
            for (auto &common : commons) {
                if (block != common.comdat) {
                    block = common.comdat;
                    base = comdat_bases[block];
                    offset = 0u;
                }
                auto const size = core_size(common);
                if (common.referenced) {
                    common.address = base + offset;
                    sub.update_symbol(std::move(common));
                }
                offset += size;  // even if not referenced!
                assert(comdat_sizes.contains(block) &&
                       offset <= comdat_sizes.find(block)->second);
            }
        }
        if (auto locals = sub.extract_symbols(is_referenced_local);
            !locals.empty()
        ) {
            for (auto &local : locals) {
                local.address = memsize;
                memsize += core_size(local);
                sub.update_symbol(std::move(local));
            }
        }
        if (auto externals = sub.extract_symbols(is_referenced_external);
            !externals.empty()
        ) {
            for (auto &external : externals) {
                external.address = memsize;
                memsize += core_size(external);
                sub.update_symbol(std::move(external));
            }
        }
    }
    prog.set_core_requirement(memsize);
    return memsize;
}

std::map<symbol_name, unsigned> common_block_sizes(program const &prog) {
    // Within a subprogram, a set of variables in the same common block
    // determines the required size of that block.  If two or more subprograms
    // require different sizes for the same block, the first one loaded
    // should determine the size.  If a later subprogram needs a larger size,
    // it's officially an error, but we'll just use the largest.
    auto comdats = std::map<symbol_name, unsigned>{};

    auto update = [&comdats](symbol_name const &block, unsigned size) {
        auto const prior_size = comdats.contains(block) ? comdats[block] : 0;
        comdats[block] = std::max(size, prior_size);
    };

    for (auto const &sub : prog) {
        auto const commons = sub.extract_symbols(is_common, by_block_index);
        auto block = symbol_name{};
        auto size = 0u;
        for (auto const &var : commons) {
            if (var.comdat != block) {
                update(block, size);
                block = var.comdat;
                size = 0u;
            }
            size += core_size(var);
        }
        if (size > 0u) update(block, size);
    }
    return comdats;
}

}
