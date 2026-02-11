#include "program.h"
#include "comdats.h"

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
    auto const comdats = common_blocks(memsize, prog);  // changes memsize!

    // In Fortran IV, recursion and re-entrancy are not allowed.  Local
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
            assert(!comdats.empty());
            auto block = comdats.begin()->first;
            auto range = comdats.begin()->second;
            auto offset = 0u;
            for (auto &common : commons) {
                if (block != common.comdat) {
                    block = common.comdat;
                    assert(comdats.contains(block));
                    range = comdats.find(block)->second;
                    offset = 0u;
                }
                auto const size = core_size(common);
                if (common.referenced) {
                    common.address = range.base + offset;
                    sub.update_symbol(std::move(common));
                }
                offset += size;  // even if not referenced!
                assert(offset <= range.size);
            }
        }
        for (auto &local : sub.extract_symbols(is_referenced_local_or_temp)) {
            local.address = memsize;
            memsize += core_size(local);
            sub.update_symbol(std::move(local));
        }
        for (auto &external : sub.extract_symbols(is_referenced_external)) {
            external.address = memsize;
            memsize += core_size(external);
            sub.update_symbol(std::move(external));
        }

        for (auto &internal : sub.internals()) {
            // Internal units need their own addresses for a few symbols.
            auto is_independent = [](symbol_info const &a) {
                return a.referenced && (
                    a.kind == symbolkind::argument  ||
                    a.kind == symbolkind::retval    ||
                    a.kind == symbolkind::temporary
                );
            };
            for (auto symbol : internal.extract_symbols(is_independent)) {
                assert(symbol.address == 0u);
                symbol.address = memsize;
                memsize += core_size(symbol);
                internal.update_symbol(symbol);
            }

            // Most of the result of the symbols should be aliases of the
            // parent unit.
            auto is_alias_of_parent = [](symbol_info const &a) {
                return a.referenced && (
                    a.kind == symbolkind::local      ||
                    a.kind == symbolkind::common     ||
                    a.kind == symbolkind::subprogram ||
                    a.kind == symbolkind::internal
                );
            };
            for (auto alias : internal.extract_symbols(is_alias_of_parent)) {
                auto const &parent = sub.find_symbol(alias.name);
                assert(alias.address == 0u);
                alias.address = parent.address;
                internal.update_symbol(alias);
            }

        }
    }
    prog.set_core_requirement(memsize);
    return memsize;
}

}
