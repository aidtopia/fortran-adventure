#ifndef AID_FORTRAN_UNIT_H
#define AID_FORTRAN_UNIT_H

#include "fieldlist.h"
#include "machine.h"
#include "basicstatement.h"
#include "symbols.h"

#include <array>
#include <iostream>
#include <map>
#include <functional>
#include <span>
#include <string>
#include <vector>

namespace aid::fortran {

// Each entry maps the COMMON block name to the count of the number of
// variables it contains.
using comdat_table = std::map<symbol_name, unsigned>;

using format_table = std::map<symbol_name, field_list_t>;

// Maps the name of a symbolkind::label to an index in m_code
using target_table = std::map<symbol_name, std::size_t>;

// A unit (think "translation unit") contains the symbols and statements for
// a program, subroutine, or function (i.e., Fortran subprograms).
class unit {
    public:
        explicit unit(symbol_name name = symbol_name{""})
            : m_unit_name(name) {}
        ~unit() = default;
        unit(unit &&rhs) noexcept = default;
        unit &operator=(unit &&rhs) noexcept = default;
        unit(unit const &rhs) = delete;
        unit &operator=(unit const &rhs) = delete;

        bool empty() const { return m_symbols.empty() && m_code.empty(); }
        bool is_reachable() const { return m_reachable; }

        // The name of this unit (i.e., the program, subroutine, or function).
        symbol_name unit_name() const { return m_unit_name; }
        void set_unit_name(symbol_name const &name) { m_unit_name = name; }

        // Replaces (or adds) the symbol record.
        void update_symbol(symbol_info const &symbol);
        void update_symbol(symbol_info &&symbol);

        // Arithmetic function definition parameters are temporary symbols that
        // might shadow an actual symbol in the unit.
        void push_shadow(symbol_info &&symbol);
        void pop_shadows();

        // A shortcut for ensuring the symbol's `referenced` flag is set.
        void mark_symbol_referenced(symbol_name const &name);

        // Uses the implicit table to resolve types that are still unknown.
        void infer_types();

        bool has_symbol(symbol_name const &name) const;
        symbol_info find_symbol(symbol_name const &name) const;

        std::vector<symbol_info> extract_symbols() const;
        std::vector<symbol_info> extract_symbols(
            std::function<bool (symbol_info const &)> choose_if
        ) const;
        std::vector<symbol_info> extract_symbols(
            std::function<bool (symbol_info const &)> choose_if,
            std::function<bool (symbol_info const &, symbol_info const &)> sort_by
        ) const;

        void add_format(symbol_name label, field_list_t &&fields);
        void add_format(statement_number_t number, field_list_t &&fields);
        format_table const &formats() const { return m_formats; }

        void add_statement(statement_t statement);
        statement_block const &code() const { return m_code; }

        // TODO:  mark_reachable should be a free function.
        void mark_reachable();
        // TODO:  print_symbol_table should be a free function.
        void print_symbol_table(std::ostream &out) const;

    private:
        static std::string format_dimension(dimension const &d);
        static std::string format_word(machine_word_t w, datatype type);

        symbol_name  m_unit_name;
        symbol_table m_symbols;
        symbol_table m_shadows;  // temporary symbols that might shadow others
        format_table m_formats;
        statement_block m_code;
        target_table m_targets;
        bool m_reachable = false;
};

// Predicates for extract_symbols.
inline bool is_array(symbol_info const &a) {
    return !a.shape.empty();
}
inline bool is_argument(symbol_info const &a) {
    return a.kind == symbolkind::argument;
}
inline bool is_unreferenced_argument(symbol_info const &a) {
    return a.kind == symbolkind::argument && !a.referenced;
}
inline bool is_common(symbol_info const &a) {
    return a.kind == symbolkind::common;
}
inline bool is_return_value(symbol_info const &a) {
    return a.kind == symbolkind::retval;
}
inline bool is_referenced_format(symbol_info const &a) {
    return a.kind == symbolkind::format && a.referenced;
}
inline bool is_referenced_local(symbol_info const &a) {
    return a.kind == symbolkind::local && a.referenced;
}
inline bool is_referenced_subprogram(symbol_info const &a) {
    return a.kind == symbolkind::subprogram && a.referenced;
}
inline bool has_unknown_type(symbol_info const &a) {
    return a.type == datatype::unknown;
}

inline bool by_address(symbol_info const &a, symbol_info const &b) {
    return a.address < b.address;
}
inline bool by_block_index(symbol_info const &a, symbol_info const &b) {
    if (a.comdat < b.comdat) return true;
    if (a.comdat > b.comdat) return false;
    if (a.index < b.index) return true;
    return false;
}
inline bool by_index(symbol_info const &a, symbol_info const &b) {
    return (a.index < b.index);
}
inline bool by_name(symbol_info const &a, symbol_info const &b) {
    if (a.name < b.name) return true;
    if (a.name > b.name) return false;
    return false;
};


}

#endif
