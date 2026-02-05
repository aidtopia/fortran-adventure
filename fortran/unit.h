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

// A unit (think "translation unit") contains the symbols and statements for
// a program, subroutine, or function (i.e., Fortran subprograms).
class unit {
    public:
        explicit unit(symbol_name name = symbol_name{""})
            : m_unit_name(name) {}

        // Arithmetic functions are implemented as "internal" subprograms within
        // the parent's scope.
        explicit unit(symbol_name name, symbol_name parent)
            : m_unit_name(name), m_parent_name(parent) {}

        ~unit() = default;
        unit(unit &&rhs) noexcept = default;
        unit &operator=(unit &&rhs) noexcept = default;
        unit(unit const &rhs) = delete;
        unit &operator=(unit const &rhs) = delete;

        bool empty() const {
            return m_unit_name.empty() && m_symbols.empty() && m_code.empty();
        }
        bool is_reachable() const { return m_reachable; }

        // The name of this unit (i.e., the program, subroutine, or function).
        symbol_name unit_name() const { return m_unit_name; }
        void set_unit_name(symbol_name const &name);
        symbol_name parent_name() const { return m_parent_name; }
        void set_parent_name(symbol_name const &name);
        std::string full_name() const;

        // Replaces (or adds) the symbol record.
        void update_symbol(symbol_info const &symbol);
        void update_symbol(symbol_info &&symbol);

        // A shortcut for ensuring the symbol's `referenced` flag is set.
        void mark_symbol_referenced(symbol_name const &name);

        // Uses the implicit table to resolve types that are still unknown.
        void infer_types();

        bool has_symbol(symbol_name const &name) const;
        symbol_info find_symbol(symbol_name const &name) const;

        std::vector<symbol_info> extract_symbols(
            std::function<bool (symbol_info const &)> choose_if
        ) const;
        std::vector<symbol_info> extract_symbols(
            std::function<bool (symbol_info const &)> choose_if,
            std::function<bool (symbol_info const &, symbol_info const &)> sort_by
        ) const;

        void add_format(symbol_name label, field_list_t &&fields);
        void add_format(statement_number_t number, field_list_t &&fields);
        field_list_t find_format(symbol_name const &label) const;

        void add_statement(statement_t statement);
        statement_block const &code() const { return m_code; }

        // Internal units are used to implement arithmetic functions.
        void add_internal(unit &&internal);
        std::span<const unit> internals() const { return m_internals; }
        std::span<unit> internals() { return m_internals; }

        void add_subroutine_pointer_type(std::size_t arg_count);
        void add_function_pointer_type(std::size_t arg_count);

        unsigned extract_subroutine_pointer_types() const {
            return m_subroutine_types;
        }
        unsigned extract_function_pointer_types() const {
            return m_function_types;
        }

        // TODO:  mark_reachable should be a free function.
        void mark_reachable();
        // TODO:  print_symbol_table should be a free function.
        void print_symbol_table(std::ostream &out) const;

    private:
        static std::string format_dimension(dimension const &d);
        static std::string format_word(machine_word_t w, datatype type);

        // Each entry maps the COMMON block name to the count of the number of
        // variables it contains.
        using comdat_table = std::map<symbol_name, unsigned>;

        // Maps statement labels to the field list of a FORMAT statement.
        using format_table = std::map<symbol_name, field_list_t>;

        // Maps the name of a symbolkind::label to an index in m_code.
        using branch_target_table = std::map<symbol_name, std::size_t>;

        using internal_table = std::vector<unit>;

        symbol_name  m_unit_name;
        symbol_name  m_parent_name;  // if this is internal, else empty
        symbol_table m_symbols;
        format_table m_formats;
        statement_block m_code;
        branch_target_table m_targets;
        internal_table m_internals;  // i.e., arithmetic functions
        unsigned m_subroutine_types = 0u;  // bitmask: if b_n, then n arg count
        unsigned m_function_types   = 0u;  // bitmask: if b_n, then n arg count
        bool m_reachable = false;
};

// Predicates for extract_symbols.
inline bool all(symbol_info const &) {
    return true;
}
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
inline bool is_referenced_external(symbol_info const &a) {
    return a.kind == symbolkind::external && a.referenced;
}
inline bool is_referenced_format(symbol_info const &a) {
    return a.kind == symbolkind::format && a.referenced;
}
inline bool is_referenced_local(symbol_info const &a) {
    return a.kind == symbolkind::local && a.referenced;
}
inline bool is_referenced_local_or_temp(symbol_info const &a) {
    return a.referenced && (
        a.kind == symbolkind::local ||
        a.kind == symbolkind::temporary
    );
}
inline bool is_referenced_subprogram(symbol_info const &a) {
    return a.kind == symbolkind::subprogram && a.referenced;
}
inline bool is_referenced_temporary(symbol_info const &a) {
    return a.kind == symbolkind::temporary && a.referenced;
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
