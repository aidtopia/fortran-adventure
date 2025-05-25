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
#include <string>
#include <vector>

namespace aid::fortran {

// Each entry maps the COMMON block name to the count of the number of
// variables it contains.
using comdat_table = std::map<symbol_name, unsigned>;

using format_table = std::map<statement_number_t, field_list_t>;

// A unit is a program, subroutine, or function.
class unit {
    public:
        explicit unit(symbol_name name = symbol_name{""})
            : m_unit_name(name) {}
        virtual ~unit();

        // The name of this unit (i.e., the program, subroutine, or function).
        symbol_name unit_name() const { return m_unit_name; }
        void set_unit_name(symbol_name const &name) { m_unit_name = name; }

        // Replaces (or adds) the symbol record.
        void update_symbol(symbol_info const &symbol);
        void update_symbol(symbol_info &&symbol);

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

        void add_format(statement_number_t number, field_list_t const &fields);
        format_table const &formats() const { return m_formats; }

        void add_statement(statement_t statement);
        statement_block const &code() const { return m_code; }

        void set_implicit_type(char prefix, datatype type);

        datatype implicit_type(char prefix) const {
            if (prefix < 'A' || 'Z' < prefix) return datatype::unknown;
            return m_implicit_type_table[prefix - 'A'];
        }
        datatype implicit_type(symbol_name const &name) const {
            return implicit_type(name.front());
        }

        unsigned comdat_count(symbol_name const &block) const {
            if (auto it = m_comdats.find(block); it != m_comdats.end()) {
                return it->second;
            }
            return 0u;
        }

        void set_comdat_count(symbol_name const &block, unsigned count) {
            m_comdats[block] = count;
        }

        virtual void print_symbol_table(std::ostream &out) const;

    private:
        std::array<datatype, 26> m_implicit_type_table = {
            // 'A' through 'H':
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            // 'I' through 'N':
            datatype::INTEGER, datatype::INTEGER, datatype::INTEGER,
            datatype::INTEGER, datatype::INTEGER, datatype::INTEGER,
            // 'O' through 'Z':
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL
        };

        static std::string format_dimension(dimension const &d);
        static std::string format_word(machine_word_t w, datatype type);

        symbol_name  m_unit_name;
        symbol_table m_symbols;
        // We store info about the common blocks (comdats) separately from the
        // symbols because they're in distinct name spaces.
        comdat_table m_comdats;
        format_table m_formats;
        statement_block m_code;
};

}

#endif
