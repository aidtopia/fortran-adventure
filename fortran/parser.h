#ifndef AID_FORTRAN_PARSER_H
#define AID_FORTRAN_PARSER_H

#include "arithmeticfunction.h"
#include "basicstatement.h"
#include "datalist.h"
#include "expressions.h"
#include "fieldlist.h"
#include "implicit.h"
#include "iolist.h"
#include "program.h"
#include "unit.h"

#include <cassert>
#include <concepts>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace aid::fortran {

class parser {
    public:
        // Encapsulate a parsing error.
        class error_t {
            public:
                error_t() : m_msg("error") {};
                explicit error_t(std::string msg) : m_msg(std::move(msg)) {}
                std::string_view message() const { return m_msg; }
            private:
                std::string m_msg;
                // TODO:  source file line number
        };

        // parser::expected is like std::expected except that the unexpected
        // type is always parser::error_t.
        template <typename T> using expected = std::expected<T, error_t>;

        static expected<program> parse_files(
            std::span<std::filesystem::path> paths
        );
        static expected<program> parse_stream(std::istream &stream);

    private:
        explicit parser(std::istream &stream) : m_in(stream) {}

        // These function templates make returning errors more concise.
        template <typename... Args>
        static std::unexpected<error_t> error(
            std::format_string<Args...> fmt,
            Args && ... args
        ) {
            return std::unexpected<error_t>{
                error_t{std::format(fmt, std::forward<Args>(args)...)}
            };
        }

        // MOVE ONLY!
        // This takes the error_t from an expected<T> and returns it in a way
        // that can be used in create or assign into an expected<U>.
        template <typename T>
        static std::unexpected<error_t> error_of(expected<T> &&e) {
            return std::unexpected<error_t>{std::move(e).error()};
        }

        template <typename... Args>
        void warn(
            std::format_string<Args...> fmt,
            Args && ... args
        ) const {
            std::print(std::cerr, "WARNING: {}\n  {}\n",
                       std::format(fmt, std::forward<Args>(args)...),
                       m_statement);
        }

        using prefix_set_t = std::set<char>;

        // We're working with just the subset of Fortran IV keywords that are
        // used in the source for Adventure.  These keywords are also used to
        // identify the type of statement being processed.
        enum class keyword {
            unknown,
            assignment, // not a actually a keyword, but a statement type
            ACCEPT,
            CALL,
            COMMON,
            CONTINUE,
            DATA,
            DIMENSION,
            DO,
            END,
            EXTERNAL,
            FORMAT,
            FUNCTION,
            GOTO,
            IF,
            IMPLICIT,
            INTEGER,
            LOGICAL,
            OPEN,
            PAUSE,
            PROGRAM,
            SUBROUTINE,
            READ,
            REAL,
            RETURN,
            STOP,
            TYPE
        };

        // The types of statements allowed at a given point in the code depends
        // on the phase.  A later version of the PDP-10 Fortran IV reference
        // says some restrictions aren't quite as rigid as shown here.
        // WOOD0350v2 doesn't exploit that flexibility, but v1 does.
        enum phase_t {
        //  PHASE       STATEMENTS ALLOWED
            phase0,  // PROGRAM (possibly implied), FUNCTION, or SUBROUTINE
            phase1,  // IMPLICIT and FORMAT
            phase2,  // specifications and FORMAT
            phase3,  // arithmetic function definition, FORMAT, and DATA
            phase4   // executable statement, FORMAT, DATA, and END
        };

        enum class openkey { unknown, UNIT, ACCESS, FILE, NAME=FILE };

        expected<program> parse_statements();
        expected<statement_t> parse_full_statement();
        expected<statement_number_t> parse_statement_number_field();
        expected<statement_t> parse_statement(statement_number_t number);
        expected<statement_t> parse_identified_statement(
            keyword kw,
            statement_number_t number
        );

        // Subprogram statements
        expected<statement_t> parse_program();
        expected<statement_t> parse_function(datatype type = datatype::unknown);
        expected<statement_t> parse_subroutine();
        expected<statement_t> parse_end();
        expected<statement_t> parse_arithmetic_function(symbol_name const &fn);

        // Non-executable statements
        expected<statement_t> parse_common();
        expected<statement_t> parse_data();
        expected<statement_t> parse_dimension();
        expected<statement_t> parse_external();
        expected<statement_t> parse_format(statement_number_t number);
        expected<statement_t> parse_implicit();
        expected<prefix_set_t> parse_implicit_prefixes();
        expected<statement_t> parse_type_specification(datatype type);
        
        // Executable statements
        expected<statement_t> parse_assignment();  // don't confuse with ASSIGN
        expected<statement_t> parse_accept();
        expected<statement_t> parse_call();
        expected<statement_t> parse_continue();
        expected<statement_t> parse_do();
        expected<statement_t> parse_goto();
        expected<statement_t> parse_unconditional_goto();
        expected<statement_t> parse_computed_goto();
        expected<statement_t> parse_if();
        expected<statement_t> parse_open();
        expected<statement_t> parse_pause();
        expected<statement_t> parse_read();
        expected<statement_t> parse_return();
        expected<statement_t> parse_stop();
        expected<statement_t> parse_type();

        // Expressions
        expected<expression_t> parse_expression();
        expected<expression_t> parse_alternative_expression();
        expected<expression_t> parse_compound_expression();
        expected<expression_t> parse_comparison();
        expected<expression_t> parse_arithmetic_expression();
        expected<expression_t> parse_term();
        expected<expression_t> parse_factor();
        expected<expression_t> parse_atom();

        // Parsing helpers
        expected<expression_t> parse_arithmetic_function_expression(
            parameter_list_t const &params
        );
        expected<array_shape> parse_array_shape();
        expected<dimension> parse_one_dimension();
        expected<io_list_t> parse_io_list();
        expected<io_list_item> parse_io_list_item();
        expected<index_control_t> parse_index_control();
        expected<data_list_t> parse_data_list();
        expected<field_list_t> parse_field_list();
        expected<parameter_list_t> parse_parameter_list();
        expected<argument_list_t> parse_argument_list();
        expected<expression_t> parse_argument();
        expected<keyword> parse_keyword();
        expected<variable_list_t> parse_variable_list();
        expected<variable_list_item_t> parse_variable_list_item();
        expected<constant_index_control_t> parse_constant_index_control();
        expected<index_list_t> parse_index_list();
        expected<index_t> parse_index();
        expected<subscript_list_t> parse_subscript_list();
        symbol_name parse_identifier();
        expected<openkey> parse_open_keyword();
        expected<operator_t> parse_operator();
        expected<constant_t> parse_constant();
        expected<constant_t> parse_numeric_constant();
        expected<machine_word_t> parse_integer_constant();
        expected<machine_word_t> parse_literal_constant();
        expected<machine_word_t> parse_logical_constant();
        expected<machine_word_t> parse_integer(int base = 10, int length = 0);
        expected<std::string> parse_literal();
        expected<statement_number_t> parse_statement_number();

        expected<bool> begin_main_subprogram(symbol_name const &name);
        expected<bool> end_main_subprogram();
        expected<bool> begin_subprogram(symbol_name const &name);
        expected<bool> end_subprogram();

        expected<bool> add_branch_target(statement_number_t number);

        datatype inferred_type(symbol_info const &symbol) const;
        void infer_type_and_update(symbol_info &symbol);
        void infer_types();

        // Hacks for when we need to peek ahead one token.
        bool accept(keyword kw);
        bool accept(operator_t op);

        bool match_assignment_statement() const;

        bool next_statement();
        bool next_line();

        void begin_statement_field() {
            m_it =
                (m_statement.size() > column_7) ? m_statement.begin() + column_7
                                                : m_statement.end();
        }

        unsigned next_common_count(symbol_name block) {
            if (!m_common_counts.contains(block)) {
                m_common_counts[block] = 0u;
            }
            return ++m_common_counts[block];
        }

        bool at_eol() const { return m_it == m_statement.end(); }
        char current() const { return at_eol() ? '\0' : *m_it; }
        bool match(char ch) const { return current() == ch; }
        bool match(std::string_view s) const {
            return std::string_view(m_it, m_statement.end()).starts_with(s);
        }
        bool match_letter() const {
            auto const ch = current();
            return 'A' <= ch && ch <= 'Z';
        }
        bool match_digit(int base = 10) const {
            assert(1 < base && base <= 10);
            auto const ch = current();
            return '0' <= ch && ch < '0' + base;
        }
        bool match_alphanumeric() const {
            auto const ch = current();
            return ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9');
        }
        void advance() { if (!at_eol()) ++m_it; }
        void advance(std::size_t count) {
            auto const limit = static_cast<std::size_t>(
                std::distance(m_it, m_statement.cend()));
            m_it += std::min(count, limit);
        }
        char consume() { return at_eol() ? '\0' : *m_it++; }
        bool accept(char ch) {
            if (match(ch)) {
                advance();
                return true;
            }
            return false;
        }
        bool accept(std::string_view s) {
            if (match(s)) {
                advance(s.size());
                return true;
            }
            return false;
        }

        std::string::const_iterator position() const { return m_it; }

        // Column numbers are 1-based, but our strings are 0-based, so we'll
        // name some constants to keep things straight.
        static std::size_t constexpr column_1 = 0;
        static std::size_t constexpr column_5 = 4;
        static std::size_t constexpr column_6 = 5;
        static std::size_t constexpr column_7 = 6;

        enum class line_type {
            comment, initial, continuation
        };

        static line_type constexpr categorize_line(std::string_view line);
        static void normalize_line(std::string &line);
        static void trim_leading_form_feeds(std::string &line);
        static void crush_statement(std::string &s);

        static openkey look_up_open_keyword(std::string_view token);

        template <typename T, typename... Args>
            requires std::derived_from<T, basic_statement>
        static statement_t make(Args &&... args) {
            return std::make_shared<T>(std::forward<Args>(args)...);
        }

        std::istream &m_in;
        std::string m_line;
        std::string m_statement;
        std::string::const_iterator m_it;
        program m_program;
        unit m_subprogram;
        implicit m_implicit;
        std::map<symbol_name, unsigned> m_common_counts;
        std::map<symbol_name, arithmetic_function_t> m_arithmetic_functions;
        phase_t m_phase = phase0;
        bool m_current_subprogram_is_main = false;
        statement_number_t m_statement_number = no_statement_number;
};

}

#endif
