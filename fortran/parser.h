#ifndef AID_EXPERIMENTAL_FORTRAN_PARSER_H
#define AID_EXPERIMENTAL_FORTRAN_PARSER_H

#include "aid/experimental/fortran/basicstatement.h"
#include "aid/experimental/fortran/datalist.h"
#include "aid/experimental/fortran/expressions.h"
#include "aid/experimental/fortran/fieldlist.h"
#include "aid/experimental/fortran/iolist.h"
#include "aid/experimental/fortran/program.h"
#include "aid/experimental/fortran/unit.h"

#include <cassert>
#include <concepts>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <string_view>
#include <utility>
#include <vector>

namespace aid::fortran {

class parser {
    public:
        // Encapsulate a parsing error.
        class error_t {
            public:
                error_t() : m_msg("error") {};
                explicit error_t(std::string_view msg) : m_msg(msg) {}
                explicit error_t(std::string &&msg) : m_msg(std::move(msg)) {}
                std::string message() const { return m_msg; }
            private:
                std::string m_msg;
                // TODO:  source file line number
        };

        // parser::expected is like std::expected except that the unexpected
        // type is always parser::error_t.
        template <typename T> using expected = std::expected<T, error_t>;

        static program parse_file(std::string_view filename);
        static program parse_stream(std::istream &stream);

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

        static std::unexpected<error_t> error(error_t const &e) {
            return std::unexpected<error_t>{e};
        }

        static std::unexpected<error_t> error(error_t &&e) {
            return std::unexpected<error_t>{std::move(e)};
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
            RETURN,
            STOP,
            TYPE
        };

        // The types of statements allowed at a given point in the code depends
        // on the phase.  A later version of the PDP-10 Fortran IV reference
        // says some restrictions aren't as rigid as shown here, but the
        // Adventure code doesn't exploit that flexibility.
        enum phase_t {
        //  PHASE       STATEMENTS ALLOWED
            phase0,  // PROGRAM (possibly implied), FUNCTION, or SUBROUTINE
            phase1,  // IMPLICIT and FORMAT
            phase2,  // specifications and FORMAT
            phase3,  // statement function definition, FORMAT, and DATA
            phase4,  // executable statement, FORMAT, DATA, and END
            phase5   // FUNCTION or SUBROUTINE
        };

        enum class openkey { unknown, UNIT, ACCESS, FILE, NAME=FILE };

        program parse_source_code();
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
        expected<statement_t>
        parse_statement_function_definition(symbol_name const &name);

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
        array_shape parse_array_shape();
        expected<dimension> parse_one_dimension();
        expected<io_list_t> parse_io_list();
        expected<io_list_item> parse_io_list_item();
        expected<expression_t> parse_indexed_array_element();
        expected<index_control_t> parse_index_control();
        expected<data_list_t> parse_data_list();
        expected<field_list_t> parse_field_list();
        expected<parameter_list_t> parse_parameter_list();
        expected<argument_list_t> parse_argument_list();
        expected<expression_t> parse_argument();
        keyword parse_keyword();
        expected<variable_list_t> parse_variable_list();
        symbol_name parse_identifier();
        expected<openkey> parse_open_keyword();
        expected<operator_t> parse_operator();
        expected<constant_t> parse_constant();
        expected<machine_word_t> parse_integer_constant();
        expected<machine_word_t> parse_literal_constant();
        expected<machine_word_t> parse_logical_constant();
        expected<machine_word_t> parse_integer(int base = 10, int length = 0);
        expected<std::string> parse_literal();
        expected<statement_number_t> parse_statement_number();

        void add_label(statement_number_t number);

        bool begin_program(symbol_name const &name = symbol_name{});
        bool end_program();
        bool begin_subprogram(symbol_name const &name = symbol_name{"SUBPRG"});
        bool end_subprogram();

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

        bool at_eol() const { return m_it == m_statement.end(); }
        char current() const { return at_eol() ? '\0' : *m_it; }
        bool match(char ch) const { return current() == ch; }
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
        char consume() { return at_eol() ? '\0' : *m_it++; }
        bool accept(char ch) {
            if (match(ch)) {
                advance();
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

        static keyword look_up_keyword(std::string_view token);
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
        unit *m_current_unit = nullptr;  // points to m_program or m_subprogram
        phase_t m_phase = phase0;
        statement_number_t m_statement_number;
};

}

#endif
