#ifndef AID_FORTRAN_STATEMENTS_H
#define AID_FORTRAN_STATEMENTS_H

#include "basicstatement.h"
#include "expressions.h"
#include "iolist.h"
#include "utility.h"

#include <filesystem>
#include <string>
#include <string_view>

namespace aid::fortran {

class arithmetic_function_definition_statement : public basic_statement {
    public:
        arithmetic_function_definition_statement(
            symbol_name macro,
            parameter_list_t const &params,
            expression_t definition
        ) :
            basic_statement(), m_macro(macro), m_params(params),
            m_definition(definition) {}
    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        static std::string format_parameters(parameter_list_t const &params);

        symbol_name m_macro;
        parameter_list_t m_params;
        expression_t m_definition;
};

class assignment_statement : public basic_statement {
    public:
        assignment_statement(expression_t lvalue, expression_t rhs) :
            m_lvalue(lvalue), m_rhs(rhs) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        expression_t m_lvalue;
        expression_t m_rhs;
};

class call_statement : public basic_statement {
    public:
        call_statement(symbol_name const &name, argument_list_t const &args) :
            m_name(name), m_args(args) {}

    protected:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        symbol_name m_name;
        argument_list_t m_args;
};

class indirect_call_statement : public call_statement {
    public:
        indirect_call_statement(
            symbol_name const &name,
            argument_list_t const &args
        ) : call_statement(name, args) {}

    private:
        std::string do_generate(unit const &u) const override;
};

class continue_statement : public basic_statement {
    public:
        continue_statement() : basic_statement() {}

    private:
        std::string do_generate(unit const &u) const override;
};

class do_statement : public basic_statement {
    public:
        do_statement(index_control_t const &control) :
            basic_statement(),
            m_index_control(control), m_body() {}

        void add(statement_t statement) {
            m_body.push_back(statement);
        }

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        index_control_t m_index_control;
        statement_block m_body;
};

class goto_statement : public basic_statement {
    public:
        explicit goto_statement(statement_number_t target):
            basic_statement(), m_target(target) {}
    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        statement_number_t m_target;
};

class computed_goto_statement : public basic_statement {
    public:
        explicit computed_goto_statement(
            std::vector<statement_number_t> const &targets,
            expression_t expression
        ) : basic_statement(), m_expression(expression), m_targets(targets) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        std::string cases() const;

        expression_t m_expression;
        std::vector<statement_number_t> m_targets;
};

class if_statement : public basic_statement {
    public:
        if_statement(expression_t condition, statement_t then) :
            basic_statement(), m_condition(condition), m_then(then) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        expression_t m_condition;
        statement_t m_then;
};

class numeric_if_statement : public basic_statement {
    public:
        numeric_if_statement(
            expression_t condition,
            statement_number_t negative,
            statement_number_t zero,
            statement_number_t positive
        ) :
            basic_statement(), m_condition(condition),
            m_negative(negative), m_zero(zero), m_positive(positive) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        expression_t m_condition;
        statement_number_t m_negative;
        statement_number_t m_zero;
        statement_number_t m_positive;
};

class open_statement : public basic_statement {
    public:
        open_statement(expression_t iounit, std::filesystem::path const &name) :
            basic_statement(), m_iounit(iounit), m_name(name) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        expression_t m_iounit;
        std::filesystem::path m_name;
};

class pause_statement : public basic_statement {
    public:
        pause_statement(std::string_view message) :
            basic_statement(), m_message(escape_string(message)) {}
    private:
        std::string do_generate(unit const &u) const override;

        std::string m_message;
};

class read_statement : public basic_statement {
    public:
        read_statement(
            expression_t iounit,
            statement_number_t format,
            io_list_t const &items
        ) : m_iounit(iounit), m_format(format), m_items(items) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        expression_t        m_iounit;
        statement_number_t  m_format;
        io_list_t           m_items;
};

class return_statement : public basic_statement {
    public:
        return_statement() : basic_statement(), m_retval() {}
        explicit return_statement(symbol_name const &retval) :
            basic_statement(), m_retval(retval) {};

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        symbol_name m_retval;
};

class stop_statement : public basic_statement {
    public:
        stop_statement() : basic_statement() {}
    private:
        std::string do_generate(unit const &u) const override;
};

class type_statement : public basic_statement {
    public:
        type_statement(statement_number_t format, io_list_t const &items) :
            basic_statement(), m_format(format), m_items(items) {}

    private:
        std::string do_generate(unit const &u) const override;
        void do_mark_referenced(unit &u) const override;

        statement_number_t  m_format;
        io_list_t           m_items;
};

}

#endif
