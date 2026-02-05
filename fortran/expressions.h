#ifndef AID_FORTRAN_EXPRESSIONS_H
#define AID_FORTRAN_EXPRESSIONS_H

#include "expression.h"
#include "symbols.h"

#include <cassert>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace aid::fortran {

using argument_list_t = std::vector<expression_t>;
using subscript_list_t = std::vector<expression_t>;

class unary_node : public value_expression_node {
    public:
        unary_node(operator_t op, expression_t node) : m_op(op), m_node(node) {}

    private:
        std::string do_generate_value() const override;
        void do_mark_referenced(unit &, unsigned &) override;

        operator_t m_op;
        expression_t m_node;
};

class binary_node : public value_expression_node {
    public:
        binary_node(expression_t lhs, operator_t op, expression_t rhs) :
            m_lhs(lhs), m_op(op), m_rhs(rhs) {}

    private:
        std::string do_generate_value() const override;
        void do_mark_referenced(unit &, unsigned &) override;

        expression_t m_lhs;
        operator_t m_op;
        expression_t m_rhs;
};

class constant_node : public value_expression_node {
    public:
        constant_node(machine_word_t constant) : m_constant(constant) {}

    private:
        std::string do_generate_value() const override;

        machine_word_t m_constant;
};

class variable_node : public address_expression_node {
    public:
        variable_node(symbol_name const &name) : m_name(name) {}

    private:
        std::string do_generate_address() const override;
        void do_mark_referenced(unit &, unsigned &) override;

        symbol_name m_name;
};

class temp_variable_node : public address_expression_node {
    public:
        temp_variable_node(expression_t expr) : m_count(0), m_expr(expr) {}

    private:
        std::string do_generate_address() const override;
        void do_mark_referenced(unit &, unsigned &) override;

        symbol_name name() const;

        unsigned m_count;
        expression_t m_expr;
};

class external_node : public address_expression_node {
    public:
        external_node(symbol_name const &name) : m_name(name) {}

    private:
        std::string do_generate_address() const override;
        void do_mark_referenced(unit &, unsigned &) override;

        symbol_name m_name;
};

class array_index_node : public address_expression_node {
    public:
        array_index_node(
            symbol_name const &name,
            array_shape const &shape,
            subscript_list_t const &subscripts
        ) :
            m_array(name),
            m_index_expr(make_index_expression(subscripts, shape)) {}

        array_index_node(
            symbol_name const &name,
            expression_t index_expr
        ) : m_array(name), m_index_expr(index_expr) {}

    private:
        std::string do_generate_address() const override;
        void do_mark_referenced(unit &, unsigned &) override;
        
        static expression_t make_index_expression(
            subscript_list_t const &subscripts,
            array_shape const &shape
        );

        static expression_t zero_based(
            expression_t index,
            dimension const &dimen
        );

        static expression_t scale_up(expression_t index, machine_word_t scale);

        symbol_name m_array;
        expression_t m_index_expr;
};

class function_invocation_node : public value_expression_node {
    public:
        function_invocation_node(
            symbol_name const &function,
            argument_list_t const &arguments
        ) : m_parent(), m_function(function), m_arguments(arguments) {}
        function_invocation_node(
            symbol_name const &parent,
            symbol_name const &function,
            argument_list_t const &arguments
        ) : m_parent(parent), m_function(function), m_arguments(arguments) {
            assert(!m_parent.empty() && "wrong c'tor if no parent");
        }

    private:
        std::string do_generate_value() const override;
        void do_mark_referenced(unit &, unsigned &) override;

        static std::string formatted_args(argument_list_t const &arguments);
        static std::string full_name(
            symbol_name const &parent,
            symbol_name const &function
        );

        symbol_name m_parent;  // non-empty for internal functions
        symbol_name m_function;
        argument_list_t m_arguments;
};

}

#endif
