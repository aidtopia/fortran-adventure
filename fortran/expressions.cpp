#include "expressions.h"

#include "unit.h"

#include <cassert>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace aid::fortran {

namespace {

    static inline std::string_view operator_function(operator_t op) {
        switch (op) {
            case operator_t::none:          return "not an operator";
            case operator_t::negate:        return "neg";
            case operator_t::logic_not:     [[fallthrough]];
            case operator_t::bit_not:       return "bitnot";
            case operator_t::add:           return "add";
            case operator_t::subtract:      return "sub";
            case operator_t::multiply:      return "mul";
            case operator_t::divide:        return "dvd";  // CRT lib has div
            //case operator_t::remainder:     return "rem";
            //case operator_t::exponentiate:
            //    return "exponentiate is not supported";
            case operator_t::compare_eq:    return "eq";
            case operator_t::compare_ne:    return "ne";
            case operator_t::compare_lt:    return "lt";
            case operator_t::compare_lte:   return "le";
            case operator_t::compare_gte:   return "ge";
            case operator_t::compare_gt:    return "gt";
            case operator_t::logic_and:     [[fallthrough]];
            case operator_t::bit_and:       return "bitand";
            case operator_t::logic_or:      [[fallthrough]];
            case operator_t::bit_or:        return "bitor";
            case operator_t::logic_xor:     [[fallthrough]];
            case operator_t::bit_xor:       return "bitxor";
        };
        return "missing operator text";
    }

}  // anonymous namespace

std::string unary_node::do_generate_value() const {
    return std::format("{}({})",
                       operator_function(m_op), m_node->generate_value());
}

void unary_node::do_mark_referenced(unit &u) const {
    m_node->mark_referenced(u);
}


std::string binary_node::do_generate_value() const {
    return std::format("{}({}, {})",
                       operator_function(m_op),
                       m_lhs->generate_value(), m_rhs->generate_value());
}

void binary_node::do_mark_referenced(unit &u) const {
    m_lhs->mark_referenced(u);
    m_rhs->mark_referenced(u);
}


std::string constant_node::do_generate_value() const {
    return std::format("{}", m_constant);
}


std::string variable_node::do_generate_reference() const {
    return std::format("v{}", m_name);
}

std::string variable_node::do_generate_value() const {
    return std::format("*{}", do_generate_reference());
}

void variable_node::do_mark_referenced(unit &u) const {
    u.mark_symbol_referenced(m_name);
}


std::string external_node::do_generate_reference() const {
    return std::format("(word_t *){}", do_generate_value());
}

std::string external_node::do_generate_value() const {
    // TODO:  Why assume sub?  Couldn't it be fn?
    return std::format("sub{}", m_name);
}

void external_node::do_mark_referenced(unit &u) const {
    u.mark_symbol_referenced(m_name);
}


std::string array_index_node::do_generate_reference() const {
    return std::format("(v{} + {})", m_array, m_index_expr->generate_value());
}

std::string array_index_node::do_generate_value() const {
    return std::format("*{}", do_generate_reference());
}

void array_index_node::do_mark_referenced(unit &u) const {
    u.mark_symbol_referenced(m_array);
    m_index_expr->mark_referenced(u);
}


// Returns an expression to compute the 1-dimensional index into a flat array
// from indices into a hypothetical N-dimensional array.
expression_t array_index_node::make_index_expression(
    argument_list_t const &indices,
    array_shape const &shape
) {
    assert(!shape.empty() && shape.size() == indices.size());
    auto result = zero_based(indices[0], shape[0]);
    auto scale = shape[0].size();
    for (std::size_t i = 1; i < shape.size(); ++i) {
        auto const component =
            scale_up(zero_based(indices[i], shape[i]), scale);
        result =
            std::make_shared<binary_node>(result, operator_t::add, component);
        scale *= shape[i].size();
    }
    return result;
}

expression_t array_index_node::zero_based(
    expression_t index,
    dimension const &dimen
) {
    auto const dim_min = std::make_shared<constant_node>(dimen.minimum);
    return std::make_shared<binary_node>(index, operator_t::subtract, dim_min);
}

expression_t array_index_node::scale_up(
    expression_t index,
    machine_word_t scale
) {
    if (scale == 1) return index;
    auto const s = std::make_shared<constant_node>(scale);
    return std::make_shared<binary_node>(index, operator_t::multiply, s);
}


std::string function_invocation_node::do_generate_value() const {
    return std::format("EVAL(fn{}({}))",
                       m_function, formatted_args(m_arguments));
}

void function_invocation_node::do_mark_referenced(unit &u) const {
    u.mark_symbol_referenced(m_function);
    for (const auto &arg : m_arguments) {
        arg->mark_referenced(u);
    }
}

std::string function_invocation_node::formatted_args(
    argument_list_t const &arguments
) {
    if (arguments.empty()) return "";
    auto formatted = std::string{};
    for (const auto &arg : arguments) {
        if (!formatted.empty()) formatted.append(", ");
        formatted.append(arg->generate_reference());
    }
    return formatted;
}

}
