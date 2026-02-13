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
        // The DEC PDP-10 Fortran IV Programmer's Reference says:
        // "Logical expressions are evaluated by combining the full word values
        // of P and Q (...) using the appropriate logical operator.  The result
        // is TRUE if it is arithmetically negative and FALSE if it is
        // arithmetically positive or zero."
        // In other words, the logical operations are actually implemented as
        // the corresponding bitwise operations on the values, and if you
        // interpret the result as a logical, you get the same result.  We do
        // the same by having the logic_xxx operators produce the same code as
        // the bit_xxx operators would.
        switch (op) {
            case operator_t::none:          return "not an operator";
            case operator_t::negate:        return "neg";
            case operator_t::logic_not:     [[fallthrough]];
            case operator_t::bit_not:       return "bitnot";
            case operator_t::as_logical:    return "logical";
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


void unary_node::do_mark_referenced(unit &u, unsigned &t) {
    m_node->mark_referenced(u, t);
}


std::string binary_node::do_generate_value() const {
    return std::format("{}({}, {})",
                       operator_function(m_op),
                       m_lhs->generate_value(), m_rhs->generate_value());
}

void binary_node::do_mark_referenced(unit &u, unsigned &t) {
    m_lhs->mark_referenced(u, t);
    m_rhs->mark_referenced(u, t);
}


std::string constant_node::do_generate_value() const {
    return std::format("{}", m_constant);
}


std::string variable_node::do_generate_address() const {
    return std::format("v{}", m_name);
}

void variable_node::do_mark_referenced(unit &u, unsigned &) {
    u.mark_symbol_referenced(m_name);
}


std::string temp_variable_node::do_generate_address() const {
    return std::format("((core[v{0}] = {1}), v{0})",
                       name(), m_expr->generate_value());
}

void temp_variable_node::do_mark_referenced(unit &u, unsigned &counter) {
    m_count = ++counter;
    // Note that we're not just marking the symbol as referenced, we _may_ be
    // creating it.
    auto symbol = u.find_symbol(name());
    symbol.kind = symbolkind::temporary;
    symbol.type = datatype::INTEGER;  // HACK!!!!
    symbol.referenced = true;
    u.update_symbol(symbol);
    m_expr->mark_referenced(u, counter);
}

symbol_name temp_variable_node::name() const {
    assert(m_count > 0 && "forgot to call do_mark_referenced?");
    return symbol_name{std::format("tmp{:03}", m_count)};
}


std::string external_node::do_generate_address() const {
    return std::format("v{}", m_name);
}

void external_node::do_mark_referenced(unit &u, unsigned &) {
    u.mark_symbol_referenced(m_name);
}


std::string array_subscript_node::do_generate_address() const {
    return std::format("a{}({})", m_array, formatted_subscripts(m_subscripts));
}

void array_subscript_node::do_mark_referenced(unit &u, unsigned &t) {
    u.mark_symbol_referenced(m_array);
    for (auto const &subscript : m_subscripts) {
        subscript->mark_referenced(u, t);
    }
}

std::string array_subscript_node::formatted_subscripts(
    subscript_list_t const &subscripts
) {
    assert(!subscripts.empty());
    auto formatted = std::string{};
    for (auto const &subscript : subscripts) {
        if (!formatted.empty()) formatted += ", ";
        formatted += subscript->generate_value();
    }
    return formatted;
}


std::string function_invocation_node::do_generate_value() const {
    return std::format("fn{}({})",
                       full_name(m_parent, m_function),
                       formatted_args(m_arguments));
}

void function_invocation_node::do_mark_referenced(unit &u, unsigned &t) {
    u.mark_symbol_referenced(m_function);
    for (const auto &arg : m_arguments) {
        arg->mark_referenced(u, t);
    }
    assert(m_parent.empty()          || // not an internal function
           m_parent == u.unit_name() || // internal to the unit
           m_parent == u.parent_name()  // from another internal of same parent
    );
}

std::string function_invocation_node::full_name(
    symbol_name const &parent,
    symbol_name const &function
) {
    return parent.empty()
        ? std::format("{}", function)
        : std::format("{}_{}", parent, function);
}

std::string function_invocation_node::formatted_args(
    argument_list_t const &arguments
) {
    if (arguments.empty()) return "";
    auto formatted = std::string{};
    for (const auto &arg : arguments) {
        if (!formatted.empty()) formatted.append(", ");
        formatted.append(arg->generate_address());
    }
    return formatted;
}

}
