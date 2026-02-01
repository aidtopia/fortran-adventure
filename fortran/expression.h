#ifndef AID_FORTRAN_EXPRESSION_H
#define AID_FORTRAN_EXPRESSION_H

#include "symbols.h"

#include <cassert>
#include <format>
#include <map>
#include <memory>
#include <string>

namespace aid::fortran {

enum class operator_t {
    none,
    // unary
    negate,
    posigate,       // unary +
    logic_not,
    bit_not,  // same as ones complement
    as_logical,  // casts to a LOGICAL so value is exactly .TRUE. or .FALSE.
    // binary
    add, subtract, multiply, divide,
    // remainder,      // not in Fortran
    exponentiate,   // not in Adventure, and would require special node type
    compare_eq, compare_ne, compare_lt, compare_lte, compare_gte, compare_gt,
    logic_and, logic_or, logic_eqv, logic_xor,
    bit_and, bit_or, bit_xor
};

class unit;  // forward reference

class expression_node;
using expression_t = std::shared_ptr<expression_node>;

using argument_map_t = std::map<symbol_name, expression_t>;

class expression_node {
    public:
        virtual ~expression_node() = default;

        expression_t clone(argument_map_t const &args) const {
            return do_clone(args);
        }

        std::string generate_address() const { return do_generate_address(); }
        std::string generate_value() const { return do_generate_value(); }
        void mark_referenced(unit &u, unsigned &temp_count) {
            return do_mark_referenced(u, temp_count);
        }

    private:
        virtual expression_t do_clone(argument_map_t const &args) const = 0;
        virtual std::string do_generate_address() const = 0;
        virtual std::string do_generate_value() const = 0;
        virtual void do_mark_referenced(unit &, unsigned &) {};
};

class value_expression_node : public expression_node {
    private:
        std::string do_generate_address() const override {
            assert(false && "wrap with a temp_variable_node");
            return "BUG BUG BUG";
        }
};

class address_expression_node : public expression_node {
    private:
        std::string do_generate_value() const override {
            return std::format("core[{}]", generate_address());
        }
};


}

#endif
