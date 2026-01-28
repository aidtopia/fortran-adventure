#ifndef AID_FORTRAN_EXPRESSION_H
#define AID_FORTRAN_EXPRESSION_H

#include <format>
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

class expression_node {
    public:
        virtual ~expression_node() = default;
        std::string generate_address() const { return do_generate_address(); }
        std::string generate_value() const { return do_generate_value(); }
        void mark_referenced(unit &u, unsigned &temp_count) {
            return do_mark_referenced(u, temp_count);
        }

    private:
        virtual std::string do_generate_address() const {
            // This default implementation is appropriate for many but not all
            // expression node types.
            return std::format("tmp({})", do_generate_value());
        }
        virtual std::string do_generate_value() const = 0;
        virtual void do_mark_referenced(unit &, unsigned &) {};
};

using expression_t = std::shared_ptr<expression_node>;

}

#endif
