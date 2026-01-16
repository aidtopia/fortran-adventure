#ifndef AID_FORTRAN_CGENERATOR_H
#define AID_FORTRAN_CGENERATOR_H

#include "program.h"
#include "unit.h"
#include "utility.h"

#include <format>
#include <span>
#include <string>
#include <string_view>

namespace aid::fortran {

class c_generator {
    public:
        static std::string generate_c(program const &prog);

        c_generator() {}

    private:
        std::string generate_program(program const &prog);
        std::string generate_builtins(program const &prog);
        std::string generate_prototypes(program const &prog);
        std::string generate_common_blocks(program const &prog);
        std::string generate_common_block(symbol_name const &block, machine_word_t size);
        std::string generate_subprograms(program const &prog);
        std::string generate_main_function(program const &prog);

        std::string generate_unit(unit const &u);
        std::string generate_function_signature(unit const &u);
        std::string generate_statements(unit const &u);
        std::string generate_return_value(unit const &u);
        std::string generate_dummies(unit const &u);
        std::string generate_common_variable_declarations(unit const &u);
        std::string generate_local_variable_declarations(unit const &u);
        std::string generate_format_specifications(unit const &u);
        std::string generate_return_statement(unit const &u);

        std::string generate_variable_definition(symbol_info const &var);
        std::string generate_array_definition(symbol_info const &array);
        std::string generate_scalar_definition(symbol_info const &scalar);

        // The runtime support consists of static text.
        static constexpr std::string_view machine_definitions();
        static constexpr std::string_view io_subsystem();
        static constexpr std::string_view kron_subsystem();
        static constexpr std::string_view usage_function();

        machine_word_t m_memsize = 0;
};

}

#endif


