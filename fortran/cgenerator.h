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

        c_generator() : m_initializers{} {}

    private:
        std::string generate_program(program const &prog);
        std::string generate_core_memory(program const &prog);
        std::string generate_builtins(program const &prog);
        std::string generate_prototypes(program const &prog);
        std::string generate_common_blocks(program const &prog);
        std::string generate_main_function(program const &prog);
        std::string generate_subprograms(program const &prog);
        std::string generate_static_initialization();

        std::string generate_unit(unit const &u);
        std::string generate_function_signature(unit const &u);
        std::string generate_statements(unit const &u);
        std::string generate_return_value(unit const &u);
        std::string generate_dummies(unit const &u);
        std::string generate_common_variable_declarations(unit const &u);
        std::string generate_local_variable_declarations(unit const &u);
        std::string generate_format_specifications(unit const &u);

        std::string generate_variable_definition(symbol_info const &var);
        std::string generate_array_definition(symbol_info const &var);
        std::string generate_scalar_definition(symbol_info const &var);

        void add_initializer(symbol_info const &symbol);

        // The runtime support consists of static text.
        static constexpr std::string_view external_dependencies();
        static constexpr std::string_view machine_definitions();
        static constexpr std::string_view io_subsystem();
        static constexpr std::string_view kron_subsystem();
        static constexpr std::string_view host_subsystem();
        static constexpr std::string_view usage_function();

        // Static initialization cannot be done where the variables are defined,
        // so save them until generate_static_initialization creates a function
        // called at startup.
        std::vector<symbol_info> m_initializers;
};

}

#endif


