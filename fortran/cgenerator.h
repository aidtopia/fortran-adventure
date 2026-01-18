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

        c_generator() : m_memsize{0}, m_init_data{} {}

    private:
        using init_data_t = std::vector<machine_word_t>;

        std::string generate_program(program const &prog);
        std::string generate_builtins(program const &prog);
        std::string generate_prototypes(program const &prog);
        std::string generate_common_blocks(program const &prog);
        std::string generate_common_block(
            symbol_name const &block,
            machine_word_t size
        );
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
        std::string generate_return_statement(unit const &u);

        std::string generate_variable_definition(
            symbol_info const &var,
            machine_word_t offset
        );
        std::string generate_array_definition(
            symbol_info const &var,
            machine_word_t offset
        );
        std::string generate_scalar_definition(
            symbol_info const &var,
            machine_word_t offset
        );

        void add_initializer(
            symbol_name block,
            machine_word_t offset,
            init_data_t data
        );

        // The runtime support consists of static text.
        static constexpr std::string_view machine_definitions();
        static constexpr std::string_view io_subsystem();
        static constexpr std::string_view kron_subsystem();
        static constexpr std::string_view host_subsystem();
        static constexpr std::string_view usage_function();

        // As the C code is generated, m_memsize keeps track of the next
        // available address in the program memory.
        machine_word_t m_memsize = 0;

        // When static initialization cannot be done where a variable is
        // defined, the initialization data is stored in m_init_data.  It's
        // then used by generate_static_initialization to create a function
        // called at startup.
        struct initializer {
            symbol_name block;  // COMDAT name or empty for regular memory
            machine_word_t offset;  // relative to start of block
            init_data_t values;
        };
        std::vector<initializer> m_init_data;
};

}

#endif


