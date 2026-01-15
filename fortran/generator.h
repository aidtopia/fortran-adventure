#ifndef AID_FORTRAN_GENERATOR_H
#define AID_FORTRAN_GENERATOR_H

#include "program.h"
#include "unit.h"
#include "utility.h"

#include <ostream>
#include <string_view>

namespace aid::fortran {

class generator {
    public:
        static void generate(std::ostream &out, program const &prog);

        generator(std::ostream &out) : m_out(out) {}

    private:
        using core_addr = machine_word_t;
        void generate_program(program const &prog) const;
        void generate_machine_definitions() const;
        void generate_builtins(program const &prog) const;
        void generate_prototypes(program const &prog) const;
        core_addr generate_memory(program const &prog) const;
        void generate_common_blocks(program const &prog, core_addr &addr) const;
        void generate_common_block(symbol_name const &block, core_addr size, core_addr &addr) const;
        void generate_unit(unit const &u, core_addr &addr) const;
        void generate_function_signature(unit const &u) const;
        void generate_variable_definition(symbol_info const &var, core_addr &addr) const;
        void generate_array_definition(symbol_info const &array, core_addr &addr) const;
        void generate_scalar_definition(symbol_info const &scalar, core_addr &addr) const;

        template <typename... Args>
        void spew(std::format_string<Args...> fmt, Args && ... args) const {
            std::print(m_out, fmt, std::forward<Args>(args)...);
        }

        std::ostream &m_out;
};

}

#endif

