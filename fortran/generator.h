#ifndef AID_FORTRAN_GENERATOR_H
#define AID_FORTRAN_GENERATOR_H

#include "program.h"
#include "unit.h"

#include <ostream>
#include <string_view>

namespace aid::fortran {

std::string escape_string(std::string_view s);

class generator {
    public:
        static void generate(
            std::filesystem::path const &path,
            program const &prog
        );
        static void generate(std::ostream &out, program const &prog);

        generator(std::ostream &out) :
            m_out(out) {}

    private:
        void generate_program(program const &prog) const;
        void generate_definitions() const;
        void generate_common_blocks(program const &prog) const;
        void generate_prototype(unit const &u) const;
        void generate_unit(unit const &u) const;
        void generate_variable_definition(symbol_info const &var) const;
        void generate_array_definition(symbol_info const &array) const;
        void generate_scalar_definition(symbol_info const &scalar) const;

        template <typename... Args>
        void spew(std::format_string<Args...> fmt, Args && ... args) const {
            std::print(m_out, fmt, std::forward<Args>(args)...);
        }

        std::ostream &m_out;
};

}

#endif

