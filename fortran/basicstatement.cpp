#include "aid/experimental/fortran/basicstatement.h"

#include "aid/experimental/fortran/unit.h"

#include <format>

namespace aid::fortran {

std::string basic_statement::generate(unit const &u) const {
    auto const code = do_generate(u);
    if (m_number == no_statement_number) return code;
    auto const symbol = u.find_symbol(symbol_name{m_number});
    if (!symbol.referenced) return code;
    return std::format("L{0}: {1}", m_number, code);
    //return std::format("L{0}: puts(\"L{0}:\"); {1}", m_number, code);
}

void basic_statement::mark_referenced(unit &u) const { do_mark_referenced(u); }

std::string basic_statement::do_generate(unit const &) const { return ""; }
void basic_statement::do_mark_referenced(unit &) const {}

}
