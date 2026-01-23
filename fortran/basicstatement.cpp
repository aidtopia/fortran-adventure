#include "basicstatement.h"

#include "unit.h"

#include <format>

namespace aid::fortran {

std::string basic_statement::generate(unit const &u) const {
    auto const code = do_generate(u);
    if (m_number == no_statement_number) return code;
    auto const symbol = u.find_symbol(symbol_name{static_cast<unsigned int>(m_number)});
    if (!symbol.referenced) return code;
    return std::format("L{0}: {1}", m_number, code);
    //return std::format("L{0}: puts(\"L{0}:\"); {1}", m_number, code);
}

void basic_statement::mark_reachable(unit &u) {
    if (m_reachable) return;
    m_reachable = true;
    do_mark_reachable(u);
}

std::string basic_statement::do_generate(unit const &) const { return ""; }
void basic_statement::do_mark_reachable(unit &) const {}

}
