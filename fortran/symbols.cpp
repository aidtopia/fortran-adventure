#include "symbols.h"

#include <algorithm>
#include <cassert>
#include <iterator>

namespace aid::fortran {

std::size_t memory_size(datatype type) {
    switch (type) {
        case datatype::INTEGER:
        case datatype::LITERAL:
        case datatype::LOGICAL:
        case datatype::REAL:
        case datatype::subptr :
            return 1;
        case datatype::DOUBLE:
        case datatype::COMPLEX:
            assert(!"values > 1 machine word are not currently supported");
            return 2;
        case datatype::unknown:
        case datatype::none:
            assert(!"suspicious!");
            return 0;
        default:
            assert(!"forget to handle a new datatype?");
            return 0;
    }
}

std::size_t array_size(array_shape const &shape) {
    auto size = std::size_t{1};
    for (auto const &dim : shape) {
        size *= dim.maximum - dim.minimum + 1;
    }
    return size;
}

std::size_t memory_size(symbol_info const &symbol) {
    switch (symbol.kind) {
        case symbolkind::local:
        case symbolkind::common:
        case symbolkind::argument:
        case symbolkind::retval:
            return memory_size(symbol.type) * array_size(symbol.shape);

        case symbolkind::subprogram:
        case symbolkind::internal:
        case symbolkind::external:
        case symbolkind::label:
            return 0;

        case symbolkind::shadow:
            assert(!"suspicious!");
            return 0;

        default:
            assert(!"forget to handle a new symbolkind?");
            return 0;
    }
}

symbol_info symbol_table::get(symbol_name const &name) const {
    if (auto const i = find(name); i != npos) return m_symbols[i];
    return symbol_info{.name = name};
}

std::size_t symbol_table::update(symbol_info const &symbol) {
    if (auto const it = m_index.find(symbol.name); it != m_index.end()) {
        auto const i = it->second;
        m_symbols[i] = symbol;
        return i;
    }
    auto const i = m_symbols.size();
    m_symbols.push_back(symbol);
    m_index[symbol.name] = i;
    return i;
}

std::size_t symbol_table::update(symbol_info &&symbol) {
    if (auto const it = m_index.find(symbol.name); it != m_index.end()) {
        auto const i = it->second;
        m_symbols[i] = std::move(symbol);
        return i;
    }
    auto const i = m_symbols.size();
    m_index[symbol.name] = i;
    m_symbols.push_back(std::move(symbol));
    return i;
}

std::vector<std::size_t> symbol_table::indices_sorted_by_name() const {
    std::vector<std::size_t> indices;
    for (auto const &entry : m_index) { indices.push_back(entry.second); }
    return indices;
}

std::vector<symbol_info> symbol_table::extract() const {
    std::vector<symbol_info> copies;
    copies.reserve(m_symbols.size());
    std::copy(m_symbols.begin(), m_symbols.end(), std::back_inserter(copies));
    return copies;
}

std::vector<symbol_info> symbol_table::extract_if(
    std::function<bool (symbol_info const &)> predicate
) const {
    std::vector<symbol_info> copies;
    std::copy_if(m_symbols.begin(), m_symbols.end(), std::back_inserter(copies),
                 predicate);
    return copies;
}

}
