#include "symbols.h"

#include <algorithm>
#include <iterator>

namespace aid::fortran {

std::size_t array_size(array_shape const &shape) {
    auto size = std::size_t{1};
    for (auto const &dim : shape) {
        size *= dim.maximum - dim.minimum + 1;
    }
    return size;
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

void symbol_table::remove(symbol_name const &name) {
    auto const it = m_index.find(name);
    if (it == m_index.end()) return;
    m_index.erase(it);
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
