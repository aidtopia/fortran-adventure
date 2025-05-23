#include "aid/experimental/fortran/datalist.h"

namespace aid::fortran {

void data_list_t::emplace_back(
    machine_word_t value,
    datatype type,
    machine_word_t count
) {
    m_items.emplace_back(value, count, type);
}

data_list_t::const_iterator data_list_t::begin() const { return iterator(m_items.begin()); }
data_list_t::const_iterator data_list_t::end()   const { return iterator(m_items.end());   }


data_list_t::iterator::iterator(vector_type::const_iterator it) :
    m_it(it), m_count(0) {}

constant_t data_list_t::iterator::operator*() const {
    return constant_t{m_it->value, m_it->type};
}

data_list_t::iterator &data_list_t::iterator::operator++() {
    if (++m_count >= m_it->count) {
        ++m_it;
        m_count = 0;
    }
    return *this;
}

data_list_t::iterator data_list_t::iterator::operator++(int) {
    auto before = *this;
    ++(*this);
    return before;
}

}
