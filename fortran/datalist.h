#ifndef AID_FORTRAN_DATALIST_H
#define AID_FORTRAN_DATALIST_H

#include "machine.h"
#include "symbols.h"

#include <bit>
#include <vector>

namespace aid::fortran {

struct constant_t {
    constant_t(machine_word_t v, datatype t) :
        value((v << (64-36)) >> (64-36)), type(t) {};
    explicit constant_t(float real) :
        value((static_cast<machine_word_t>(std::bit_cast<std::uint32_t>(real))<<32)>>32),
        type(datatype::REAL) {};
    machine_word_t value;
    datatype       type;
};

class data_list_t {
    public:
        void emplace_back(
            machine_word_t value,
            datatype type,
            machine_word_t count = 1
        );

        struct data_item {
            machine_word_t value;
            machine_word_t count;
            datatype       type;
        };

        using vector_type = std::vector<data_item>;

        class iterator {
            public:
                explicit iterator(vector_type::const_iterator it);
                bool operator==(iterator const &rhs) const = default;
                constant_t operator*() const;
                iterator &operator++();
                iterator operator++(int);

            private:
                vector_type::const_iterator m_it;
                machine_word_t m_count;
        };

        using const_iterator = iterator;
        const_iterator begin() const;
        const_iterator end()   const;

    private:
        vector_type m_items;
};

}

#endif
