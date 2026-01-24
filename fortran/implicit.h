#ifndef IMPLICIT_H
#define IMPLICIT_H

#include "symbols.h"

#include <algorithm>
#include <array>

namespace aid::fortran {

// Keeps track of the mapping from letters to data types for implementing
// Fortran's implicit typing.
class implicit {
    public:
        constexpr implicit() noexcept = default;

        void reset() noexcept { *this = implicit{}; }

        void set(char prefix, datatype type) noexcept {
            if (prefix < 'A' || 'Z' < prefix) return;
            m_table[prefix - 'A'] = type;
        }

        constexpr datatype type_for(char prefix) const noexcept {
            if (prefix < 'A' || 'Z' < prefix) return datatype::unknown;
            return m_table[prefix - 'A'];
        }

        constexpr datatype type_for(symbol_name const &name) const noexcept {
            return type_for(name.front());
        }

    private:
        std::array<datatype, 26> m_table = {
            // 'A' through 'H':
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            // 'I' through 'N':
            datatype::INTEGER, datatype::INTEGER, datatype::INTEGER,
            datatype::INTEGER, datatype::INTEGER, datatype::INTEGER,
            // 'O' through 'Z':
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL,
            datatype::REAL, datatype::REAL, datatype::REAL, datatype::REAL
        };
};

}

#endif
