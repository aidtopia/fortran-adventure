#include "aid/experimental/fortran/machine.h"

#include <cassert>
#include <cmath>
#include <type_traits>

namespace aid::fortran {

std::string unpack_literal(machine_word_t word) {
    // Up to five 7-bit ASCII characters can be packed into a machine word.
    auto constexpr max_length    = 5u;
    auto constexpr bits_per_char = 7u;  // 7-bit ASCII only

    using mask_t = std::make_unsigned_t<machine_word_t>;
    auto constexpr char_mask = ((mask_t{1} << bits_per_char) - 1);

    auto result = std::string{};
    for (auto i = 0; i < max_length; ++i) {
        auto const offset = machine_word_bits - (i + 1)*bits_per_char;
        auto const ch = static_cast<char>((word >> offset) & char_mask);
        result.push_back(ch);
    }

    return result;
}

bool looks_literal(machine_word_t w) {
    auto constexpr one_space = make_literal(" ");
    if (std::abs(w) < one_space) return false;
    auto const s = unpack_literal(w);
    for (auto const ch : s) {
        if (ch < ' ' || '\x7E' < ch) return false;
    }
    return true;
}

std::string decode_logical(machine_word_t word) {
    return (word < 0) ? ".TRUE." : ".FALSE.";
}

}
