#ifndef AID_FORTRAN_MACHINE_H
#define AID_FORTRAN_MACHINE_H

#include <cstdint>
#include <string>
#include <string_view>

namespace aid::fortran {

// The PDP-10 has 36-bit machine words.
auto constexpr machine_word_bits = 36u;

// We don't have 36-bit integers, so we're using a larger size.
using machine_word_t = std::int64_t;
static_assert(sizeof(machine_word_t)*CHAR_BIT >= machine_word_bits);

// A literal is a short character string packed into a machine word.  This is
// the `A5` input-output format.
machine_word_t constexpr make_literal(std::string_view text) {
    // Up to five 7-bit ASCII characters can be packed into a machine word.
    auto constexpr bits_per_char = 7u;  // 7-bit ASCII only
    auto constexpr max_length    = machine_word_bits / bits_per_char;
    auto constexpr representation_bits = sizeof(machine_word_t) * CHAR_BIT;

    // If more than max_length characters in text, drop the leading ones.
    auto result = machine_word_t{0};
    for (auto const ch : text) {
        result <<= 7;
        result |= static_cast<unsigned char>(ch) & 0x7F;
    }

    // If fewer than max_length characters, pad with spaces.
    for (auto i = text.size(); i < max_length; ++i) {
        result <<=7;
        result |= ' ';
    }

    // The first character occupies the topmost bits of the machine word.  The
    // next character is packed just below, and so on.  If bits_per_char doesn't
    // divide evenly into machine_word_bits, we'll need to shift up, leaving the
    // leftover bottom bits 0.
    auto constexpr shift_for_unused_bits =
        machine_word_bits - (max_length * bits_per_char);

    // Machine words are signed, so the MSB of the first character determines
    // whether the literal is negative.  Since we're storing machine words in
    // wider type, we shift everything up and back in order to get sign
    // extension.  The Adventure code checks the sign of literals, so this is
    // important.
    auto constexpr shift_for_sign_extension =
        representation_bits - machine_word_bits;
    static_assert(0 <= shift_for_sign_extension &&
                       shift_for_sign_extension < representation_bits);

    result <<= shift_for_sign_extension + shift_for_unused_bits;
    result >>= shift_for_sign_extension;
    return result;
}

std::string unpack_literal(machine_word_t word);
bool looks_literal(machine_word_t w);

std::string decode_logical(machine_word_t word);

inline auto constexpr LOGICAL_TRUE  = machine_word_t{-1};
inline auto constexpr LOGICAL_FALSE = machine_word_t{0};

}

#endif
