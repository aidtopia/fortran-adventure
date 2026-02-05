#include "comdats.h"
#include "program.h"
#include "unit.h"

#include <algorithm>

namespace aid::fortran {

comdat_table common_blocks(unsigned &base_address, program const &prog) {
    // Within a subprogram, a set of variables in the same common block
    // determines the required size of that block.  If two or more subprograms
    // require different sizes for the same block, the first one loaded
    // should determine the size.  If a later subprogram needs a larger size,
    // it's officially an error, but we'll just use the largest.
    auto comdats = comdat_table{};

    auto update = [&comdats](symbol_name const &block, unsigned size) {
        auto const prior_size =
            comdats.contains(block) ? comdats[block].size : 0u;
        comdats[block].size = std::max(size, prior_size);
    };

    for (auto const &sub : prog) {
        auto const commons = sub.extract_symbols(is_common, by_block_index);
        auto block = symbol_name{};
        auto size = 0u;
        for (auto const &var : commons) {
            if (var.comdat != block) {
                update(block, size);
                block = var.comdat;
                size = 0u;
            }
            size += core_size(var);
        }
        if (size > 0u) update(block, size);
    }

    // Now that we know the necessary sizes, we can set their base addresses.
    // Note the base is passed by reference, so the caller not only gets the
    // table, but the next available address.
    for (auto &[n, range] : comdats) {
        range.base = base_address;
        base_address += range.size;
    }

    return comdats;
}

}
