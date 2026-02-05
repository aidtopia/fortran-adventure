#include "unit.h"

#include <cassert>
#include <format>
#include <memory>
#include <print>
#include <set>
#include <string_view>

using namespace std::string_view_literals;

namespace aid::fortran {

// entry is a fake symbol name used in unit::m_targets to identify the first
// executable statement in the subprogram.
auto constexpr entry = symbol_name{"_ENTRY"};

void unit::set_unit_name(symbol_name const &name) {
    m_unit_name = name;
    // This unit's internal functions know their parent name, so update those.
    for (auto &internal : m_internals) {
        internal.set_parent_name(name);
    }
}

void unit::set_parent_name(symbol_name const &name) {
    m_parent_name = name;
}

std::string unit::full_name() const {
    if (m_parent_name.empty()) return std::format("{}", m_unit_name);
    return std::format("{}_{}", m_parent_name, m_unit_name);
}

void unit::update_symbol(symbol_info const &symbol) {
    m_symbols.update(symbol);
}

void unit::update_symbol(symbol_info &&symbol) {
    m_symbols.update(std::move(symbol));
}

void unit::mark_symbol_referenced(symbol_name const &name) {
    auto const i = m_symbols.find(name);
    if (i != symbol_table::npos) m_symbols[i].referenced = true;
}

void unit::add_statement(statement_t statement) {
    if (m_targets.empty()) m_targets[entry] = 0uz;
    if (auto const number = statement->get_statement_number();
        number != no_statement_number
    ) {
        assert(!m_targets.contains(number) && "duplicate statement number!");
        m_targets[symbol_name{number}] = m_code.size();
    }
    m_code.push_back(statement);
}

bool unit::has_symbol(symbol_name const &name) const {
    return m_symbols.has(name);
}

symbol_info unit::find_symbol(symbol_name const &name) const {
    return m_symbols.get(name);
}

std::vector<symbol_info> unit::extract_symbols(
    std::function<bool (symbol_info const &)> predicate
) const {
    return m_symbols.extract_if(predicate);
}

std::vector<symbol_info> unit::extract_symbols(
    std::function<bool (symbol_info const &)> choose_if,
    std::function<bool (symbol_info const &, symbol_info const &)> sort_by 
) const {
    auto symbols = extract_symbols(choose_if);
    std::sort(symbols.begin(), symbols.end(), sort_by);
    return symbols;
}

void unit::add_format(symbol_name label, field_list_t &&fields) {
    assert(m_formats.find(label) == m_formats.end() && "FORMAT defined twice");
    m_formats[label] = std::move(fields);
}

void unit::add_format(statement_number_t number, field_list_t &&fields) {
    add_format(symbol_name{number}, std::move(fields));
}

field_list_t unit::find_format(symbol_name const &label) const {
    auto const it = m_formats.find(label);
    return it != m_formats.end() ? it->second : field_list_t{};
}

void unit::add_internal(unit &&internal) {
    m_internals.push_back(std::move(internal));
}

void unit::add_subroutine_pointer_type(std::size_t arg_count) {
    assert(arg_count < sizeof(m_subroutine_types)*CHAR_BIT);
    m_subroutine_types |= (1u << arg_count);
}

void unit::add_function_pointer_type(std::size_t arg_count) {
    assert(arg_count < sizeof(m_function_types)*CHAR_BIT);
    m_function_types |= (1u << arg_count);
}

void unit::mark_reachable() {
    if (m_reachable) return;  // already done
    m_reachable = true;
    if (m_code.empty()) return;

    auto is_branch_target = [] (symbol_info const &s) {
        return s.kind == symbolkind::label && s.referenced;
    };

    auto processed = std::set<symbol_name>{};
    auto to_process = std::set<symbol_name>{entry};

    while (!to_process.empty()) {
        auto const this_round = to_process;  // copy to avoid iter invalidation
        for (auto target : this_round) {
            to_process.erase(target);
            processed.insert(target);
            if (auto it = m_targets.find(target); it != m_targets.end()) {
                auto index = it->second;
                while (index < m_code.size()) {
                    auto const &statement = m_code[index++];
                    if (statement->is_reachable()) break;
                    statement->mark_reachable(*this);
                    if (!statement->may_proceed()) break;
                }
                // Add newly discovered branch targets.
                for (auto const &label : extract_symbols(is_branch_target)) {
                    if (!processed.contains(label.name)) {
                        to_process.insert(label.name);
                    }
                }
            }
        }
    }

    // TODO:  Generalize the function for finding all of the reachable units
    // for a program so we can re-use it here to find all of the reachable
    // internal units.  For now, though, ...
    for (auto &internal : m_internals) {
        internal.mark_reachable();
    }
}

void unit::print_symbol_table(std::ostream &out) const {
    auto constexpr k_table_width = 80;
    auto constexpr k_bang       =  1;
    auto constexpr k_symbol     =  6;
    auto constexpr k_kind       = 10;
    auto constexpr k_comdat     =  6;
    auto constexpr k_index      =  3;
    auto constexpr k_type       =  8;
    auto constexpr k_address    =  6;
    auto constexpr k_dimens     =  8;
    auto constexpr k_subtotal   =
        k_bang + k_symbol + 1 + k_kind + 1 + k_comdat + 1 + k_index + 1 +
        k_type + 1 + k_address + 1 + k_dimens + 1;
    auto constexpr k_init_data  = k_table_width - k_subtotal;
    auto constexpr k_format     = k_init_data;

    auto const name = std::format("{}{}", m_reachable ? "" : "!", full_name());
    std::print(out, "{:-^{}}\n", name, k_table_width);
    std::print(out,
        "{:-^{}}{:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}}\n",
        "", k_bang, "symbol", k_symbol, "kind", k_kind, "comdat", k_comdat,
        "idx", k_index, "type", k_type, "addr", k_address,
        "dimens", k_dimens, "initial data", k_init_data);
    auto is_any = [] (symbol_info const &) { return true; };

    auto const copies = extract_symbols(is_any, by_name);
    for (auto const &s : copies) { 
        auto const comdat =
            s.kind == symbolkind::common && s.comdat.empty()
                ? "//"sv
                : s.comdat.view();
        auto const idx = s.index > 0 ? std::format("{}", s.index) : "";
        std::print(out, "{:{}}{:<{}} {:<{}} {:<{}} {:>{}} {:<{}} ",
                   s.referenced ? "" : "!", k_bang,
                   s.name, k_symbol, s.kind, k_kind, comdat, k_comdat,
                   idx, k_index, s.type, k_type);

        auto address = std::string{};
        if (s.address != 0) address = std::format("{}", s.address);
        std::print(out, "{:>{}} ", address, k_address);

        auto dimensions = std::string{};
        if (!s.shape.empty()) {
            dimensions = "(";
            dimensions += format_dimension(s.shape.front());
            for (std::size_t j = 1; j != s.shape.size(); ++j) {
                dimensions.push_back(',');
                dimensions += format_dimension(s.shape[j]);
            }
            dimensions.push_back(')');
        }
        std::print(out, "{:<{}} ", dimensions, k_dimens);

        if (s.kind == symbolkind::format) {
            auto const it = m_formats.find(s.name);
            auto const &fields =
                it == m_formats.end() ? "missing format!" : it->second;
            if (fields.size() > k_format) {
                std::print(out, "{}...",
                           std::string_view(fields.data(), k_format-3));
            } else {
                std::print(out, "{}", fields);
            }
        } else if (!s.init_data.empty()) {
            auto data = std::string{};
            data = format_word(s.init_data[0], s.type);
            for (std::size_t j = 1; j < s.init_data.size(); ++j) {
                data.push_back(',');
                auto const word = format_word(s.init_data[j], s.type);
                if (data.size() + word.size() >= k_init_data-3) {
                    data.append("...");
                    break;
                }
                data += word;
            }
            std::print(out, "{}", data);
        }
        std::print(out, "\n");
    }
    for (auto const &internal : m_internals) {
        internal.print_symbol_table(out);
    }
}

std::string unit::format_dimension(dimension const &d) {
    if (d.minimum == 1) return std::format("{}", d.maximum);
    return std::format("{}/{}", d.minimum, d.maximum);
}

std::string unit::format_word(machine_word_t w, datatype type) {
    switch (type) {
        case datatype::LITERAL:
            return std::format("'{}'", unpack_literal(w));
        case datatype::LOGICAL:
            return decode_logical(w);
        case datatype::INTEGER:
        default:
            return looks_literal(w) ? std::format("'{}'", unpack_literal(w))
                                    : std::format("{}", w);
    }
}

}
