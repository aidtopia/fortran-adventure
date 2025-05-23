#include "unit.h"

#include <cassert>
#include <format>
#include <memory>
#include <print>

namespace aid::fortran {

unit::~unit() {}

void unit::update_symbol(symbol_info const &symbol) {
    m_symbols.update(symbol);
}

void unit::update_symbol(symbol_info &&symbol) {
    m_symbols.update(std::move(symbol));
}

void unit::mark_symbol_referenced(symbol_name const &name) {
    auto const i = m_symbols.find(name);
    assert(i != symbol_table::npos);
    m_symbols[i].referenced = true;
}

void unit::infer_types() {
    for (std::size_t i = 0; i < m_symbols.size(); ++i) {
        auto &symbol = m_symbols[i];
        if (symbol.type == datatype::unknown) {
            symbol.type = implicit_type(symbol.name);
        }
    }
}

void unit::add_statement(statement_t statement) {
    m_code.push_back(statement);
    statement->mark_referenced(*this);
}

void unit::set_implicit_type(char prefix, datatype type) {
    if (prefix < 'A' || 'Z' < prefix) return;
    m_implicit_type_table[prefix - 'A'] = type;
}

bool unit::has_symbol(symbol_name const &name) const {
    return m_symbols.has(name);
}

symbol_info unit::find_symbol(symbol_name const &name) const {
    return m_symbols.get(name);
}

std::vector<symbol_info> unit::extract_symbols() const {
    return m_symbols.extract();
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

void unit::add_format(statement_number_t number, field_list_t const &fields) {
    assert(m_formats.find(number) == m_formats.end() && "FORMAT defined twice");
    m_formats[number] = fields;
}

void unit::print_symbol_table() const {
    auto constexpr k_bang       =  1;
    auto constexpr k_symbol     =  6;
    auto constexpr k_kind       = 10;
    auto constexpr k_comdat     =  6;
    auto constexpr k_index      =  3;
    auto constexpr k_type       =  8;
    auto constexpr k_dimens     =  8;
    auto constexpr k_subtotal   =
        k_bang + k_symbol + 1 + k_kind + 1 + k_comdat + 1 + k_index + 1 +
        k_type + 1 + k_dimens + 1;
    auto constexpr k_init_data  = 72 - k_subtotal;
    auto constexpr k_total      = k_subtotal + k_init_data;
    auto constexpr k_format     =
        /* k_comdat + 1 + k_index + 1 + k_type + 1 + k_dimens + 1 + */ k_init_data;

    std::print("{:-^{}}\n", unit_name(), k_total);
    std::print("{:-^{}}{:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}} {:-^{}}\n",
               "", k_bang, "symbol", k_symbol, "kind", k_kind,
               "comdat", k_comdat, "idx", k_index, "type", k_type,
               "dimens", k_dimens, "initial data", k_init_data);
    auto if_any = [] (symbol_info const &) { return true; };
    auto sort_order =
        [] (symbol_info const &a, symbol_info const &b) {
            if (a.name < b.name) return true;
            if (a.name > b.name) return false;
            return false;
        };

    auto const copies = extract_symbols(if_any, sort_order);
    for (auto const &s : copies) { 
        auto const comdat =
            s.kind != symbolkind::common ? "" :
            s.comdat.empty() ? "//" : std::format("{}", s.comdat);
        auto const idx = s.index > 0 ? std::format("{}", s.index) : "";
        std::print("{:{}}{:<{}} {:<{}} {:<{}} {:>{}} {:<{}} ",
                   s.referenced ? "" : "!", k_bang,
                   s.name, k_symbol, s.kind, k_kind, comdat, k_comdat,
                   idx, k_index, s.type, k_type);
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
        std::print("{:<{}} ", dimensions, k_dimens);

        if (!s.init_data.empty()) {
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
            std::print("{}", data);
        }
        std::print("\n");
    }
    for (auto const &[number, fields] : m_formats) {
        std::print("{:{}}{:>{}} {:{}} {:{}} {:{}} {:{}} {:{}} ",
                   "", k_bang, number, k_symbol, "FORMAT", k_kind,
                   "", k_comdat, "", k_index, "", k_type, "", k_dimens);
        if (fields.size() > k_format) {
            std::print("{}...\n", std::string_view(fields.data(), k_format-3));
        } else {
            std::print("{}\n", fields);
        }
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
