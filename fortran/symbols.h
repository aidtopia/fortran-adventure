#ifndef AID_FORTRAN_SYMBOL_H
#define AID_FORTRAN_SYMBOL_H

#include "machine.h"

#include <algorithm>
#include <array>
#include <charconv>
#include <format>
#include <functional>
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace aid::fortran {

enum class datatype {
    unknown,  // so far ...
    none,     // SUBROUTINEs don't have a type
    // basic types
    INTEGER,  // machine word-sized signed integer
    REAL,     // experimenting with partial implementation, not supported
    DOUBLE,   // not implemented
    COMPLEX,  // not implemented
    LOGICAL,  // a bool essentially
    LITERAL   // a short character string, interchangeable with INTEGER
};

enum class symbolkind {
    local,      // a variable scoped to the program or subprogram
    common,     // a variable in a common block
    argument,   // an argument passed in to the subprogram
    retval,     // a local variable for the return value of the function
    subprogram, // a subroutine or function (or statement function)
    external,   // a subprogram that's passed as arg to another
    label,      // a statement number
    fsparam     // a function statement parameter (always temporary!!)
};

struct dimension {
    bool operator==(dimension const &rhs) const = default;
    machine_word_t constexpr size() const { return 1 + maximum - minimum; }

    machine_word_t minimum = 1;
    machine_word_t maximum = 0;
};

using array_shape = std::vector<dimension>;

std::size_t array_size(array_shape const &shape);

class symbol_name {
    public:
        constexpr symbol_name() : m_name{'\0'}, m_padding{'\0'} {}

        constexpr symbol_name(std::string_view identifier) :
            m_name(as_name(identifier)), m_padding{'\0'} {}

        // Construct a symbol name from a statement number.
        constexpr symbol_name(unsigned number) :
            m_name(as_name(number)), m_padding{'\0'} {}

        auto operator<=>(symbol_name const &rhs) const = default;

        std::size_t constexpr capacity() const { return m_name.size(); }
        bool constexpr empty() const { return size() == 0; }
        std::size_t constexpr size() const { return length(m_name); }
        std::string_view constexpr view() const {
            return std::string_view(m_name.data(), length(m_name));
        }
        char const &front() const { return m_name.front(); }
        char &front() { return m_name.front(); }

    private:
        using name_t = std::array<char, 6>;
        static name_t constexpr as_name(std::string_view identifier) {
            name_t name;
            auto const length = std::min(identifier.size(), name.size());
            std::fill(std::copy_n(identifier.begin(), length, name.begin()),
                      name.end(), '\0');
            return name;
        }

        static name_t constexpr as_name(unsigned statement_number) {
            char buffer[8];
            auto const result =
                std::to_chars(buffer, buffer + sizeof(buffer), statement_number);
            auto p = (result.ec == std::errc{}) ? result.ptr : buffer;
            *p = '\0';
            return as_name(buffer);
        }

        static std::size_t constexpr length(name_t const &name) {
            return name.size() - std::count(name.begin(), name.end(), '\0');
        }

        name_t m_name;
        std::array<char, 2> m_padding;
};

struct symbol_info {
    symbol_name name;
    symbolkind  kind = symbolkind::local;
    symbol_name comdat;
    unsigned    index = 0;  // see note below
    datatype    type = datatype::unknown;
    array_shape shape;
    std::vector<machine_word_t> init_data;
    bool        referenced;

    // NOTE the meaning of the index field above depends on the kind:
    //  symbolkind::
    //      local       index not needed
    //      common      index is the slot into the common block
    //      argument    index is the argument's position in the arg list
    //                  (functions use argument 0 for return value)
    //      subprogram  the number of arguments needed to call
    //      external    index not needed
    //      label       a statement number used as a jump target
    //
    // `referenced` means the symbol is assigned to (other than initial data),
    // read from, called, invoked, or ref-passed.  For labels, it means it is
    // the target of a GOTO statement.  Any symbol that's not referenced in the
    // program will be dropped in translation (to avoid warnings from the C
    // compiler).
};

// The meaning of symbol_info::index depends on symbol_info::kind:

class symbol_table {
    public:
        static std::size_t constexpr npos = static_cast<std::size_t>(-1);

        // If the symbol is in the table, it returns the symbol_info, otherwise,
        // it returns a fresh symbol_info.
        symbol_info get(symbol_name const &name) const;

        // If the symbol already exists, replaces the existing symbol_info,
        // otherwise it adds the symbol.
        std::size_t update(symbol_info const &symbol);
        std::size_t update(symbol_info &&symbol);

        // Retrieves a symbol by its index.  UB if `i` is out of bounds.
        symbol_info const &operator[](std::size_t i) const { return m_symbols[i]; }
        symbol_info       &operator[](std::size_t i)       { return m_symbols[i]; }

        // Indicates whether the named symbol is in the table.
        bool has(symbol_name const &name) const noexcept {
            return find(name) != npos;
        }

        // Returns the index of the symbol if it's in the table, or `npos`.
        std::size_t find(symbol_name const &name) const noexcept {
            auto const it = m_index.find(name);
            return (it != m_index.end()) ? it->second : npos;
        }

        void clear() { m_index.clear(); m_symbols.clear(); }

        bool empty() const { return m_symbols.empty(); }

        // Returns the number of entries in the table.
        std::size_t size() const { return m_symbols.size(); }

        // Returns the sequence of indices to visit all of the symbols as if
        // they are sorted by name.
        std::vector<std::size_t> indices_sorted_by_name() const;

        // Returns a copy of each symbol.
        std::vector<symbol_info> extract() const;

        // Returns a copy of each symbol that matches the predicate.
        std::vector<symbol_info> extract_if(
            std::function<bool (symbol_info const &)> predicate
        ) const;

    private:
        std::vector<symbol_info> m_symbols;
        std::map<symbol_name, std::size_t> m_index;
};

using parameter_list_t = std::vector<symbol_name>;

}  // namespace aid::fortran

// std::formatter specializations below

template <>
struct std::formatter<aid::fortran::symbol_name> :
    public std::formatter<std::string_view>
{
    auto format(
        aid::fortran::symbol_name const &name,
        std::format_context &context
    ) const {
        return std::formatter<std::string_view>::format(name.view(), context);
    }
};

template <>
struct std::formatter<aid::fortran::symbolkind> :
    public std::formatter<std::string_view>
{
    auto format(
        aid::fortran::symbolkind const &kind,
        std::format_context &context
    ) const {
        return std::formatter<std::string_view>::format(text(kind), context);
    }

    static std::string_view constexpr text(aid::fortran::symbolkind kind) {
        using enum aid::fortran::symbolkind;
        switch (kind) {
            case local:         return "local";
            case common:        return "common";
            case argument:      return "argument";
            case retval:        return "retval";
            case subprogram:    return "subprogram";
            case external:      return "external";
            case label:         return "label";
            case fsparam:       return "fsparam";
            default:            return "* update symbolkind formatter *";
        }
    }
};

template <>
struct std::formatter<aid::fortran::datatype> :
    public std::formatter<std::string_view>
{
    auto format(
        aid::fortran::datatype const &type,
        std::format_context &context
    ) const {
        return std::formatter<std::string_view>::format(text(type), context);
    }

    static std::string_view constexpr text(aid::fortran::datatype type) {
        using enum aid::fortran::datatype;
        switch (type) {
            case unknown:       return "unknown";
            case none:          return "none";
            case INTEGER:       return "INTEGER";
            case REAL:          return "REAL";
            case DOUBLE:        return "DOUBLE";
            case COMPLEX:       return "COMPLEX";
            case LOGICAL:       return "LOGICAL";
            case LITERAL:       return "LITERAL";
            default:            return "* update datatype formatter *";
        }
    }
};

#endif
