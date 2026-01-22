#include "cgenerator.h"

#include "symbols.h"
#include "utility.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <format>
#include <fstream>
#include <print>
#include <set>
#include <string>
#include <string_view>

using namespace std::string_literals;
using namespace std::string_view_literals;

namespace aid::fortran {

namespace {
    static std::string name(symbol_info const &sym) {
        // sym.name is the identifier used in the Fortran code, which sometimes
        // clashes with C or C++ identifiers (especially `NULL`), so we'll
        // prefix it.
        if (sym.kind == symbolkind::subprogram) {
            if (sym.type == datatype::none) {
                return std::format("sub{}", sym.name);
            }
            return std::format("fn{}", sym.name);
        }
        if (sym.kind == symbolkind::external) {
            return std::format("sub{}", sym.name);
        }
        return std::format("v{}", sym.name);
    }
    static std::string base(symbol_name const &block) {
        return block.empty() ? "memory"s : std::format("common{}", block);
    }
    static std::string base(symbol_info const &sym) {
        return base(sym.comdat);
    }
    static std::string macro_defined(std::string_view c_stmt) {
        auto constexpr directive = "#define "sv;
        if (!c_stmt.starts_with(directive)) return {};
        auto const begin = c_stmt.cbegin() + directive.size();
        auto it = begin;
        while (it != c_stmt.end() && *it == ' ') ++it;
        if (it == c_stmt.end()) return {};
        while (it != c_stmt.end() && std::isalnum(*it)) ++it;
        return std::string(begin, it);
    }

    static constexpr std::size_t size(symbol_info const &symbol) {
        switch (symbol.kind) {
            // Regular variables
            case symbolkind::local:
            case symbolkind::common:
            case symbolkind::argument:
            case symbolkind::retval:
                // array_size is poorly named.  Scalars return 1.
                return array_size(symbol.shape);

            // These don't require program memory
            case symbolkind::subprogram:
            case symbolkind::internal:
            case symbolkind::external:
            case symbolkind::label:
                return 0uz;

            // It's weird that the caller is even asking
            case symbolkind::shadow:
                assert(false);
                return 0uz;

            default:
                assert(!"forget to handle a new symbolkind?");
                return 0uz;
        }
    }

    struct builtin_t {
        symbol_name name;
        std::string_view code;
    };
    static auto constexpr available_builtins = std::array<builtin_t, 8>{
        builtin_t{symbol_name{"IABS"},
R"(word_t fnIABS(word_t *x) { return (*x < 0) ? -*x : *x; }
)"},
        builtin_t{symbol_name{"MIN0"},
R"(word_t fnMIN0(word_t *a, word_t *b) { return (*a <= *b) ? *a : *b; }
)"},
        builtin_t{symbol_name{"MAX0"},
R"(word_t fnMAX0(word_t *a, word_t *b) { return (*a >= *b) ? *a : *b; }
)"},
        builtin_t{symbol_name{"MOD"},
R"(word_t fnMOD(word_t *a, word_t *b) { return *a % *b; }
)"},
        builtin_t{symbol_name{"DATE"},
R"(
// Returns the date as two `word_t`s of text, in the form 'dd-MMM-yy '.
void subDATE(word_t r[2]) {
    struct tm now; kron_time(&now);
    const int yy = kron_use_y2k() ? now.tm_year : now.tm_year % 100;
    r[0] = pack_A5((now.tm_mday < 10) ? ' ' : (char)('0' + (now.tm_mday / 10)),
                   (char)('0' + now.tm_mday % 10),
                   '-',
                   kron_mmm[now.tm_mon * 3 + 0],
                   kron_mmm[now.tm_mon * 3 + 1]);
    r[1] = pack_A5(kron_mmm[now.tm_mon * 3 + 2],
                   '-',
                   (char)('0' + yy / 10), // produces non-digit after 1999
                   (char)('0' + yy % 10),
                   ' ');
}
)"},
        builtin_t{symbol_name{"TIME"},
R"(
// Returns the time as text, in the form 'hh:mm'.  Both fields are two digits,
// with a leading '0' if necessary.  The actual library function has an optional
// second argument to receive seconds and tenths.
void subTIME(word_t *r) {
    struct tm now; kron_time(&now);
    r[0] = pack_A5((char)('0' + now.tm_hour / 10),
                   (char)('0' + now.tm_hour % 10),
                   ':',
                   (char)('0' + now.tm_min / 10),
                   (char)('0' + now.tm_min % 10));
}
)"},
        builtin_t{symbol_name{"IFILE"},
R"(
// The oldest versions of Adventure used IFILE rather than an OPEN statement.
// The file name is a single A5-encoded word, so the file name is limited to
// five characters and no extension.
void subIFILE(word_t *unit, word_t *file) {
    char buffer[6];
    for (int i = 0; i < 5; ++i) {
        char ch = (char)((*file >> (1 + 7*(4-i))) & 0x7F);
        buffer[i] = ch == ' ' ? '\0' : ch;
    }
    buffer[5] = '\0';
    io_open(*unit, buffer);
}
)"},
        builtin_t{symbol_name{"RAN"},
R"(
// Returns a random REAL value between 0.0 and 1.0 (inclusive).
word_t fnRAN(word_t *state) {
    if (*state == 0) {
        *state = time(NULL);
        srand((unsigned int)(*state));
    }
    const float r = 1.0f * rand() / RAND_MAX;
    return ( ( (word_t)(*(uint32_t*)(&r)) ) << 32 ) >> 32;
}
)"}
    };

}

std::string c_generator::generate_c(program const &prog) {
    auto g = c_generator{};
    return g.generate_program(prog);
}

std::string c_generator::generate_program(program const &prog) {
    m_memsize = 0;
    m_init_data.clear();
    // Argument evaluation order is not specified, so we generate each
    // portion first and use the std::format to concatenate it all together.
    auto const builtins      = generate_builtins(prog);
    auto const prototypes    = generate_prototypes(prog);
    auto const common_blocks = generate_common_blocks(prog);
    auto const subprograms   = generate_subprograms(prog);
    auto const init_function = generate_static_initialization();
    auto const main_function = generate_main_function(prog);

    // We generate the memory last, because the required memory size is a
    // side-effect of generating everything else.
    auto const memory =
        std::format("#define MEMSIZE {}\n"
                    "static word_t memory[MEMSIZE];\n", m_memsize);

    return std::format("{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
        machine_definitions(),
        host_subsystem(), io_subsystem(), kron_subsystem(), builtins,
        prototypes, memory, common_blocks, subprograms,
        init_function, usage_function(), main_function);
}

std::string c_generator::generate_builtins(program const &prog) {
    auto result = std::string{};

    // Find all functions and subroutines that are is_referenced in any unit.
    auto undefined_subs = std::set<symbol_name>{};
    for (auto const &u : prog) {
        auto const subs_refed_by_unit =
            u.extract_symbols(is_referenced_subprogram);
        for (auto const &s : subs_refed_by_unit) {
            undefined_subs.insert(s.name);
        }
    }

    // Remove ones that are defined.
    for (auto const &u : prog) {
        undefined_subs.erase(u.unit_name());
    }

    // Provide built-in ones. This is in a separate pass so that user-defined
    // subprograms will supercede builtins.
    bool any_builtins = false;
    for (auto const &builtin : available_builtins) {
        if (undefined_subs.contains(builtin.name)) {
            if (!any_builtins) {
                result += "// Emulated PDP-10 system library subroutines.\n";
                any_builtins = true;
            }
            result += builtin.code;
            undefined_subs.erase(builtin.name);
        }
    }

    for (auto const &sub : undefined_subs) {
        std::print(std::cerr, "ERROR: subprogram '{}' is not defined\n", sub);
    }
    return result;
}

std::string c_generator::generate_prototypes(program const &prog) {
    auto result =
        std::string{};
    auto prototypes = std::string{};
    for (auto const &sub : prog) {
        prototypes += std::format("{};\n", generate_function_signature(sub));
    }
    if (prototypes.empty()) return {};
    return "// Function prototypes for the program's subprograms\n"s +
           prototypes;
}

std::string c_generator::generate_common_blocks(program const &prog) {
    auto const comdats = common_block_sizes(prog);
    if (comdats.empty()) return {};
    auto result = "// Common Blocks\n"s;
    for (auto const &[block, size] : common_block_sizes(prog)) {
        if (size == 0uz) continue;
        result +=
            std::format("word_t * const common{} = &memory[{}]; // [{}];\n",
                        block, m_memsize, size);
        m_memsize += size;
    }
    return result;
}

std::string c_generator::generate_main_function(program const &prog) {
    return std::format(R"(
void atexit_handler(void) {{ host_savecore(memory, MEMSIZE); }}

int main(int argc, const char *argv[]) {{
    io_init();
    kron_init();
    const char *core_file_name = NULL;
    for (int i = 1; i < argc; ++i) {{
        const char *arg = argv[i];
        if (arg == NULL) break;
        if (arg[0] == '-') {{
            if (strncmp(arg, "-y2k", 4) == 0) {{
                kron_set_y2k(arg[4] != '-');
                continue;
            }} else if (strncmp(arg, "-CAPS", 5) == 0) {{
                io_force_caps(arg[5] != '-');
            }} else switch (arg[1]) {{
                case 'F': case 'f': io_addmapping(arg+2); continue;
                case 'T': case 't': kron_override_time(arg+2); continue;
                case 'D': case 'd': kron_override_date(arg+2); continue;
                case 'C': case 'c': core_file_name = arg+2; continue;
                case 'H': case 'h':
                case '?':           usage(); return 0;
                default:
                    fprintf(stderr, "ignoring unrecognized option: %s\n", arg);
                    break;
            }}
        }} else {{
            fprintf(stderr, "ignoring argument: %s\n", arg);
        }}
    }}
    if (core_file_name != NULL && *core_file_name != '\0') {{
        if (!host_loadcore(core_file_name, memory, MEMSIZE)) {{
            memset(memory, 0, MEMSIZE*sizeof(word_t));
            initialize_static_data();
        }}
    }} else {{
        initialize_static_data();
    }}
    atexit(atexit_handler);
    sub{}();
    return 0;
}}
)", prog.name());
}

std::string c_generator::generate_subprograms(program const &prog) {
    auto result = std::string{};
    for (auto const &subprogram : prog) {
        if (!subprogram.is_referenced()) continue;
        result += generate_unit(subprogram);
    }
    return result;
}

std::string c_generator::generate_static_initialization() {
    auto const is_run = [] (auto data) -> bool {
        if (data.size() <= 3) return false;
        for (auto value : data) {
            if (value != data.front()) return false;
        }
        return true;
    };

    auto runs = std::string{};
    auto singletons = std::string{};
    for (auto [block, offset, data] : m_init_data) {
        if (is_run(data)) {
            if (data.front() != 0) {
                runs +=
                    std::format(
                        " for (int i = 0; i < {}; ++i) {}[{}+i] = {};\n",
                        data.size(), base(block), offset, data.front());
            }
        } else {
            for (auto value : data) {
                if (value != 0) {
                    singletons +=
                        std::format(
                            " {}[{}] = {};\n",
                            base(block), offset, value);
                }
                ++offset;
            }
        }
    }

    return
        std::format("void initialize_static_data() {{\n{}{}}}\n",
                    runs, singletons);
}

std::string c_generator::generate_unit(unit const &u) {
    return std::format("{} {{\n{}{}{}{}{}{}}}\n\n",
                       generate_function_signature(u),
                       generate_return_value(u),
                       generate_dummies(u),
                       generate_common_variable_declarations(u),
                       generate_local_variable_declarations(u),
                       generate_format_specifications(u),
                       generate_statements(u));
}

std::string c_generator::generate_function_signature(unit const &u) {
    auto result = std::string{};
    auto const retval = u.extract_symbols(is_return_value);
    if (retval.empty()) {
        result += std::format("void sub{}(", u.unit_name());
    } else {
        result += std::format("word_t fn{}(", u.unit_name());
    }
    auto const parameters = u.extract_symbols(is_argument, by_index);
    for (auto const &param : parameters) {
        if (is_return_value(param)) continue;
        if (param.index > 1) result += ", ";
        result += std::format("word_t *{}", name(param));
    }
    result += ')';
    return result;
}

std::string c_generator::generate_statements(unit const &u) {
    auto result = std::string{};

    auto macros_to_undef = std::vector<std::string>{};
    for (auto const &statement : u.code()) {
        if (auto const c_stmt = statement->generate(u); !c_stmt.empty()) {
            result += std::format(" {}\n", c_stmt);
            if (auto const macro = macro_defined(c_stmt); !macro.empty()) {
                macros_to_undef.push_back(macro);
            }
        }
    }
    for (auto const &macro : macros_to_undef) {
        result += std::format(" #undef {}\n", macro);
    }

    return result;
}

std::string c_generator::generate_return_value(unit const &u) {
    auto const retvals = u.extract_symbols(is_return_value); 
    if (retvals.empty()) return {};
    auto const &retval = retvals.front();
    auto const addr = m_memsize;
    m_memsize += size(retval);
    return std::format(" word_t *{} = &memory[{}];  // return value\n",
                       name(retval), addr);
}

std::string c_generator::generate_dummies(unit const &u) {
    // Avoid compiler warnings for unused arguments.
    auto result = std::string{};
    auto const dummies = u.extract_symbols(is_unreferenced_argument);
    for (auto const dummy : dummies) {
        result += std::format(" (void) {};  // unused argument\n", name(dummy));
    }
    return result;
}

std::string c_generator::generate_common_variable_declarations(unit const &u) {
    auto const commons = u.extract_symbols(is_common, by_block_index);
    if (commons.empty()) return {};
    auto result = " // Common variables\n"s;
    auto block = symbol_name{};
    auto offset = machine_word_t{0};
    for (auto const &common : commons) {
        if (common.comdat != block) offset = 0;
        block = common.comdat;
        if (common.referenced) {
            result += generate_variable_definition(common, offset);
            add_initializer(block, offset, common.init_data);
        }
        // Its size still matters to the layout, even if it wasn't is_referenced.
        offset += size(common);
        // Note that we do NOT bump m_memsize because the space for the common
        // variables is already accounted for by the comdat blocks.
    }
    return result;
}

std::string c_generator::generate_local_variable_declarations(unit const &u) {
    auto const locals = u.extract_symbols(is_referenced_local);
    if (locals.empty()) return {};
    auto result = " // Local variables\n"s;
    for (auto const &local : locals) {
        result += generate_variable_definition(local, m_memsize);
        add_initializer(local.comdat, m_memsize, local.init_data);
        m_memsize += size(local);
    }
    return result;
}

std::string c_generator::generate_format_specifications(unit const &u) {
    if (u.formats().empty()) return {};
    auto result = " // IO format specifications\n"s;
    for (auto const format : u.formats()) {
        result +=
            std::format(" static const char fmt{}[] = \"{}\";\n",
                        format.first, escape_string(format.second));
    }
    return result;
}

std::string c_generator::generate_variable_definition(
    symbol_info const &variable,
    machine_word_t offset
) {
    if (is_array(variable)) {
        return generate_array_definition(variable, offset);
    } else {
        return generate_scalar_definition(variable, offset);
    }
}

std::string c_generator::generate_array_definition(
    symbol_info const &var,
    machine_word_t offset
) {
    auto result =
        std::format(" word_t * const {} = &{}[{}]; // [{}]",
                    name(var), base(var), offset, size(var));
    if (!var.init_data.empty()) {
        if (var.init_data.size() <= 3) {
            result += std::format(" = {{{}", var.init_data[0]);
            for (auto i = 1uz; i < var.init_data.size(); ++i) {
                result += std::format(",{}", var.init_data[i]);
            }
            result += '}';
        } else {
            result +=
                std::format(" = {{{}, {}, ..., {}}}",
                            var.init_data[0], var.init_data[1],
                            var.init_data.back());
        }
    }
    result += '\n';
    return result;
}

std::string c_generator::generate_scalar_definition(
    symbol_info const &var,
    machine_word_t offset
) {
    auto result =
        std::format(" word_t * const {} = &{}[{}];",
                    name(var), base(var), offset);
    if (!var.init_data.empty()) {
        result += std::format(" // = {}", var.init_data[0]);
    }
    result += '\n';
    return result;
}

void c_generator::add_initializer(
    symbol_name block,
    machine_word_t offset,
    init_data_t data
) {
    if (!data.empty()) {
        m_init_data.emplace_back(block, offset, std::move(data));
    }
}

constexpr std::string_view c_generator::machine_definitions() {
    return R"(#define __STDC_WANT_LIB_EXT1__ 1
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if __STDC_VERSION__ >= 202311
#define NORETURN [[noreturn]]
#elif defined(_MSC_VER)
#define NORETURN __declspec(noreturn)
#elif __STDC_VERSION__ >= 201112
#define NORETURN [[_Noreturn]]
#else
#define NORETURN /*noreturn*/
#endif

// Definitions for emulating the PDP-10 and its Fortran system.
typedef int64_t word_t;

// The PDP-10 LOGICAL uses the sign bit for truth.
const word_t logical_true  = (word_t)-1;
const word_t logical_false = (word_t)0;
bool truth(word_t w) { return (w < 0) ? true : false; }
word_t logical(bool b) { return b ? logical_true : logical_false; }

// A buffer for passing temporaries by reference.
#define TMP_BUFFER_SIZE 256u
static word_t temps[TMP_BUFFER_SIZE];
static word_t tmp_index = 0;
static word_t tmp_frame = 0;
word_t *tmp(word_t x) {
    assert(0 <= tmp_index && tmp_index < TMP_BUFFER_SIZE);
    if (tmp_index < 0 || TMP_BUFFER_SIZE < tmp_index) return NULL;
    temps[tmp_index] = x;
    return &temps[tmp_index++];
}
word_t tmp_push() {
    assert(0 <= tmp_frame && tmp_frame <= tmp_index && tmp_index < TMP_BUFFER_SIZE);
    temps[tmp_index] = tmp_frame;
    tmp_frame = tmp_index++;
    return 0;
}
word_t tmp_pop(word_t x) {
    tmp_index = tmp_frame;
    tmp_frame = temps[tmp_index];
    assert(0 <= tmp_frame && tmp_frame <= tmp_index && tmp_index < TMP_BUFFER_SIZE);
    return x;
}
#define EVAL(EXPR) (tmp_push(), tmp_pop(EXPR))
#define CALL(SUB)  do { tmp_push(); SUB; tmp_pop(0); } while (false)

// Types for coercing function pointers for indirect subroutine calls.
typedef void (*psub0)();
typedef void (*psub1)(word_t*);
typedef void (*psub2)(word_t*,word_t*);
typedef void (*psub3)(word_t*,word_t*,word_t*);
typedef void (*psub4)(word_t*,word_t*,word_t*,word_t*);
typedef void (*psub5)(word_t*,word_t*,word_t*,word_t*,word_t*);
typedef void (*psub6)(word_t*,word_t*,word_t*,word_t*,word_t*,word_t*);

// Helpers for processing expressions:
word_t fixsign(word_t x) { return (x << (64-36)) >> (64-36); }
word_t neg(word_t x) { return -x; }
word_t bitnot(word_t x) { return ~x; }
word_t add(word_t a, word_t b) { return fixsign(a + b); }
word_t sub(word_t a, word_t b) { return fixsign(a - b); }
word_t mul(word_t a, word_t b) { return fixsign(a * b); }
word_t dvd(word_t a, word_t b) { return fixsign(a / b); }
word_t eq(word_t a, word_t b) { return logical(a == b); }
word_t ne(word_t a, word_t b) { return logical(a != b); }
word_t lt(word_t a, word_t b) { return logical(a <  b); }
word_t le(word_t a, word_t b) { return logical(a <= b); }
word_t gt(word_t a, word_t b) { return logical(a >  b); }
word_t ge(word_t a, word_t b) { return logical(a >= b); }
word_t bitand(word_t a, word_t b) { return fixsign(a & b); }
word_t bitor(word_t a, word_t b)  { return fixsign(a | b); }
word_t bitxor(word_t a, word_t b) { return fixsign(a ^ b); }
word_t sign(word_t a) { return a < 0 ? -1 : 0 < a ? 1 : 0; }

bool in_range(word_t index, word_t bound1, word_t bound2) {
    const word_t low  = (bound2 < bound1) ? bound2 : bound1;
    const word_t high = (bound1 < bound2) ? bound2 : bound1;
    return low <= index && index <= high;
}

// Packs five ASCII characters into a word_t as PDP-10 does.
word_t pack_A5(char c0, char c1, char c2, char c3, char c4) {
    return
        (((((((((word_t)(c0) << 7) + c1) << 7) + c2) << 7) + c3) << 7) + c4)
        << 1;
}
)";
}

constexpr std::string_view c_generator::io_subsystem() {
    return R"(// The input-output subsystem
#define IO_MAX_MAPPINGS 4
#define IO_MAX_UNITS 4
struct iocontext {
    const char *mappings[IO_MAX_MAPPINGS];
    FILE *units[IO_MAX_UNITS+1];
    char record[132];
    char guard[sizeof(word_t)];
    const char *psrc;
    char       *pdst;
    const char *pfmt;
    int repeat;
    int width;
    const char *(*reader)(int, const char *, word_t *);
    char *(*writer)(int, const word_t *, char *);
    bool upcase;
} io;

void io_init() {
    for (size_t i = 0; i < sizeof(io.mappings) / sizeof(io.mappings[0]); ++i) {
        io.mappings[i] = NULL;
    }
    io.units[0] = stdin;
    for (size_t i = 1; i < sizeof(io.units) / sizeof(io.units[0]); ++i) {
        io.units[i] = NULL;
    }
    memset(io.record, 0, sizeof(io.record));
    memset(io.guard, 0, sizeof(io.guard));
    io.psrc = io.record;
    io.pdst = io.record;
    io.pfmt = "";
    io.repeat = 0;
    io.width = 0;
    io.reader = NULL;
    io.writer = NULL;
    io.upcase = false;
}

void io_addmapping(const char *mapping) {
    for (size_t i = 0; i < sizeof(io.mappings) / sizeof(io.mappings[0]); ++i) {
        if (io.mappings[i] == NULL) {
            io.mappings[i] = mapping;
            return;
        }
    }
}

void io_force_caps(bool enable) { io.upcase = enable; }

void io_open(word_t unit, const char *name) {
    for (int i = 0; i < IO_MAX_MAPPINGS; ++i) {
        if (io.mappings[i] == NULL) continue;
        const char *p1 = name, *p2 = io.mappings[i];
        while (*p1 != '\0' && *p1 == *p2) { ++p1; ++p2; }
        if (*p1 == '\0' && *p2 == '=') { name = p2 + 1; break; }
    }
    assert(1 <= unit && unit <= IO_MAX_UNITS);
    if (1 <= unit && unit <= IO_MAX_UNITS) {
        if (io.units[unit] != NULL) {
            fclose(io.units[unit]);
            io.units[unit] = NULL;
        }
#if defined(__STDC_LIB_EXT1__) | defined(_MSC_VER)
        fopen_s(&io.units[unit], name, "r");
#else
        io.units[unit] = fopen(name, "r");
#endif
    }
    if (io.units[unit] == NULL) {
        fprintf(stderr,
            "\nThe program failed to open a file named \"%s\".\n"
            "You can restart the program with a file name mapping using the\n"
            "command line option -f, like this:\n\n"
            "    -f%s=<path>\n", name, name);
        host_exit(EXIT_FAILURE);
    }
}

void io_loadrecord(word_t unit) {
    const size_t size = sizeof(io.record) - 1;  // reserve room for terminator
    size_t i = 0;
    FILE *source = io.units[unit];
    assert(source != NULL);
    if (source != NULL) {
        while (i < size) {
            int ch = fgetc(source);
            if (ch == EOF || ch == '\n') break;
            if (io.upcase && unit == 0 && islower(ch)) ch = toupper(ch);
            io.record[i++] = (char)ch;
        }
        assert(0 <= i && i < size);
    }
    io.record[i] = '\0';
    io.psrc = io.pdst = io.record;
}

void io_selectformat(const char *format) {
    io.pfmt = format;
    io.repeat = 0;
    io.psrc = io.pdst = io.record;
}

int isdelimiter(int ch) { return ch == '\t' || ch == ','; }

const char *io_readinteger(int width, const char *psrc, word_t *pvar) {
    // If `width` is 0, the format string didn't specify a field width, so the
    // first non-valid character delimits the field.  Otherwise, the field is
    // exactly `width`.
    word_t value = 0;
    int sign = 1;
    const char *begin = psrc;
    skipping:
        if (*psrc == '\0') goto done;
        if (width > 0 && psrc - begin == width) goto done;
        if (*psrc == '-') { sign = -1; ++psrc; goto digits; }
        if (isdigit(*psrc)) { value = *psrc++ - '0'; goto digits; }
        ++psrc; goto skipping;
    digits:
        if (*psrc == '\0') goto done;
        if (width > 0 && psrc - begin == width) goto done;
        if (isdigit(*psrc)) { value = 10*value + (*psrc++ - '0'); goto digits; }
        if (width == 0) {
            if (isdelimiter(*psrc)) ++psrc;
            goto done;
        }
        ++psrc; goto extra;
    extra:
        if (*psrc == '\0') goto done;
        if (psrc - begin == width) goto done;
        ++psrc; goto extra;
    done:
        *pvar = sign * value;
        return psrc;
}

char *io_writeinteger(int width, const word_t *pvar, char *pdst) {
    uint64_t value = *pvar < 0 ? -*pvar : *pvar;
    if (width == 0) {
        width = 1;
        if (*pvar < 0) width += 1;
        for (uint64_t x = value; x >= 10; x /= 10) width += 1;
    }
    char *pfinal = pdst + width;
                                     pdst[--width] = '0' + value % 10; value /= 10;
    while (value > 0 && width > 0) { pdst[--width] = '0' + value % 10; value /= 10; }
    if (*pvar < 0 && width > 0)    { pdst[--width] = '-'; }
    while (width > 0)              { pdst[--width] = ' '; }
    return pfinal;
}

const char *io_readliteral(int width, const char *psrc, word_t *pvar) {
    // We read all `width` characters even if that's more than five.
    word_t value = 0;
    int i;
    for (i = 0; i < width && *psrc != '\0'; ++i) {
        value <<= 7;
        value |= (word_t)(*psrc++ & 0x7F);
    }
    // Pad with blanks on the right to a minimum size of 5.
    for (; i < 5; ++i) {
        value <<= 7;
        value |= ' ';
    }
    // Final adjustment and fix for sign extension since a 64-bit word_t is
    // wider than the 36-bit machine word being emulated.
    value <<= (1 + 64 - 36);
    value >>= (64 - 36);
    *pvar = value;
    return psrc;
}

char *io_writeliteral(int width, const word_t *pvar, char *pdst) {
    const int shift = 29;
    const word_t mask = (word_t)0x7F << shift;
    word_t x = *pvar;
    const int count = width < 5 ? width : 5;
    int i;
    for (i = 0; i < count; ++i) {
        if (width < i) break;
        *pdst++ = (char)((x & mask) >> shift);
        x <<= 7;
    }
    while (i < width) *pdst++ = ' ';
    return pdst;
}

const char *io_readlogical(int width, const char *psrc, word_t *pvar) {
    word_t value = logical_false;
    while (width-- > 0 && isspace(*psrc)) ++psrc;
    if (width-- > 0 && *psrc != '\0') {
        if (*psrc == 'T' || *psrc == 't') { value = logical_true; ++psrc; }
        else if (*psrc == 'F' || *psrc == 'f') { ++psrc; }
    }
    while (width-- > 0 && *psrc != '\0') ++psrc;
    *pvar = value;
    return psrc;
}

char *io_writelogical(int width, word_t const *pvar, char *pdst) {
    if (width > 1) {
        while (width-- > 1) { *pdst++ = ' '; }
    }
    *pdst++ = *pvar < 0 ? 'T' : 'F';
    return pdst;
}

void io_input(word_t *pvar) {
    if (io.repeat == 0) {
        io.repeat = 1;
        if ('1' <= *io.pfmt && *io.pfmt <= '9') {
            io.repeat = *io.pfmt++ - '0';
            while (isdigit(*io.pfmt)) {
                io.repeat = io.repeat*10 + (*io.pfmt++ - '0');
            }
        }
        io.reader = NULL;
        switch (*io.pfmt) {
            case 'A': io.reader = io_readliteral; ++io.pfmt; break;
            case 'G':
            case 'I': io.reader = io_readinteger; ++io.pfmt; break;
            case 'L': io.reader = io_readlogical; ++io.pfmt; break;
            default: assert(!"unsupported conversion format"); break;
        }
        io.width = 0;
        if ('1' <= *io.pfmt && *io.pfmt <= '9') {
            io.width = *io.pfmt++ - '0';
            while (isdigit(*io.pfmt)) {
                io.width = io.width*10 + (*io.pfmt++ - '0');
            }
        }
        if (*io.pfmt == ',') ++io.pfmt;
    }
    if (io.repeat > 0) {
        io.repeat -= 1;
        if (io.reader != NULL) io.psrc = (*io.reader)(io.width, io.psrc, pvar);
    }
}

void io_storerecord(word_t unit) {
    FILE *out = unit == 0 ? stdout : io.units[unit];
    *io.pdst = '\0';
    switch (io.record[0]) {
        case ' ':  fputs(io.record+1, out); /*FALLTHROUGH*/
        case '\0': fputc('\n', out); break;
        case '+':  fputs(io.record+1, out); fputc('\r', out); break;
        default:   fputs(io.record, out); fputc('\n', out); break;
    }
    memset(io.record, 0, sizeof(io.record));
    io.psrc = io.pdst = io.record;
}

void io_scanfmt(word_t unit) {
    while (*io.pfmt == '/' || *io.pfmt == '\'') {
        switch (*io.pfmt++) {
            case '/': io_storerecord(unit); break;
            case '\'':
                while (*io.pfmt != '\0' && *io.pfmt != '\'') {
                    *io.pdst++ = *io.pfmt++;
                }
                if (*io.pfmt == '\'') ++io.pfmt;
                if (*io.pfmt == '\'') *io.pdst++ = '\'';
                *io.pdst = '\0';
            break;
        }
        if (*io.pfmt == ',') ++io.pfmt;
    }
}

void io_output(word_t unit, word_t *pvar) {
    if (io.repeat == 0) {
        while (io.repeat == 0) {
            switch (*io.pfmt) {
                case '\0': io_storerecord(unit); return;
                case '/':  io_storerecord(unit); ++io.pfmt; break;
                case '\'':
                    ++io.pfmt;
                    while (*io.pfmt != '\0' && *io.pfmt != '\'') {
                        *io.pdst++ = *io.pfmt++;
                    }
                    if (*io.pfmt == '\'') ++io.pfmt;
                    if (*io.pfmt == '\'') *io.pdst++ = '\'';
                    break;
                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                    io.repeat = *io.pfmt++ - '0';
                    while (isdigit(*io.pfmt)) {
                        io.repeat = 10*io.repeat + *io.pfmt++ - '0';
                    }
                    if (*io.pfmt == 'X') {
                        ++io.pfmt;
                        while (io.repeat > 0) {
                            *io.pdst++ = ' ';
                            --io.repeat;
                        }
                    }
                    break;
                default: io.repeat = 1; break;
            }
            while (*io.pfmt == ',') { ++io.pfmt; io.repeat = 0; }
        }
        io.writer = NULL;
        switch (*io.pfmt) {
            case 'A': io.writer = io_writeliteral; ++io.pfmt; break;
            case 'G':
            case 'I': io.writer = io_writeinteger; ++io.pfmt; break;
            case 'L': io.writer = io_writelogical; ++io.pfmt; break;
            default: assert(!"unsupported conversion format"); break;
        }
        if (io.writer == NULL) { io_storerecord(unit); return; }
        io.width = 0;
        if ('1' <= *io.pfmt && *io.pfmt <= '9') {
            io.width = *io.pfmt++ - '0';
            while (isdigit(*io.pfmt)) {
                io.width = io.width*10 + (*io.pfmt++ - '0');
            }
        }
        assert(io.repeat > 0);
        assert(io.writer != NULL);
    }
    if (pvar == NULL) { io_storerecord(unit); return; }
    io.pdst = (*io.writer)(io.width, pvar, io.pdst);
    *io.pdst = '\0';
    --io.repeat;
}
)";
}

constexpr std::string_view c_generator::kron_subsystem() {
    return R"(// The date and time subsystem
struct kroncontext {
    bool y2k_hack;
    bool override_time;
    bool override_date;
    struct tm overrides;
} kron;

const char kron_mmm[] = "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";

void kron_init() {
    kron.y2k_hack = true;
    kron.override_time = false;
    kron.override_date = false;
    memset(&kron.overrides, 0, sizeof(kron.overrides));
}

void kron_time(struct tm *ptm) {
    const time_t t = time(0);
#if defined(_MSC_VER)
    const errno_t result = localtime_s(ptm, &t);
    assert(result == 0); (void) result;
#elif defined(__STDC_LIB_EXT1__)
    void *result = localtime_s(&t, ptm);
    assert(result != NULL); (void) result;
#else
    *ptm = *localtime(&t);
#endif
    if (kron.override_time) {
        ptm->tm_min = kron.overrides.tm_min;
        ptm->tm_hour = kron.overrides.tm_hour;
    }
    if (kron.override_date) {
        ptm->tm_mday = kron.overrides.tm_mday;
        ptm->tm_mon = kron.overrides.tm_mon;
        ptm->tm_year = kron.overrides.tm_year;
    }
}

void kron_set_y2k(bool enable) { kron.y2k_hack = enable; }

bool kron_use_y2k() { return kron.y2k_hack; }

void kron_override_time(const char *p) {
    int hh = 0, mm = 0;
    if (isdigit(*p)) { hh = *p++ - '0'; }
    if (isdigit(*p)) { hh = 10*hh + *p++ - '0'; }
    if (*p == ':') {
        ++p;
        if (isdigit(*p)) { mm = *p++ - '0'; }
        if (isdigit(*p)) { mm = 10*mm + *p++ - '0'; }
    }
    if (toupper(*p) == 'P' && 0 < hh && hh < 12) { hh += 12; ++p; }
    kron.overrides.tm_hour = hh % 24;
    kron.overrides.tm_min  = mm % 60;
    kron.override_time = true;
}

int kron_days_in_month(int month, int year) {
    const int days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    assert(1 <= month && month <= 12);
    const int leap_day = (month == 2 && (year % 4) == 0) ? 1 : 0;
    return days[month - 1] + leap_day;
}

bool kron_valid(int year, int month, int day) {
    if (month < 1 || 12 < month) return false;
    if (year < 0 || 9999 < year) return false;
    if (99 < year && year < 1970) return false;
    const int mdays = kron_days_in_month(month, year);
    if (day < 1 || mdays < day) return false;
    return true;
}

void kron_override_date(const char *p) {
    int year = 0, month = 0, day = 0;
    int x1 = 0;
    while (isdigit(*p) && x1 < 1000) x1 = 10*x1 + (*p++ - '0');
    if (ispunct(*p)) ++p;
    int x2 = 0;
    if (isalpha(*p) && strlen(p) >= 3) {
        char buffer[4] = {
            (char)toupper(*p++), (char)toupper(*p++), (char)toupper(*p++), '\0'
        };
        const char *match = strstr(kron_mmm, buffer);
        if (match == NULL) return;
        const ptrdiff_t dist = match - kron_mmm;
        if (dist < 0 && dist % 3 != 0) return;
        month = (int)(dist/3 + 1);
        while (isalpha(*p)) ++p;
    } else if (isdigit(*p)) {
        while (isdigit(*p) && x2 < 1000) x2 = 10*x2 + *p++ - '0';
    } else {
        return;
    }
    if (ispunct(*p)) ++p;
    int x3 = 0;
    while (isdigit(*p) && x3 < 1000) x3 = 10*x3 + *p++ - '0';
    if (month != 0) {
        if      (kron_valid(x3, month, x1)) { year = x3; day = x1; }
        else if (kron_valid(x1, month, x3)) { year = x1; day = x3; }
        else return;
    } else {
        if      (kron_valid(x3, x2, x1)) { year = x3; month = x2; day = x1; }
        else if (kron_valid(x1, x2, x3)) { year = x1; month = x2; day = x3; }
        else if (kron_valid(x3, x1, x2)) { year = x3; month = x1; day = x2; }
        else return;
    }
    kron.overrides.tm_year =
        (year < 77) ? year + 100 : (year >= 1900) ? year - 1900 : year;
    kron.overrides.tm_mon = month - 1;
    kron.overrides.tm_mday = day;
    kron.override_date = true;
}
)";
}

constexpr std::string_view c_generator::host_subsystem() {
    return R"(// Host subsystem (core images, pause, etc.)
char host_endianness() {
    #if CHAR_BIT == 8
        static const char buffer[sizeof(int)] = { 0x01, 0x02 };
        int test = -1;
        memcpy_s(&test, sizeof(test), buffer, sizeof(buffer));
        switch (test) {
            case 0x0102: return 'B';
            case 0x0201: return 'L';
            default:     break;
        }
    #endif
    return '?';
}

void host_tidyfname(char *fname, size_t size, char const *defext) {
    if (fname == NULL) return;
    const char *source = fname;
    const char *slash = NULL;
    const char *dot = NULL;
    char *target = fname;
    while (isspace(*source)) ++source;
    while (*source != '\0') {
        char ch = *source++;
        switch (ch) {
            case '\r': case '\n': continue;
            case '.':             dot   = target; break;
#if _WIN32
            case '/':             ch = '\\'; /*FALLTHROUGH*/
            case '\\':            slash = target; break;
            case '<': case '>':
            case '"': case '|':
            case '?': case '*':   ch = '_'; break;

            case ':':  // allow only after a drive letter
                if (target != fname + 1 || !isalpha(*fname)) ch = '_';
                break;
#else
            case '\\':            ch = '/'; /*FALLTHROUGH*/
            case '/':             slash = target; break;
#endif
            default:
                if (ch < 0x20) ch = '_';
                break;
        }
        *target++ = ch;
    }
    while (target != fname && isspace(*(target-1))) --target;
    const bool needext =
        target != fname && defext != NULL && *defext != '\0' &&
        (dot == NULL ? true : slash == NULL ? false : dot < slash);
    if (needext && defext != NULL && target + strlen(defext) < fname + size) {
        dot = target;
        if (*defext != '.') *target++ = '.';
        while (*defext != '\0') *target++ = *defext++;
    }
    *target = '\0';
}

typedef struct {
    char magic[8];
    char pdp_bits;
    char host_bits;
    char host_endian;
    char reserved;
    uint32_t count;
    uint32_t offset;
    char pad[12];
} host_corehdr;

static const host_corehdr host_coremodel = {
    {'C', 'O', 'R', 'E', '\x0D','\x0A', '\x1A', '\0'},
    36, 64, '?', '\0', 0, (uint32_t)sizeof(host_corehdr), 0
};

bool host_dumpcore(const char *file_name, const word_t *memory, word_t count) {
    if (memory == NULL || count == 0) return false;
    FILE *core_file = fopen(file_name, "wb");
    if (core_file == NULL) {
        fprintf(stderr, "cannot open '%s' for writing\n", file_name);
        return false;
    }
    host_corehdr hdr = host_coremodel;
    hdr.count = (uint32_t) count;
    hdr.host_endian = host_endianness();
    const bool success =
        fwrite(&hdr, 1, sizeof(hdr), core_file) == sizeof(hdr) &&
        fwrite(memory, sizeof(word_t), hdr.count, core_file) == hdr.count;
    if (!success) fputs("failed to save core dump", stderr);
    fclose(core_file);
    return success;
}

bool host_savecore(const word_t *memory, word_t count) {
    if (memory == NULL || count == 0) return false;
    puts("ENTER FILE NAME TO SAVE CORE IMAGE:");
    char fname[260+1] = "";
    if (fgets(fname, sizeof(fname), stdin) == NULL) return false;
    host_tidyfname(fname, sizeof(fname), ".DMP");
    if (*fname == '\0') return false;
    return host_dumpcore(fname, memory, count);
}

bool host_loadcore(const char *file_name, word_t *memory, word_t count) {
    char fname[260+1] = "";
    const size_t len = strlen(file_name);
    if (len >= sizeof(fname)) return false;
#if defined(__STDC_LIB_EXT1__) | defined(_MSC_VER)
    strncpy_s(fname, sizeof(fname), file_name, len);
#else
    strncpy(fname, file_name, len);
#endif
    host_tidyfname(fname, sizeof(fname), ".DMP");
    FILE *core_file = fopen(fname, "rb");
    if (core_file == NULL) {
        fprintf(stderr, "cannot open '%s' for reading\n", fname);
        return false;
    }

    host_corehdr hdr = {0};
    const bool success =
        fread(&hdr, sizeof(hdr), 1, core_file) == 1 &&
        memcmp(hdr.magic, host_coremodel.magic, sizeof(hdr.magic)) == 0 &&
        hdr.pdp_bits == host_coremodel.pdp_bits &&
        hdr.host_bits == host_coremodel.host_bits &&
        hdr.host_endian == host_endianness() &&
        hdr.reserved == 0 &&
        hdr.offset >= sizeof(hdr) &&
        hdr.offset % sizeof(word_t) == 0 &&
        hdr.count <= count &&
        fseek(core_file, hdr.offset, SEEK_SET) == 0 &&
        fread(memory, sizeof(word_t), hdr.count, core_file) == hdr.count;
    fclose(core_file);
    if (!success) {
        fprintf(stderr, "unable to load core-image '%s'\n", fname);
    }
    return success;
}

NORETURN void host_exit(int status) { exit(status); }

void host_pause(const char *message) {
    do {
        puts(message);
        char buf[4] = {0};
        const char *response = fgets(buf, sizeof(buf), stdin);
        const unsigned char first =
            (unsigned char)(response == NULL ? ' ' : response[0]);
        const char ch = (char)(islower(first) ? toupper(first) : first);
        switch (ch) {
            case 'G': return;
            case 'X': host_exit(EXIT_SUCCESS);
        }
        puts("PROGRAM IS PAUSED.  TYPE 'G' (RETURN) TO GO OR "
             "'X' (RETURN) TO EXIT.");
    } while (1);
}
)";
}

constexpr std::string_view c_generator::usage_function() {
    return R"(
void usage() {
    printf(
        "\nOptions:\n\n"
        "-CAPS            Capitalize all letters in the keyboard input\n"
        "                 handler. Disabled by default.\n\n"
        "-c<file>         Load a core-image file into program memory during\n"
        "                 initialization.\n\n"
        "-d<date>         Force DATE to return the specified date. Allows\n"
        "                 many formats, but these are recommended:\n"
        "                   yyyy-mm-dd   example:  -d1977-01-01\n"
        "                   dd-MMM-yy    example:  -d01-JAN-77\n\n"
        "-t<time>         Force TIME to return the specified time.\n"
        "                   hh:mm        example:  -t13:35\n\n"
        "-f<name>=<path>  Substitutes <path> whenever the program tries to\n"
        "                 open a file named <name>.\n"
        "                   example:  -fTEXT=../adven.dat\n\n"
        "-y2k[-]          By default, DATE applies a hack to dates after 1999\n"
        "                 in an attempt to bypass Y2K bugs. To disable this\n"
        "                 hack, put '-' after the flag:  -y2k-\n\n"
        "-h               Displays this option summary.\n\n");
}
)";
}

}
