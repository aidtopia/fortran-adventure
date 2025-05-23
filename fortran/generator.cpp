#include "aid/experimental/fortran/generator.h"

#include "aid/experimental/fortran/symbols.h"
#include "aid/libs/core/filesystem.h"

#include <algorithm>
#include <format>
#include <fstream>
#include <print>

namespace aid::fortran {

namespace {
    static bool is_array(symbol_info const &a) {
        return !a.shape.empty();
    }
    static bool is_argument(symbol_info const &a) {
        return a.kind == symbolkind::argument;
    }
    static bool is_unreferenced_argument(symbol_info const &a) {
        return a.kind == symbolkind::argument && !a.referenced;
    }
    static bool is_common(symbol_info const &a) {
        return a.kind == symbolkind::common;
    }
    static bool is_local(symbol_info const &a) {
        return a.kind == symbolkind::local;
    }
    static bool is_return_value(symbol_info const &a) {
        return a.kind == symbolkind::retval;
    }
    static bool by_block_index(symbol_info const &a, symbol_info const &b) {
        if (a.comdat < b.comdat) return true;
        if (a.comdat > b.comdat) return false;
        if (a.index < b.index) return true;
        return false;
    }
    static bool by_index(symbol_info const &a, symbol_info const &b) {
        return (a.index < b.index);
    }
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
}

void generator::generate(
    std::filesystem::path const &path,
    program const &prog
) {
    auto constexpr extensions = std::array<std::string_view, 1>{".c"};
    auto target = core::resolve_filename(path.native(), extensions);
    auto out = std::ofstream(target);
    generate(out, prog);
}

void generator::generate(std::ostream &out, program const &source) {
    auto g = generator{out};
    g.generate_program(source);
}

void generator::generate_program(program const &prog) const {
    spew("// Generated Code\n");

    generate_definitions();

    auto const subprograms = prog.extract_subprograms();
    if (!subprograms.empty()) {
        // BUG:  Creates prototypes for statement definition functions, which
        // are actually function-style preprocessor macros.
        spew("\n");
        for (auto const *pu : subprograms) {
            generate_prototype(*pu);
            spew(";\n");
        }
    }

    generate_common_blocks(prog);

    generate_unit(prog);

    for (auto const *pu : subprograms) {
        generate_unit(*pu);
    }

    spew("\nint main() {{\n"
         " io_init();\n"
         " sub{}();\n"
         " return 0;\n"
         "}}\n", prog.unit_name());

    spew("\n// End of Generated Code\n");
}

void generator::generate_definitions() const {
    spew("{}", R"(
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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
#define TMP_WRAP(EXPR) (tmp_push(), tmp_pop(EXPR))

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
word_t not(word_t x) { return logical(x < 0); }
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
})");

    spew("{}", R"(
#define IO_MAX_UNITS  4
struct iocontext {
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
} io;

void io_init() {
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
}

void io_open(word_t unit, const char *name) {
    assert(1 <= unit && unit <= IO_MAX_UNITS);
    if (1 <= unit && unit <= IO_MAX_UNITS) {
        if (io.units[unit] != NULL) {
            fclose(io.units[unit]);
            io.units[unit] = NULL;
        }
#ifdef _MSC_VER
        fopen_s(&io.units[unit], name, "r");
#else
        io.units[unit] = fopen(name, "r");
#endif
    }
    assert(io.units[unit] != NULL);
}

void io_loadrecord(word_t unit) {
    const size_t size = sizeof(io.record) - 1;  // reserve room for terminator
    size_t i = 0;
    FILE *source = io.units[unit];
    assert(source != NULL);
    if (source != NULL) {
        while (i < size) {
            const int ch = fgetc(source);
            if (ch == EOF || ch == '\n') break;
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
    while (value > 0) { pdst[--width] = '0' + value % 10; value /= 10; }
    if (*pvar < 0)    { pdst[--width] = '-'; }
    while (width > 0) { pdst[--width] = ' '; }
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
})");

    spew("{}", R"(
// Elemental functions available as part of Fortran:
word_t fnIABS(word_t *x) { return (*x < 0) ? -*x : *x; }
word_t fnMIN0(word_t *a, word_t *b) { return (*a <= *b) ? *a : *b; }
word_t fnMAX0(word_t *a, word_t *b) { return (*a >= *b) ? *a : *b; }
word_t fnMOD(word_t *quotient, word_t *divisor) { return *quotient % *divisor; }
)");

    spew("{}", R"(
// Emulated subroutines from the PDP-10 system library.

// Packs five ASCII characters into a word_t as PDP-10 does.
word_t pack_A5(char c0, char c1, char c2, char c3, char c4) {
    return
        (((((((((word_t)(c0) << 7) + c1) << 7) + c2) << 7) + c3) << 7) + c4)
        << 1;
}

// Returns the date as two `word_t`s of text, in the form 'dd-MMM-yy '.
// HACK: The `yy` may use a non-digit to fool Adventure into handling years
// from 2000 through 2050.
void subDATE(word_t r[2]) {
    const char  mmm[] = "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";
    time_t t = time(0);
    struct tm now = *localtime(&t);
    const int yy = now.tm_year; //now.tm_year % 100;
    r[0] = pack_A5((now.tm_mday < 10) ? ' ' : (char)('0' + (now.tm_mday / 10)),
                   (char)('0' + now.tm_mday % 10),
                   '-',
                   mmm[now.tm_mon * 3 + 0],
                   mmm[now.tm_mon * 3 + 1]);
    r[1] = pack_A5(mmm[now.tm_mon * 3 + 2],
                   '-',
                   (char)('0' + yy / 10), // produced non-digit
                   (char)('0' + yy % 10),
                   ' ');
}

// Returns the time as text, in the form 'hh:mm'.  Both the hours and the
// minutes are always two digits, with a leading '0' if necessary.  The actual
// library function has an optional second argument to receive seconds and
// tenths, but that's not used by Adventure.
void subTIME(word_t *r) {
    time_t t = time(0);
    struct tm now = *localtime(&t);
    r[0] = pack_A5((char)('0' + now.tm_hour / 10),
                   (char)('0' + now.tm_hour % 10),
                   ':',
                   (char)('0' + now.tm_min / 10),
                   (char)('0' + now.tm_min % 10));
}

)");
}

void generator::generate_common_blocks(program const &prog) const {
    auto const commons = prog.extract_symbols(is_common, by_block_index);
    if (commons.empty()) return;
    spew("\n// Common Blocks\n");
    auto block = symbol_name{};
    auto size = std::size_t{0};
    for (auto const &var : commons) {
        if (var.comdat != block) {
            if (size > 0) spew("word_t common{}[{}];\n", block, size);
            block = var.comdat;
            size = 0;
        }
        size += is_array(var) ? array_size(var.shape) : 1;
    }
    if (size > 0) spew("word_t common{}[{}];\n", block, size);
}

void generator::generate_prototype(unit const &u) const {
    auto const retval = u.extract_symbols(is_return_value);
    if (retval.empty()) {
        spew("void sub{}(", u.unit_name());
    } else {
        spew("word_t fn{}(", u.unit_name());
    }
    auto const parameters = u.extract_symbols(is_argument, by_index);
    for (auto const &param : parameters) {
        if (is_return_value(param)) continue;
        if (param.index > 1) spew(", ");
        spew("word_t *{}", name(param));
    }
    spew(")");
}

void generator::generate_unit(unit const &u) const {
    if (u.code().empty()) return;
    spew("\n");
    generate_prototype(u);
    spew(" {{\n");

    auto const retval = u.extract_symbols(is_return_value); 
    if (!retval.empty()) {
        spew(" static word_t {}[1];  // return value\n", name(retval.front()));
    }

    // Avoid compiler warnings for unused parameters.
    auto const dummies = u.extract_symbols(is_unreferenced_argument);
    for (auto const dummy : dummies) {
        spew(" (void) {};  // unused argument\n", name(dummy));
    }

    auto const commons = u.extract_symbols(is_common, by_block_index);
    auto block = symbol_name{};
    auto offset = std::size_t{0};
    for (auto const &common : commons) {
        if (common.comdat != block) offset = 0;
        block = common.comdat;
        if (common.referenced) {
            spew(" word_t *{} = &common{}[{}];\n", name(common), block, offset);
            // Since the common variable is a reference into the common block,
            // we cannot initialize it in the declaration.
            for (std::size_t i = 0; i < common.init_data.size(); ++i) {
                spew(" {}[{}] = {};\n", name(common), i, common.init_data[i]);
            }
        }
        // Its size still matters to the layout, even if it wasn't referenced.
        offset += is_array(common) ? array_size(common.shape) : 1;
    }

    auto const locals = u.extract_symbols(is_local);
    for (auto const &local : locals) {
        generate_variable_definition(local);
    }

    for (auto const format : u.formats()) {
        spew(" static const char fmt{}[] = \"{}\";\n",
             format.first, escape_string(format.second));
    }

    auto const &code = u.code();
    for (auto const &statement : code) {
        if (auto const c_stmt = statement->generate(u); !c_stmt.empty()) {
            spew(" {}\n", c_stmt);
        }
    }

    auto const return_label = u.find_symbol(symbol_name{"return"});
    if (return_label.referenced) {
        spew(" L{}: ", return_label.name);
        if (retval.empty()) spew("return;\n");
        else                spew("return *{};\n", name(retval.front()));
    }
    auto const stop_label   = u.find_symbol(symbol_name{"stop"});

    if (stop_label.referenced) {
        spew(" L{}: exit(EXIT_SUCCESS);\n", stop_label.name);
    }
    spew("}}\n");
}

void generator::generate_variable_definition(symbol_info const &variable) const {
    if (!variable.referenced) return;
    if (is_array(variable)) {
        generate_array_definition(variable);
    } else {
        generate_scalar_definition(variable);
    }
}

void generator::generate_array_definition(symbol_info const &array) const {
    // Locals are now `static`, so we don't need to explicitly zero them.
    if (!array.referenced) return;
    spew(" static word_t {}[{}]", name(array), array_size(array.shape));
    if (!array.init_data.empty()) {
        spew(" = {{{}", array.init_data[0]);
        for (std::size_t i = 1; i < array.init_data.size(); ++i) {
            spew(",{}", array.init_data[i]);
        }
        spew("}}");
    }
    spew(";\n");
}

void generator::generate_scalar_definition(symbol_info const &scalar) const {
    // Locals are now `static`, so we don't need to explicitly zero them.
    spew(" static word_t {}[1]", name(scalar));
    if (!scalar.init_data.empty()) {
        spew(" = {{{}}}", scalar.init_data[0]);
    }
    spew(";\n");
}

}
