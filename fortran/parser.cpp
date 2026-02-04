#include "parser.h"

#include "machine.h"
#include "statements.h"
#include "unit.h"
#include "utility.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <print>
#include <string_view>

namespace aid::fortran {

parser::expected<program> parser::parse_files(
    std::span<std::filesystem::path> paths
) {
    if (paths.empty()) return error("no source code provided");
    std::vector<std::filesystem::path> sources;
    auto const cwd = std::filesystem::current_path();
    auto const base = cwd.parent_path();

    // Concatenate the files into a single stream.
    std::string buffer;
    for (auto const &path : paths) {
        auto in = std::ifstream(path);
        if (!in) return error("count not find or open \"{}\"", path.string());
        buffer.append(std::istreambuf_iterator<char>{in},
                      std::istreambuf_iterator<char>{});
        auto const source =
            (path.is_relative() ? cwd/path : path).lexically_proximate(base);
        sources.push_back(source);
    }
    auto stream = std::stringstream{std::move(buffer)};
    auto parsed = parse_stream(stream);
    if (!parsed) return error_of(std::move(parsed));
    auto main_program = std::move(parsed).value();
    main_program.set_source_files(sources);

    // If the program wasn't given a name, use the first source file's name.
    if (main_program.name().empty()) {
        auto const &first = paths.front();
        if (first.has_stem()) {
            auto const stem = to_upper_ascii(first.stem().string());
            auto const name = symbol_name{stem};
            main_program.set_name(name);
        }
    }
    return main_program;
}

parser::expected<program> parser::parse_stream(std::istream &in) {
    auto p = parser{in};
    return p.parse_statements();
}

parser::expected<program> parser::parse_statements() {
    while (next_statement()) {
        auto const stmt = parse_full_statement();
        if (!stmt.has_value()) {
            return error("{}\n{}\n", stmt.error().message(), m_statement);
        }
        if (stmt.value() == nullptr) continue;
        m_subprogram.add_statement(stmt.value());
    }
    // We do not call end_program here.  That was done when the END statement of
    // the program was reached.  And there were probably subprograms after that.
    return std::move(m_program);
}

parser::expected<statement_t> parser::parse_full_statement() {
    auto number = parse_statement_number_field();
    if (!number) return error_of(std::move(number));
    m_statement_number = number.value();
    crush_statement(m_statement);
    begin_statement_field();
    return parse_statement(m_statement_number);
}

parser::expected<statement_number_t> parser::parse_statement_number_field() {
    // The statement number field is in column_1 up to column_6.
    auto const length = std::min(column_6 - column_1, m_statement.size());
    auto const field = std::string_view(m_statement.data() + column_1, length);
    auto number = statement_number_t{0};
    auto has_number = false;
    // Leading '0's and all blanks--including those between digits--are ignored.
    for (auto const &ch : field) {
        if ('0' <= ch && ch <= '9') {
            number *= 10;
            number += ch - '0';
            has_number = true;
        } else if (ch != ' ') {
            return error("unexpected character '{}' in statement number field",
                         ch);
        }
    }
    return has_number ? number : no_statement_number;
}

parser::expected<statement_t>
parser::parse_statement(statement_number_t number) {
    // Recursive descent parsing, even with access to the symbol table, isn't
    // quite up to the task of resolving all ambiguities.  Because spaces are
    // insignificant, there are times we wouldn't know whether the first token
    // is a keyword or an identifier.
    //
    //     IMPLICITINTEGER(A-Z)
    //     IMPLICITINTEGER(13)=42
    //
    // The solution is to first analyze the statement as a whole to determine
    // whether it's "shaped" like an assignment statement.  If not, then the
    // first token is a keyword.
    auto kw = match_assignment_statement() ?
        keyword::assignment : keyword::unknown;
    if (kw == keyword::unknown) {
        auto keyword = parse_keyword();
        if (!keyword) return error_of(std::move(keyword));
        kw = keyword.value();
    }
    assert(kw != keyword::unknown);
    auto statement = parse_identified_statement(kw, number);
    if (!statement) return error_of(std::move(statement));
    if (statement.value() != nullptr) {
        statement.value()->set_statement_number(number);
    }
    return std::move(statement).value();
}

parser::expected<statement_t>
parser::parse_identified_statement(keyword kw, statement_number_t number) {
    auto const original_phase = m_phase;

    // In phase 0, we're looking to start the program or another subprogram.
    if (m_phase == phase0) switch (kw) {
        case keyword::PROGRAM:      return parse_program();
        case keyword::INTEGER:      return parse_function(datatype::INTEGER);
        case keyword::LOGICAL:      return parse_function(datatype::LOGICAL);
        case keyword::FUNCTION:     return parse_function();
        case keyword::SUBROUTINE:   return parse_subroutine();
        default: {
            // Adventure dives right in without a PROGRAM statement.
            if (m_program.has_main_subprogram()) {
                return error("cannot implicitly begin a PROGRAM because only "
                             "one PROGRAM is allowed");
            }
            auto begin_result = begin_main_subprogram(symbol_name{});
            if (!begin_result) return error_of(std::move(begin_result));
            break;
        }
    }

    // A couple statement types are allowed through most phases.
    if (phase1 <= m_phase && m_phase <= phase4) switch (kw) {
        case keyword::FORMAT:       return parse_format(number);
        case keyword::END:          return parse_end();
    }

    // Phase 1 is for IMPLICITs, which must come before all other specification
    // statements.  (Some implementations are lax about this ordering, but the
    // Adventure code follows the rules, so we don't need to worry about that.)
    // IMPLICIT statements adjust the parse-time rules but aren't directly
    // stored in the unit.  (In fact, they probably could be implemented in the
    // parser class rather than the unit class.)
    if (m_phase == phase1) switch (kw) {
        case keyword::IMPLICIT:     return parse_implicit();
        default:                    m_phase = phase2; break;
    }

    // Phase 2 allows non-executable statements (like specifications).  These
    // mostly add information to the current unit's symbol table.
    if (m_phase == phase2) switch (kw) {
        case keyword::COMMON:       return parse_common();
        case keyword::DIMENSION:    return parse_dimension();
        case keyword::EXTERNAL:     return parse_external();
        case keyword::INTEGER:      return parse_type_specification(datatype::INTEGER);
        case keyword::LOGICAL:      return parse_type_specification(datatype::LOGICAL);
        case keyword::REAL:         return parse_type_specification(datatype::REAL);
        default:
            // At this transition from phase2 to phase3, we can determine the
            // types for any symbols that are still unknown.
            infer_types();
            m_phase = phase3;
            break;
    }

    // DATA statements can happen after the specifications.
    if (phase3 <= m_phase && m_phase <= phase4) switch (kw) {
        case keyword::DATA:         return parse_data();
    }

    // Phase 3 is for arithmetic function definitions, which look like
    // assignments.  Note that if we're here and we get an actual, executable
    // assignment statement, the phase will be advanced.
    if (m_phase == phase3) switch (kw) {
        case keyword::assignment:   return parse_assignment();

        // Strictly speaking, these should not happen before phase 3.  A warning
        // will be issued.
        case keyword::INTEGER:      return parse_type_specification(datatype::INTEGER);
        case keyword::LOGICAL:      return parse_type_specification(datatype::LOGICAL);
        case keyword::REAL:         return parse_type_specification(datatype::REAL);

        default:                    m_phase = phase4; break;
    }

    // Phase 4 brings us executable statements (and closes the door on specs).
    if (m_phase == phase4) switch (kw) {
        case keyword::assignment:   return parse_assignment();
        case keyword::ACCEPT:       return parse_accept();
        case keyword::CALL:         return parse_call();
        case keyword::CONTINUE:     return parse_continue();
        case keyword::DO:           return parse_do();
        case keyword::GOTO:         return parse_goto();
        case keyword::IF:           return parse_if();
        case keyword::OPEN:         return parse_open();
        case keyword::PAUSE:        return parse_pause();
        case keyword::READ:         return parse_read();
        case keyword::RETURN:       return parse_return();
        case keyword::STOP:         return parse_stop();
        case keyword::TYPE:         return parse_type();
    }

    m_phase = original_phase;
    return error("unhandled keyword--wrong phase?");
}

parser::expected<statement_t> parser::parse_program() {
    auto const name = parse_identifier();
    if (name.empty()) return error("PROGRAM requires a name");
    if (!at_eol()) return error("unexpected token after PROGRAM statement");
    if (m_program.has_main_subprogram()) {
        return error("multiple PROGRAM statements are not allowed ('{}')",
                     name);
    }
    auto begin_result = begin_main_subprogram(name);
    if (!begin_result) return error_of(std::move(begin_result));
    return nullptr;
}

parser::expected<statement_t> parser::parse_function(datatype type) {
    if (type != datatype::unknown && !accept(keyword::FUNCTION)) {
        return error("expected 'FUNCTION' after '{}'", type);
    }
    auto const name = parse_identifier();
    if (name.empty()) return error("FUNCTION requires a name");
    auto params = parse_parameter_list();
    if (!params) return error_of(std::move(params));
    if (params.value().empty()) {
        return error("FUNCTION {} must have at least one parameter", name);
    }
    if (!at_eol()) return error("unexpected token after FUNCTION statement");

    auto begin_result = begin_subprogram(name);
    if (!begin_result) return error_of(std::move(begin_result));

    // The function name is used for the return value.
    auto retval = m_subprogram.find_symbol(name);
    retval.kind = symbolkind::retval;
    retval.type = type;
    m_subprogram.update_symbol(retval);

    // We also add its parameters, but we don't know their types yet.
    auto index = 1u;
    for (auto const &param : params.value()) {
        auto symbol = m_subprogram.find_symbol(param);
        symbol.kind = symbolkind::argument;
        symbol.index = index++;
        m_subprogram.update_symbol(symbol);
    }
    return nullptr;
}

parser::expected<statement_t> parser::parse_subroutine() {
    auto const name = parse_identifier();
    if (name.empty()) return error("SUBROUTINE requires a name");

    // Unlike a FUNCTION, a SUBROUTINE does not require any parameters nor even
    // empty parentheses.
    auto params = parameter_list_t{};
    if (match('(')) {
        // But if it does have an opening parenthesis, there are parameters.
        auto param_list = parse_parameter_list();
        if (!param_list) return error_of(std::move(param_list));
        params = std::move(param_list).value();
    }

    if (!at_eol()) return error("unexpected token after SUBROUTINE statement");

    auto begin_result = begin_subprogram(name);
    if (!begin_result) return error_of(std::move(begin_result));

    // Add the subroutine's parameters.  We don't know their types yet.
    auto index = 1u;
    for (auto const &param : params) {
        auto symbol = m_subprogram.find_symbol(param);
        symbol.kind = symbolkind::argument,
        symbol.index = index++;
        m_subprogram.update_symbol(symbol);
    }
    return nullptr;
}

parser::expected<statement_t> parser::parse_end() {
    if (!at_eol()) return error("unexpected token after END statement");
    if (m_phase == phase0) {
        return error("END statement while outside translation unit");
    }
    auto end_result =
        m_current_subprogram_is_main ? end_main_subprogram()
                                     : end_subprogram();
    if (!end_result) return error_of(std::move(end_result));

    // We cannot return a statement because code up the call chain would
    // attempt to add it to the unit after the unit has been closed.
    return nullptr;
}

parser::expected<statement_t>
parser::parse_arithmetic_function(symbol_name const &name) {
    if (m_arithmetic_functions.contains(name)) {
        return error("cannot re-define arithmetic function '{}'", name);
    }
    auto parameters = parse_parameter_list();
    if (!parameters) return error_of(std::move(parameters));
    if (parameters.value().empty()) {
        return error("arithmetic function {} requires at least one parameter",
                     name);
    }
    if (!accept('=')) {
        return error("expected '=' in arithmetic function definition");
    }
    auto definition =
        parse_arithmetic_function_expression(parameters.value());
    if (!definition) return error_of(std::move(definition));
    if (!at_eol()) {
        return error("unexpected token after arithmetic function definition");
    }
    auto symbol = m_subprogram.find_symbol(name);
    symbol.kind = symbolkind::internal;
    symbol.type = inferred_type(symbol);
    symbol.index = static_cast<unsigned>(parameters.value().size());
    m_subprogram.update_symbol(symbol);
    m_arithmetic_functions[symbol.name] =
        arithmetic_function_t{name, std::move(parameters).value(),
                              std::move(definition).value()};
    return nullptr;
}

parser::expected<statement_t> parser::parse_common() {
    do {
        auto block = symbol_name{};
        if (accept('/')) {
            block = parse_identifier();
            // It's OK if block is empty.
            if (!accept('/')) {
                return error("expected '/' after COMMON block name");
            }
        }

        do {
            auto name = parse_identifier();
            if (name.empty()) continue;
            auto symbol = m_subprogram.find_symbol(name);
            symbol.kind = symbolkind::common;
            symbol.comdat = block;
            symbol.index = next_common_count(block);
            symbol.type = inferred_type(symbol);

            auto shape = symbol.shape;
            if (match('(')) {
                // Although it's not used by Adventure, a variable in a COMMON
                // specification may include array dimensions instead of
                // requiring a separate DIMENSION specification.
                auto s = parse_array_shape();
                if (!s) return error_of(std::move(s));
                shape = s.value();
            }
            if (symbol.shape != shape) {
                if (!symbol.shape.empty()) {
                    return
                        error("in COMMON /{}/, dimensions for {} are not the "
                              "same as previously specified", block, name);
                }
                symbol.shape = shape;
            }

            m_subprogram.update_symbol(symbol);
        } while (accept(','));
    } while (!at_eol());

    return nullptr;
}

parser::expected<statement_t> parser::parse_data() {
    do {
        auto variable_list = parse_variable_list();
        if (!variable_list) return error_of(std::move(variable_list));
        auto data_list = parse_data_list();
        if (!data_list) return error_of(std::move(data_list));

        // Now match up the variables with the data values.
        auto data_iter = data_list.value().begin();
        auto const data_end = data_list.value().end();
        for (auto const &item : variable_list.value()) {
            auto symbol = m_subprogram.find_symbol(item.variable);
            symbol.type = inferred_type(symbol);
            if (symbol.kind == symbolkind::common) {
                // We let this go with just a warning because Adventure relies
                // on it and it doesn't seem to cause any harm.  I noticed that
                // SUPN0350 and others had to address this to compile with F77.
                warn("common variable '{}' should not appear in a DATA "
                     "statement outside a BLOCK DATA subprogram",
                     item.variable);  // also, we don't support BLOCK DATA
            }
            if (symbol.shape.empty()) {     // scalar
                assert(item.indices.empty());
                if (data_iter == data_end) {
                    return error("more variables than values in DATA "
                                 "statement");
                }
                symbol.init_data.push_back((*data_iter++).value);
            } else {                        // array
                // TODO:  Generalize for more than 1 dimension.
                assert(item.indices.size() == 1);
                auto const &control = item.index_control;
                if (control.step == 0) {
                    return error("induction step cannot be 0");
                }
                if ((control.init < control.limit && control.step < 0) ||
                    (control.init > control.limit && control.step > 0)
                ) {
                    return error("induction step is in the wrong direction");
                }
                auto const lower = std::min(control.init, control.limit);
                auto const upper = std::max(control.init, control.limit);
                auto const &subscript = item.indices[0];
                auto &init_data = symbol.init_data;
                init_data.reserve(core_size(symbol));
                auto const &shape = symbol.shape;
                for (machine_word_t i = control.init;
                     lower <= i && i <= upper;
                     i += control.step
                ) {
                    if (data_iter == data_end) {
                        return error("more variables than values in DATA "
                                     "statement");
                    }
                    auto const index =
                        subscript.coefficient*i + subscript.offset -
                        shape[0].minimum;
                    if (init_data.size() == static_cast<std::size_t>(index)) {
                        init_data.push_back((*data_iter++).value);
                    } else {
                        init_data.resize(index + 1);
                        init_data[index] = (*data_iter++).value;
                    }
                }
            }
            m_subprogram.update_symbol(symbol);
        }
        if (data_iter != data_end) {
            return error("more values than variables in DATA statement");
        }
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after DATA statement");
    return nullptr;
}

parser::expected<statement_t> parser::parse_dimension() {
    auto constexpr array_size_limit =
        std::numeric_limits<decltype(array_size(array_shape{}))>::max() / 4;
    do {
        auto const variable = parse_identifier();
        if (variable.empty()) return error("missing variable in DIMENSION");
        auto shape = parse_array_shape();
        if (!shape) return error_of(std::move(shape));
        if (array_size(shape.value()) >= array_size_limit) {
            return error("array {} requires more than {} elements",
                         variable, array_size_limit);
        }
        auto symbol = m_subprogram.find_symbol(variable);
        if (symbol.shape == shape.value()) continue;
        if (!symbol.shape.empty()) {
            return error("attempted to re-dimension {}", variable);
        }
        symbol.shape = std::move(shape).value();
        m_subprogram.update_symbol(std::move(symbol));
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after DIMENSION statement");
    return nullptr;
}

parser::expected<statement_t> parser::parse_external() {
    do {
        auto const name = parse_identifier();
        if (name.empty()) continue;
        auto symbol = m_subprogram.find_symbol(name);
        symbol.kind = symbolkind::external;
        symbol.type = datatype::none;
        m_subprogram.update_symbol(symbol);
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after EXTERNAL statement");
    return nullptr;
}

parser::expected<statement_t> parser::parse_format(statement_number_t number) {
    if (number == no_statement_number) {
        return error("FORMAT requires a statement number");
    }

    auto fields = parse_field_list();
    if (!fields) return error_of(std::move(fields));
    if (!at_eol()) return error("unexpected token after FORMAT statement");

    auto const name = symbol_name(number);
    auto symbol = m_subprogram.find_symbol(name);
    if (symbol.kind == symbolkind::label) {
        return error("statement {} cannot be both a FORMAT and branch target; "
                     "branch targets must be executable statements", number);
    }
    assert(symbol.type == datatype::unknown);
    symbol.kind = symbolkind::format;
    symbol.type = datatype::none;
    m_subprogram.update_symbol(symbol);
    m_subprogram.add_format(name, std::move(fields).value());
    return nullptr;
}

parser::expected<statement_t> parser::parse_implicit() {
    do {
        auto const kw = parse_keyword().value_or(keyword::unknown);
        auto const type = kw == keyword::INTEGER ? datatype::INTEGER :
                          kw == keyword::LOGICAL ? datatype::LOGICAL
                                                 : datatype::unknown;
        if (type == datatype::unknown) {
            return error("IMPLICIT supports only INTEGER and LOGICAL");
        }
        auto prefixes = parse_implicit_prefixes();
        if (!prefixes) return error_of(std::move(prefixes));
        for (auto const ch : prefixes.value()) m_implicit.set(ch, type);
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after IMPLICIT statement");
    return nullptr;
}

parser::expected<parser::prefix_set_t> parser::parse_implicit_prefixes() {
    if (!accept('(')) return error("expected '(' in IMPLICIT statement");
    auto prefixes = prefix_set_t{};
    if (match_letter()) {
        do {
            auto first = consume();
            if (accept('-')) {
                if (!match_letter()) {
                    return error("IMPLICIT prefixes must be letters");
                }
                auto last = consume();
                if (last < first) std::swap(first, last);
                for (auto ch = first; ch <= last; ++ch) {
                    prefixes.insert(ch);
                }
            } else {
                prefixes.insert(first);
            }
        } while (accept(','));
    }
    if (!accept(')')) return error("missing ')' in IMPLICIT statement");
    return prefixes;
}

parser::expected<statement_t> parser::parse_type_specification(datatype type) {
    if (m_phase == phase3) {
        warn("type specifications should come before any DATA statement or "
             "arithmetic function definition");
    }
    if (type == datatype::REAL) {
        warn("support for REAL is tentative and incomplete");
    }
    do {
        auto const variable = parse_identifier();
        if (variable.empty()) {
            return error("expected variable name in type specification");
        }
        auto symbol = m_subprogram.find_symbol(variable);
        if (symbol.type == type) continue;
        if (symbol.type != datatype::unknown) {
            return error("previous type of {} conflicts with this type "
                         "specification", variable);
        }
        symbol.type = type;
        m_subprogram.update_symbol(std::move(symbol));
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after type specification");
    return nullptr;
}

parser::expected<statement_t> parser::parse_assignment() {
    auto const name = parse_identifier();
    if (name.empty()) return error("expected lvalue");
    auto symbol = m_subprogram.find_symbol(name);

    if (match('(') && symbol.shape.empty()) {
        // Looks like a arithmetic function definition.
        if (m_phase > phase3) {
            return error(
                "arithmetic function {} must be defined before the first "
                "executable statement\n", name);
        }
        return parse_arithmetic_function(name);
    }

    // A regular assignment is an executable statement, so bump the phase if
    // necessary.
    if (m_phase <= phase3) m_phase = phase4;

    auto lvalue = expression_t{nullptr};
    if (match('(')) {
        if (symbol.kind == symbolkind::subprogram) {
            return error("did you mean to CALL {}?", name);
        }
        auto subscripts = parse_subscript_list();
        if (!subscripts) return error_of(std::move(subscripts));
        if (subscripts.value().size() != symbol.shape.size()) {
            return error("wrong number of subscripts for {}", name);
        }
        lvalue =
            std::make_shared<array_index_node>(name, symbol.shape,
                                               std::move(subscripts).value());
    } else {
        lvalue = std::make_shared<variable_node>(name);
    }

    if (!accept('=')) {
        assert(false);
        return error("statement was misidentified as an assignment?!");
    }

    infer_type_and_update(symbol);

    auto rhs = parse_expression();
    if (!rhs) return error_of(std::move(rhs));

    if (!at_eol()) return error("unexpected token after assignment");

    // If we're assigning to a LOGICAL, we'll ensure the value is actually
    // .TRUE. or .FALSE. rather than an INTEGER with an implicit truthiness,
    // in case the program compares it to .TRUE. or .FALSE. explicitly.
    auto rvalue =
        symbol.type == datatype::LOGICAL
            ? std::make_shared<unary_node>(operator_t::as_logical,
                                           std::move(rhs).value())
            : std::move(rhs).value();

    return make<assignment_statement>(lvalue, std::move(rvalue));
}

parser::expected<statement_t> parser::parse_accept() {
    auto const unit = std::make_shared<constant_node>(0);
    auto f = parse_statement_number();
    if (!f) return error_of(std::move(f));
    auto format = std::move(f).value();

    auto io_list = io_list_t{};
    if (accept(',')) {
        auto list = parse_io_list();
        if (!list) return error_of(std::move(list));
        io_list = std::move(list).value();
    }
    // Re-using read_statement with a special unit number.
    return make<read_statement>(unit, std::move(format), std::move(io_list));
}

parser::expected<statement_t> parser::parse_call() {
    auto const name = parse_identifier();
    if (name.empty()) return error("CALL requires a subprogram name");
    auto arguments = argument_list_t{};
    if (match('(')) {
        auto xargs = parse_argument_list();
        if (!xargs) return error_of(std::move(xargs));
        arguments = std::move(xargs).value();
    }
    if (!at_eol()) return error("unexpected token after CALL statement");

    auto symbol = m_subprogram.find_symbol(name);
    if (!m_subprogram.has_symbol(name)) {
        // We infer that it's a subroutine.
        symbol.type = datatype::none;
        symbol.kind = symbolkind::subprogram;
        symbol.index = static_cast<unsigned>(arguments.size());
        m_subprogram.update_symbol(symbol);
    }

    switch (symbol.kind) {
        case symbolkind::subprogram:
            if (arguments.size() != symbol.index) {
                return error("wrong number of arguments in CALL of {}", name);
            }
            return make<call_statement>(name, std::move(arguments));
        case symbolkind::argument:
            symbol.type = datatype::subptr;
            m_subprogram.update_symbol(symbol);
            return make<indirect_call_statement>(name, std::move(arguments));
        case symbolkind::internal:
            return error("cannot CALL {} because it's an arithmetic function",
                         name);
        default:
            break;
    }
    return error("cannot CALL {}", name);
}

parser::expected<statement_t> parser::parse_continue() {
    if (!at_eol()) return error("unexpected token after CONTINUE");
    return make<continue_statement>();
}

parser::expected<statement_t> parser::parse_do() {
    auto ends_at = parse_statement_number();
    if (!ends_at) return error_of(std::move(ends_at));
    auto control = parse_index_control();
    if (!control) return error_of(std::move(control));
    if (!at_eol()) return error("unexpected token after DO statement");

    auto const loop =
        std::make_shared<do_statement>(std::move(control).value());
    while (next_statement()) {
        auto stmt = parse_full_statement();
        if (!stmt) return error_of(std::move(stmt));
        if (stmt.value() != nullptr) loop->add(std::move(stmt).value());
        if (m_statement_number == ends_at.value()) return loop;
    }
    return error("expected statement number {} at end of loop body",
                 std::move(ends_at).value());
}

parser::expected<statement_t> parser::parse_goto() {
    if (match_digit()) return parse_unconditional_goto();
    if (match('(')) return parse_computed_goto();
    if (match_letter()) {
        return error("assigned GOTO statements are not supported");
    }
    return error("unsupported GOTO syntax");
}

parser::expected<statement_t> parser::parse_unconditional_goto() {
    auto target = parse_statement_number();
    if (!target) return error_of(std::move(target));
    if (!at_eol()) return error("unexpected tokens after unconditional GOTO");
    auto xa = add_branch_target(target.value());
    if (!xa) return error_of(std::move(xa));
    return make<goto_statement>(std::move(target).value());
}

parser::expected<statement_t> parser::parse_computed_goto() {
    if (!accept('(')) return error("bug parsing computed GOTO statement");
    auto targets = std::vector<statement_number_t>{};
    do {
        auto target = parse_statement_number();
        if (!target) return error_of(std::move(target));
        auto xa = add_branch_target(target.value());
        if (!xa) return error_of(std::move(xa));
        targets.push_back(std::move(target).value());
    } while (accept(','));
    if (!accept(')')) return error("missing ')' in computed GOTO");
    accept(',');  // Adventure (sometimes?) omits this comma.
    auto expr = parse_expression();
    if (!expr) return error_of(std::move(expr));
    if (!at_eol()) return error("syntax error in computed GOTO");
    return make<computed_goto_statement>(targets, std::move(expr).value());
}

parser::expected<statement_t> parser::parse_if() {
    if (!accept('(')) return error("expected parenthesized condition in IF");
    auto condition = parse_expression();
    if (!condition) return error_of(std::move(condition));
    if (!accept(')')) return error("missing ')' after IF condition");
    if (match_digit()) {
        auto nega = parse_statement_number();
        if (!nega) return error_of(std::move(nega));
        if (!accept(',')) return error("expected ',' in numeric IF statement");
        auto zero = parse_statement_number();
        if (!zero) return error_of(std::move(zero));
        if (!accept(',')) return error("expected ',' in numeric IF statement");
        auto posi = parse_statement_number();
        if (!posi) return error_of(std::move(posi));
        if (!at_eol()) return error("unexpected token after numeric IF");
        auto xa = add_branch_target(nega.value());
        if (!xa) return error_of(std::move(xa));
        xa      = add_branch_target(zero.value());
        if (!xa) return error_of(std::move(xa));
        xa      = add_branch_target(posi.value());
        if (!xa) return error_of(std::move(xa));
        return make<numeric_if_statement>(
            std::move(condition).value(), std::move(nega).value(),
            std::move(zero).value(), std::move(posi).value());
    }
    auto then = parse_statement(no_statement_number);
    if (!then) return error_of(std::move(then));
    if (then.value() == nullptr) {
        return error("missing then clause of logical IF statement");
    }
    // Technically, there are a handful of statement types you cannot use as the
    // then-clause of an IF statement: IF, DO, non-executable statements, etc.
    // But that's not worth checking now.
    return make<if_statement>(
        std::move(condition).value(), std::move(then).value());
}

parser::expected<statement_t> parser::parse_open() {
    // The OPEN statement used by Adventure is not described in the Fortran IV
    // manuals I've found online.  It is, however, described in an online
    // Fortran 77 reference hosted by Oracle.  That description seems consistent
    // with how Adventure uses it if we assume some synonyms, like NAME for FILE
    // and 'SEQIN' for 'SEQUENTIAL'.)
    // https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnaf/index.html
    if (!accept('(')) {
        return error("expected parenthesized parameters list after OPEN");
    }
    auto const bookmark = position();
    auto const unitkey = parse_open_keyword();
    if (unitkey.value() != openkey::UNIT || !accept('=')) m_it = bookmark;
    auto unit = parse_expression();
    if (!unit) return error_of(std::move(unit));
    auto access = std::string{"SEQUENTIAL"};
    auto file_name = std::filesystem::path{};
    while (accept(',')) {
        auto const key = parse_open_keyword();
        if (!key.has_value() || !accept('=')) {
            return error("parameters after UNIT must be named");
        }
        switch (key.value()) {
            case openkey::ACCESS: {
                auto param = parse_literal();
                if (!param) return error_of(std::move(param));
                access = std::move(param).value();
                break;
            }
            case openkey::FILE: {
                auto param = parse_literal();
                if (!param) return error_of(std::move(param));
                file_name = std::filesystem::path{std::move(param).value()};
                break;
            }
            case openkey::unknown: [[fallthrough]];
            default:
                return error("unknown open parameter");
        }
    }
    if (!accept(')')) return error("missing ')' in OPEN statement");
    if (!at_eol()) return error("unexpected token after OPEN statement");
    // Adventure uses 'SEQIN'.
    if (access == "SEQIN") access = "SEQUENTIAL";
    if (access != "SEQUENTIAL") {
        return error("unsupported file access type '{}'", access);
    }
    if (file_name.empty()) {
        // Documentation says a default name will be provided, but c'mon!
        return error("OPEN statement didn't specify a file name");
    }
    return make<open_statement>(std::move(unit).value(), file_name);
}

parser::expected<statement_t> parser::parse_pause() {
    auto message = std::string("TYPE G TO CONTINUE OR X TO EXIT");
    if (match('\'')) {
        auto text = parse_literal();
        if (!text) return error_of(std::move(text));
        message = std::move(text).value();
    } else if (match_digit(8)) {
        auto number = parse_integer(8, 6);
        if (!number) return error_of(std::move(number));
        message = std::format("{:o}", number.value());
    }
    if (!at_eol()) return error("unexpected token after PAUSE statement");
    return make<pause_statement>(message);
}

parser::expected<statement_t> parser::parse_read() {
    auto unit = expression_t{};
    auto format = statement_number_t{};
    if (accept('(')) {
        auto u = parse_expression();
        if (!u) return error_of(std::move(u));
        unit = std::move(u).value();
        if (accept(',')) {
            auto f = parse_statement_number();
            if (!f) return error_of(std::move(f));
            format = std::move(f).value();
        }
        if (!accept(')')) return error("expected ')' in READ statement");
    } else {
        auto f = parse_statement_number();
        if (!f) return error_of(std::move(f));
        format = std::move(f).value();
    }

    auto list = parse_io_list();
    if (!list) return error_of(std::move(list));
    auto io_list = std::move(list).value();
    if (!at_eol()) return error("unexpected token after READ statement");

    return make<read_statement>(std::move(unit), format, std::move(io_list));
}

parser::expected<statement_t> parser::parse_return() {
    if (!at_eol()) return error("extra tokens after RETURN");
    auto const retvals =
        m_subprogram.extract_symbols(
            [] (auto const &info) {
                return info.kind == symbolkind::retval;
            });
    if (retvals.size() > 1) return error("too many values for RETURN");
    return retvals.empty() ? make<return_statement>()
                           : make<return_statement>(retvals.front().name);
}

parser::expected<statement_t> parser::parse_stop() {
    if (!at_eol()) return error("extra tokens after STOP");
    return make<stop_statement>();
}

parser::expected<statement_t> parser::parse_type() {
    auto format = parse_statement_number();
    if (!format) return error_of(std::move(format));
    auto io_list = io_list_t{};
    if (accept(',')) {
        auto list = parse_io_list();
        if (!list) return error_of(std::move(list));
        io_list = std::move(list).value();
    }
    if (!at_eol()) return error("unexpected token after TYPE statement");
    return make<type_statement>(std::move(format).value(), std::move(io_list));
}

parser::expected<expression_t> parser::parse_expression() {
    auto alternative = parse_alternative_expression();
    if (!alternative) return error_of(std::move(alternative));
    auto lhs = std::move(alternative).value();
    for (;;) {
        auto const op = accept(operator_t::logic_xor) ? operator_t::logic_xor :
                        accept(operator_t::logic_eqv) ? operator_t::logic_eqv
                                                      : operator_t::none;
        if (op == operator_t::none) break;
        auto rhs = parse_alternative_expression();
        if (!rhs) return error_of(std::move(rhs));
        lhs = std::make_shared<binary_node>(lhs, op, std::move(rhs).value());
    }
    return std::move(lhs);
}

parser::expected<expression_t> parser::parse_alternative_expression() {
    auto compound = parse_compound_expression();
    if (!compound) return error_of(std::move(compound));
    auto lhs = std::move(compound).value();
    while (accept(operator_t::logic_or)) {
        auto rhs = parse_compound_expression();
        if (!rhs) return error_of(std::move(rhs));
        lhs = std::make_shared<binary_node>(
            lhs, operator_t::logic_or, std::move(rhs).value());
    }
    return std::move(lhs);
}

parser::expected<expression_t> parser::parse_compound_expression() {
    auto comparison = parse_comparison();
    if (!comparison) return error_of(std::move(comparison));
    auto lhs = std::move(comparison).value();
    while (accept(operator_t::logic_and)) {
        auto rhs = parse_comparison();
        if (!rhs) return error_of(std::move(rhs));
        lhs = std::make_shared<binary_node>(
            lhs, operator_t::logic_and, std::move(rhs).value());
    }
    return std::move(lhs);
}

parser::expected<expression_t> parser::parse_comparison() {
    auto lhs = parse_arithmetic_expression();
    if (!lhs) return error_of(std::move(lhs));
    if (!match('.')) return std::move(lhs);
    auto const bookmark = position();
    auto const op = parse_operator().value_or(operator_t::none);
    if (op != operator_t::compare_eq && op != operator_t::compare_ne  &&
        op != operator_t::compare_gt && op != operator_t::compare_gte &&
        op != operator_t::compare_lt && op != operator_t::compare_lte
    ) {
        m_it = bookmark;
        return std::move(lhs).value();
    }
    auto rhs = parse_arithmetic_expression();
    if (!rhs) return error_of(std::move(rhs));
    return std::make_shared<binary_node>(
        std::move(lhs).value(), op, std::move(rhs).value());
}

parser::expected<expression_t> parser::parse_arithmetic_expression() {
    auto term = parse_term();
    if (!term) return error_of(std::move(term));
    auto lhs = std::move(term).value();
    while (match('+') || match('-')) {
        auto const op = parse_operator().value_or(operator_t::none);
        auto rhs = parse_term();
        if (!rhs) return error_of(std::move(rhs));
        lhs = std::make_shared<binary_node>(lhs, op, std::move(rhs).value());
    }
    return std::move(lhs);
}

parser::expected<expression_t> parser::parse_term() {
    auto factor = parse_factor();
    if (!factor) return error_of(std::move(factor));
    auto lhs = std::move(factor).value();
    while (match('*') || match('/')) {
        auto const op = parse_operator().value_or(operator_t::none);
        if (op == operator_t::exponentiate) {
            return error("exponentiation is not supported");
        }
        auto rhs = parse_factor();
        if (!rhs) return error_of(std::move(rhs));
        lhs = std::make_shared<binary_node>(lhs, op, std::move(rhs).value());
    }
    return std::move(lhs);
}

parser::expected<expression_t> parser::parse_factor() {
    if (accept(operator_t::logic_not)) {
        auto factor = parse_factor();
        if (!factor) return error_of(std::move(factor));
        return
            std::make_shared<unary_node>(
                operator_t::logic_not, std::move(factor).value());
    }
    if (accept('-')) {
        auto factor = parse_factor();
        if (!factor) return error_of(std::move(factor));
        return
            std::make_shared<unary_node>(
                operator_t::negate, std::move(factor).value());
    }
    while (accept('+')) { /* unnecessary unary '+' */ }
    return parse_atom();
}

parser::expected<expression_t> parser::parse_atom() {
    if (accept('(')) {
        auto expr = parse_expression();
        if (!expr) return error_of(std::move(expr));
        if (!accept(')')) return error("expected ')' in expression");
        return std::move(expr).value();
    }
    if (match_letter()) {
        auto const name = parse_identifier();
        if (name.empty()) return error("expected identifier in expression");
        auto symbol = m_subprogram.find_symbol(name);

        if (!match('(')) {  // variables and externals
            infer_type_and_update(symbol);
            return std::make_shared<variable_node>(name);
        }

        if (!symbol.shape.empty()) {  // array subscripts
            auto subscripts = parse_subscript_list();
            if (!subscripts) return error_of(std::move(subscripts));
            if (subscripts.value().size() != symbol.shape.size()) {
                return error("array has {} dimension(s) but only {} "
                             "subscript(s) were provided", symbol.shape.size(),
                             subscripts.value().size());
            }
            return
                std::make_shared<array_index_node>(
                    name, symbol.shape, std::move(subscripts).value());
        }

        // function arguments
        auto arguments = parse_argument_list();
        if (!arguments) return error_of(std::move(arguments));
        auto const args = std::move(arguments).value();
        if (symbol.kind == symbolkind::local) {
            // Either the program has an error, or this is actually a subprogram
            // (specifically a FUNCTION) that hasn't yet been defined.
            symbol.kind = symbolkind::subprogram;
            symbol.type = inferred_type(symbol);
            assert(symbol.index == 0);
            symbol.index = static_cast<unsigned>(args.size());
            m_subprogram.update_symbol(symbol);
        }
        if (symbol.kind == symbolkind::subprogram &&
            symbol.type == datatype::none
        ) {
            return error("cannot invoke SUBROUTINE {} in an expression",
                         symbol.name);
        }
        if (args.size() != symbol.index) {
            return error("'{}' requires {} argument{}, but {} provided",
                symbol.name, symbol.index, symbol.index != 1 ? "s" : "",
                args.size());
        }
        if (symbol.kind == symbolkind::subprogram) {
            return std::make_shared<function_invocation_node>(name, args);
        } else if (symbol.kind == symbolkind::internal) {
            assert(m_arithmetic_functions.contains(symbol.name));
            return std::make_shared<inlined_internal_node>(
                m_arithmetic_functions[symbol.name], args);
        }
        return error("syntax error in expression near '{}'", name);
    }
    auto constant = parse_constant();
    if (!constant) return error_of(std::move(constant));
    return std::make_shared<constant_node>(std::move(constant).value().value);
}

parser::expected<expression_t> parser::parse_arithmetic_function_expression(
    parameter_list_t const &params
) {
    // In theory, any parameter of an arithmetic function may shadow another
    // symbol that's been declared.  They need to be in scope only while parsing
    // the expression that defines the function.  So we temporarily add them as
    // shadows, and them pop them right back off.  Any other variables
    // referenced in the arithmetic function are scoped to the unit.
    for (auto const &param : params) {
        auto symbol = symbol_info{};
        symbol.name = param;
        symbol.kind = symbolkind::shadow;
        symbol.type = inferred_type(symbol);
        m_subprogram.push_shadow(std::move(symbol));
    }
    auto const definition = parse_expression();
    m_subprogram.pop_shadows();
    return definition;
}

parser::expected<array_shape> parser::parse_array_shape() {
    if (!accept('(')) return error("expected array dimensions");
    auto shape = array_shape{};
    do {
        auto d = parse_one_dimension();
        if (!d) return error_of(std::move(d));
        shape.push_back(d.value());
    } while (accept(','));
    if (!(accept(')'))) return error("array dimensions missing ')'");
    return shape;
}

parser::expected<dimension> parser::parse_one_dimension() {
    auto limit1 = parse_integer_constant();
    if (!limit1) return error_of(std::move(limit1));
    if (accept('/')) {
        auto limit2 = parse_integer_constant();
        if (!limit2) return error_of(std::move(limit2));
        if (limit2.value() < limit1.value()) {
            return error("maximum dimension {} cannot be less than minimum {}",
                         limit2.value(), limit1.value());
        }
        return dimension{std::move(limit1).value(), std::move(limit2).value()};
    }
    if (limit1.value() < 1) {
        return error("maximum dimension {} is less than the default minimum 1",
                     limit1.value());
    }
    return dimension{1, std::move(limit1).value()};
}

parser::expected<io_list_t> parser::parse_io_list() {
    auto list = io_list_t{};
    do {
        auto item = parse_io_list_item();
        if (!item) return error_of(std::move(item));
        list.push_back(std::move(item).value());
    } while (accept(','));
    return list;
}

parser::expected<io_list_item> parser::parse_io_list_item() {
    if (accept('(')) {  // should be indexed array expression and index control
        auto const array = parse_identifier();
        if (array.empty()) return error("expected array name for i/o item");
        auto const symbol = m_subprogram.find_symbol(array);
        if (symbol.shape.empty()) {
            return error("expected array name, saw '{}'", array);
        }
        auto indices = parse_argument_list();
        if (!indices) return error_of(std::move(indices));
        if (indices.value().size() != symbol.shape.size()) {
            return error("expected {} subscripts for '{}'",
                         symbol.shape.size(), array);
        }
        if (!accept(',')) {
            return error("missing ',' separating the indexed expression from "
                         "the index control");
        }
        auto control = parse_index_control();
        if (!control) return error_of(std::move(control));
        if (!accept(')')) return error("missing ')' after index control");
        return io_list_item{array, std::move(indices).value(),
                            std::move(control).value()};
    }

    auto const name = parse_identifier();
    if (name.empty()) {
        return error("expected variable name in input-output list");
    }
    auto symbol = m_subprogram.find_symbol(name);
    infer_type_and_update(symbol);
    if (symbol.kind == symbolkind::subprogram ||
        symbol.kind == symbolkind::external
    ) {
        return error("cannot use a subprogram in an input-output list");
    }
    
    if (symbol.shape.empty()) return io_list_item{name};
    if (match('(')) {
        auto indices = parse_argument_list();
        if (!indices) return error_of(std::move(indices));
        return io_list_item{name, std::move(indices).value()};
    }

    // Implicitly apply to the entire array.
    auto const induction_name = symbol_name{"_I_"};
    auto induction_symbol = m_subprogram.find_symbol(induction_name);
    if (induction_symbol.type == datatype::unknown) {
        induction_symbol.type = datatype::INTEGER;
        m_subprogram.update_symbol(std::move(induction_symbol));
    }
    auto const induction = std::make_shared<variable_node>(induction_name);
    return io_list_item{
        name, argument_list_t{induction},
        {.index=induction_name,
         .init=std::make_shared<constant_node>(symbol.shape[0].minimum),
         .limit=std::make_shared<constant_node>(symbol.shape[0].maximum),
         .step=std::make_shared<constant_node>(1)}
    };
}

parser::expected<index_control_t> parser::parse_index_control() {
    auto const index = parse_identifier();
    if (index.empty()) return error("missing index variable");

    auto symbol = m_subprogram.find_symbol(index);
    if (symbol.type == datatype::unknown) {
        symbol.type = datatype::INTEGER;
    }
    if (symbol.type != datatype::INTEGER) {
        return error("index must be an integer variable");
    }
    if (symbol.kind == symbolkind::subprogram ||
        symbol.kind == symbolkind::external
    ) {
        return error("index cannot be a subprogram");
    }
    if (!symbol.shape.empty()) return error("index cannot be an array");
    m_subprogram.update_symbol(symbol);

    if (!accept('=')) return error("expected '=' in index control");
    auto init = parse_expression();
    if (!init) return error_of(std::move(init));
    if (!accept(',')) return error("expected ',' in index control");
    auto limit = parse_expression();
    if (!limit) return error_of(std::move(limit));
    auto step =
        accept(',') ? parse_expression() : std::make_shared<constant_node>(1);
    if (!step) return error_of(std::move(step));
    return index_control_t{index, std::move(init).value(),
                           std::move(limit).value(), std::move(step).value()};
}

parser::expected<data_list_t> parser::parse_data_list() {
    if (!accept('/')) return error("expected data list between slashes");
    auto data_list = data_list_t{};
    do {
        auto item = parse_constant();
        if (!item) return error_of(std::move(item));
        if (accept('*') && item.value().type == datatype::INTEGER) {
            auto const repeat_count = item.value().value;
            if (repeat_count < 1) {
                return error("data item repeat count {} is less than 1",
                             repeat_count);
            }
            item = parse_constant();
            if (!item) return error_of(std::move(item));
            data_list.emplace_back(item.value().value, item.value().type,
                                   repeat_count);
        } else {
            data_list.emplace_back(item.value().value, item.value().type);
        }
    } while (accept(','));
    if (!accept('/')) return error("data list missing final slash");
    return data_list;
}

parser::expected<field_list_t> parser::parse_field_list() {
    if (!accept('(')) return error("expected '(' in FORMAT statement");

    auto const head = position();
    auto parens = 1;  // because we just read the opening paren
    auto quoted = false;
    while (parens != 0) {
        if (at_eol()) return error("mismatched parentheses in FORMAT");
        switch (current()) {
            case '\0': return error("missing ')' in FORMAT statement");
            case '\'': quoted = !quoted; break;
            case '(':  if (!quoted) parens += 1; break;
            case ')':
                if (!quoted) {
                    parens -= 1;
                    if (parens == 0) {
                        auto const tail = position();
                        advance();  // eats the closing paren
                        if (!at_eol()) {
                            return error("unexpected token after FORMAT "
                                         "statement");
                        }
                        return std::string(head, tail);
                    }
                }
                break;
            default: break;
        }
        advance();
    }

    assert(false && "unreachable code reached?!");
    return error("parsing error in field specification list");
}

parser::expected<parameter_list_t> parser::parse_parameter_list() {
    if (!accept('(')) return error("expected a parameter list");
    auto parameters = parameter_list_t{};
    if (accept(')')) return parameters;  // no parameters
    do {
        auto const param = parse_identifier();
        if (param.empty()) return error("expected parameter in parameter list");
        parameters.push_back(param);
    } while (accept(','));
    if (!accept(')')) return error("expected ')' at end of parameter list");
    return parameters;
}

parser::expected<argument_list_t> parser::parse_argument_list() {
    if (!accept('(')) return error("expected an argument list");
    auto arguments = argument_list_t{};
    if (accept(')')) return arguments;  // no arguments
    do {
        auto arg = parse_argument();
        if (!arg) return error_of(std::move(arg));
        arguments.push_back(std::move(arg).value());
    } while (accept(','));
    if (!accept(')')) return error("expected ')' at end of argument list");
    return arguments;
}

parser::expected<expression_t> parser::parse_argument() {
    auto const bookmark = position();
    auto const name = parse_identifier();
    if (!name.empty() && (match(',') || match(')'))) {
        // It's just a variable or external, so it has an address we can use as
        // the argument expression.
        auto symbol = m_subprogram.find_symbol(name);
        if (symbol.type == datatype::unknown) {
            infer_type_and_update(symbol);
        }
        switch (symbol.kind) {
            case symbolkind::external:
                return std::make_shared<external_node>(symbol.name);
            case symbolkind::common:
            case symbolkind::local:
            case symbolkind::shadow:
            case symbolkind::argument:
            case symbolkind::retval:
                return std::make_shared<variable_node>(symbol.name);
        }
        return error("cannot use {} as an argument", symbol.name);
    }
    // Whoops, back up.  It's a more complex expression that will have to be
    // stashed in a temporary variable in order to pass by reference.
    m_it = bookmark;
    auto expr = parse_expression();
    if (!expr) return error_of(std::move(expr));
    return std::make_shared<temp_variable_node>(std::move(expr).value());
}

parser::expected<parser::keyword> parser::parse_keyword() {
    // Remember that spaces are removed from the statement, so there's no
    // separator between adjacent keywords or identifiers (e.g., "GOTO" and
    // "IMPLICITINTEGER"), so we recognize the first prefix that matches a
    // keyword.  Switching on the first character slashes the number of keywords
    // we have to try.
    if (at_eol()) return error("statement ended while expecting a keyword");
    switch (current()) {
        #define RECOG(KEYWORD) if (accept(#KEYWORD)) return keyword::##KEYWORD
        case 'A': RECOG(ACCEPT);                                break;
        case 'C': RECOG(CALL); RECOG(COMMON); RECOG(CONTINUE);  break;
        case 'D': RECOG(DATA); RECOG(DIMENSION); RECOG(DO);     break;
        case 'E': RECOG(END); RECOG(EXTERNAL);                  break;
        case 'F': RECOG(FORMAT); RECOG(FUNCTION);               break;
        case 'G': RECOG(GOTO);                                  break;
        case 'I': RECOG(IF); RECOG(IMPLICIT); RECOG(INTEGER);   break;
        case 'L': RECOG(LOGICAL);                               break;
        case 'O': RECOG(OPEN);                                  break;
        case 'P': RECOG(PAUSE); RECOG(PROGRAM);                 break;
        case 'R': RECOG(READ); RECOG(REAL); RECOG(RETURN);      break;
        case 'S': RECOG(STOP); RECOG(SUBROUTINE);               break;
        case 'T': RECOG(TYPE);                                  break;
        #undef RECOG
    }
    return error("expected a keyword at '{}'",
        std::string_view(position(), m_statement.end()));
}

parser::expected<variable_list_t> parser::parse_variable_list() {
    auto list = variable_list_t{};
    do {
        auto item = parse_variable_list_item();
        if (!item) return error_of(std::move(item));
        list.push_back(std::move(item).value());
    } while (accept(','));
    return list;
}

parser::expected<variable_list_item_t> parser::parse_variable_list_item() {
    if (accept('(')) {  // should be indexed array expression and index control
        auto const array = parse_identifier();
        if (array.empty()) return error("expected array name");
        auto const symbol = m_subprogram.find_symbol(array);
        if (symbol.shape.empty()) {
            return error("expected array name, saw '{}'", array);
        }
        auto indices = parse_index_list();
        if (!indices) return error_of(std::move(indices));
        if (indices.value().size() != symbol.shape.size()) {
            return error("expected {} subscripts for '{}'",
                         symbol.shape.size(), array);
        }
        if (!accept(',')) {
            return error("missing ',' separating the indexed expression from "
                         "the index control");
        }
        auto control = parse_constant_index_control();
        if (!control) return error_of(std::move(control));
        if (!accept(')')) return error("missing ')' after index control");
        return variable_list_item_t{array, std::move(indices).value(),
                                    std::move(control).value()};
    }

    auto const name = parse_identifier();
    if (name.empty()) {
        return error("expected variable name in variable list");
    }
    auto symbol = m_subprogram.find_symbol(name);
    infer_type_and_update(symbol);
    if (symbol.kind == symbolkind::subprogram ||
        symbol.kind == symbolkind::external
    ) {
        return error("cannot use a subprogram in a variable list");
    }
    
    if (symbol.shape.empty()) return variable_list_item_t{name};
    if (match('(')) {
        auto indices = parse_index_list();
        if (!indices) return error_of(std::move(indices));
        return variable_list_item_t{name, std::move(indices).value()};
    }

    // Implicitly apply to the entire array.
    // TODO:  Generalize for more than 1 dimension.
    auto const indices = index_list_t{
        index_t{
            .induction = symbol_name{"_I_"},
            .coefficient = 1,
            .offset = 0
        }
    };
    auto const index_control = constant_index_control_t{
        .index = symbol_name{"_I_"},
        .init  = symbol.shape[0].minimum,
        .limit = symbol.shape[0].maximum,
        .step  = 1
    };
    return variable_list_item_t{name, indices, index_control};
}

parser::expected<constant_index_control_t>
parser::parse_constant_index_control() {
    auto const induction = parse_identifier();
    if (induction.empty()) return error("missing induction variable");

    auto symbol = m_subprogram.find_symbol(induction);
    if (symbol.type == datatype::unknown) {
        symbol.type = datatype::INTEGER;
    }
    if (symbol.type != datatype::INTEGER) {
        return error("induction variable must be an integer");
    }
    if (symbol.kind == symbolkind::subprogram ||
        symbol.kind == symbolkind::external
    ) {
        return error("induction variable cannot be a subprogram");
    }
    if (!symbol.shape.empty()) return error("index cannot be an array");
    m_subprogram.update_symbol(symbol);

    if (!accept('=')) return error("expected '=' in index control");
    auto k0 = parse_constant();
    if (!k0) return error_of(std::move(k0));
    if (k0.value().type != datatype::INTEGER) {
        return error("initial value for induction must be an INTEGER constant");
    }
    auto const init = k0.value().value;
    if (!accept(',')) return error("expected ',' in index control");
    auto k1 = parse_constant();
    if (!k1) return error_of(std::move(k1));
    if (k1.value().type != datatype::INTEGER) {
        return error("final value for induction must be an INTEGER constant");
    }
    auto const limit = k1.value().value;
    auto step = machine_word_t{1};
    if (accept(',')) {
        auto k2 = parse_constant();
        if (!k2) return error_of(std::move(k2));
        if (k2.value().type != datatype::INTEGER) {
            return error("increment for induction must be an INTEGER constant");
        }
        step = k2.value().value;
    }
    return constant_index_control_t{induction, init, limit, step};
}

parser::expected<index_list_t> parser::parse_index_list() {
    auto list = index_list_t{};
    if (!accept('(')) return error("expected '(' for subscripting");
    do {
        auto index = parse_index();
        if (!index) return error_of(std::move(index));
        list.push_back(std::move(index).value());
    } while (accept(','));
    if (!accept(')')) return error("expected ')' after subscript");
    return list;
}

parser::expected<index_t> parser::parse_index() {
    // TODO:  Allow any permutation of:  coefficient * index +|- offset
    auto const induction = parse_identifier();
    if (induction.empty()) return error("expected induction variable");
    machine_word_t coefficient = 1;
    machine_word_t offset = 0;
    return index_t{
        .induction = induction,
        .coefficient = coefficient,
        .offset = offset
    };
}

parser::expected<subscript_list_t> parser::parse_subscript_list() {
    auto list = subscript_list_t{};
    if (!accept('(')) return error("expected '(' for subscripting");
    do {
        auto subscript = parse_expression();
        if (!subscript) return error_of(std::move(subscript));
        list.push_back(std::move(subscript).value());
    } while (accept(','));
    if (!accept(')')) return error("expected ')' after subscript");
    return list;
}

symbol_name parser::parse_identifier() {
    // Identifiers (i.e., variable and subroutine names) must start with a
    // letter and contain only letters and digits.  Only the first six
    // characters are significant, but we consume the whole identifier.
    if (!match_letter()) return {};
    auto const begin = position();
    advance();
    while (match_alphanumeric()) advance();
    auto const end = position();
    auto const view = std::string_view(begin, end);
    return symbol_name{view};
}

parser::expected<parser::openkey> parser::parse_open_keyword() {
    auto const begin = position();
    while (match_letter()) { advance(); }
    auto const token = std::string_view(begin, position());
    if (auto const key = look_up_open_keyword(token); key != openkey::unknown) {
        return key;
    }
    return error("expected parameter name in OPEN");
}

parser::expected<operator_t> parser::parse_operator() {
    // There are few operators, and they're short, so let's code the tree.
    // Numeric Operators
    if (accept('+')) return operator_t::add;
    if (accept('-')) return operator_t::subtract;
    if (accept('*')) return accept('*') ? operator_t::exponentiate
                                        : operator_t::multiply;
    if (accept('/')) return operator_t::divide;

    // Comparison and Logical Operators.
    // Note that Fortran IV defines .NOT., .AND., .OR., .XOR., and .EQV. as
    // logical operators, so we return logic_not, logic_and, etc.
    // However, the DEC compiler implements those as bitwise operations
    // performed on the numeric values, so we'll switch to bit_not, bit_and,
    // etc. in code generation.
    if (accept('.')) {
        switch (current()) {
        #define RECOG(TOKEN, OP) if (accept(TOKEN".")) return operator_t::##OP
            case 'A': RECOG("AND", logic_and);
                      break;
            case 'E': RECOG("EQ",  compare_eq);
                      RECOG("EQV", logic_eqv);
                      break;
            case 'G': RECOG("GE",  compare_gte);
                      RECOG("GT",  compare_gt);
                      break;
            case 'L': RECOG("LE",  compare_lte);
                      RECOG("LT",  compare_lt);
                      break;
            case 'N': RECOG("NE",  compare_ne);
                      RECOG("NOT", logic_not);
                      break;
            case 'O': RECOG("OR",  logic_or);
                      break;
            case 'X': RECOG("XOR", logic_xor);
                      break;
        #undef RECOG
        }
        return error("unrecognized operator");
    }
    return error("expected an operator");
}

parser::expected<constant_t> parser::parse_constant() {
    switch (current()) {
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        case '-': case '"':
            return parse_numeric_constant();
        case '\'': {
            auto k = parse_literal_constant();
            if (!k) return error_of(std::move(k));
            return constant_t{k.value(), datatype::LITERAL};
        }
        case '.': {
            auto k = parse_logical_constant();
            if (!k) return error_of(std::move(k));
            return constant_t{k.value(), datatype::LOGICAL};
        }
    }
    return error("expected a constant");
}

parser::expected<constant_t> parser::parse_numeric_constant() {
    if (accept('"')) {
        auto octal = parse_integer(8);
        if (!octal) return error_of(std::move(octal));
        return constant_t{octal.value(), datatype::INTEGER};
    }
    auto parsed = parse_integer(10);
    if (!parsed) return error_of(std::move(parsed));
    auto const whole = parsed.value();
    auto const sign = (whole < 0) ? -1.0f : 1.0f;
    auto const bookmark = position();
    if (accept('.') && match_digit()) {
        // This is pretty hokey parsing for real numbers, but CROW0000 uses only
        // basic REAL constants.
        auto numer = machine_word_t(consume() - '0');
        auto denom = 10.0f;
        while (match_digit() && denom < 10'000'000.0f) {
            numer = 10*numer + consume() - '0';
            denom *= 10.0f;
        }
        auto const real = sign*(sign*whole + numer/denom);
        return constant_t{real};
    }
    m_it = bookmark;
    return constant_t{whole, datatype::INTEGER};
}

parser::expected<machine_word_t> parser::parse_integer_constant() {
    auto const base = accept('"') ? 8 : 10;
    return parse_integer(base);
}

parser::expected<machine_word_t> parser::parse_literal_constant() {
    // Packs a short character string into a machine word.  Does not handle
    // Hollerith data.
    auto text = parse_literal();
    if (!text) return error_of(std::move(text));
    if (text.value().size() > 5) {
        return error("literal constant cannot exceed 5 characters");
    }
    return make_literal(std::move(text).value());
}

parser::expected<machine_word_t> parser::parse_logical_constant() {
    if (accept(".TRUE."))  return LOGICAL_TRUE;
    if (accept(".FALSE.")) return LOGICAL_FALSE;
    return error("expected .TRUE. or .FALSE.");
}

parser::expected<machine_word_t> parser::parse_integer(int base, int length) {
    auto const max_digits =
        length > 0 ? length
                   : base == 10 ? 11 : 12;
    auto digits = 0;
    auto k = machine_word_t{0};
    bool negative = false;
    if (accept('-')) negative = true;
    while (digits < max_digits && match_digit(base)) {
        k *= base;
        k += consume() - '0';
        digits += 1;
    }
    while (match_digit(base)) consume();
    if (digits == 0) return error("no digits in constant");
    return negative ? -k : k;
}

parser::expected<std::string> parser::parse_literal() {
    if (!accept('\'')) return error("expected a quoted character constant");
    auto text = std::string{};
    while (!at_eol()) {
        if (accept('\'') && !match('\'')) {
            // We just accepted the closing quotation mark.  (If it had been
            // "\'\'", it would have been an escaped apostrophe.)
            return text;
        }
        text.push_back(consume());
    }
    return error("unterminated string '{}...", text.substr(0, 8));
}

parser::expected<statement_number_t> parser::parse_statement_number() {
    if (!match_digit()) return error("expected statement number");
    auto number = statement_number_t{0};
    auto digits = 0;
    while (match_digit() && digits < 5) {
        number *= 10;
        number += consume() - '0';
    }
    return number;
}

parser::expected<bool> parser::add_branch_target(statement_number_t number) {
    assert(number != no_statement_number);
    assert(phase1 <= m_phase && m_phase <= phase4);
    auto symbol =
        m_subprogram.find_symbol(symbol_name{static_cast<unsigned>(number)});
    if (symbol.kind == symbolkind::format) {
        return error("statement {} cannot be a branch target and a FORMAT; "
                     "a FORMAT statement is not executable", number);
    }
    if (symbol.kind != symbolkind::label) {
        symbol.kind = symbolkind::label;
        symbol.type = datatype::none;
        m_subprogram.update_symbol(symbol);
    }
    return true;
}

parser::expected<bool> parser::begin_main_subprogram(symbol_name const &name) {
    assert(m_phase == phase0);
    assert(!m_program.has_main_subprogram());
    m_subprogram = unit{name};
    m_implicit.reset();
    m_common_counts.clear();
    m_arithmetic_functions.clear();
    m_current_subprogram_is_main = true;
    m_phase = phase1;
    return true;
}

parser::expected<bool> parser::end_main_subprogram() {
    assert(m_current_subprogram_is_main);
    assert(!m_program.has_main_subprogram());
    m_program.add_main_subprogram(std::move(m_subprogram));
    m_current_subprogram_is_main = false;
    m_phase = phase0;
    return true;
}

parser::expected<bool> parser::begin_subprogram(symbol_name const &name) {
    assert(m_phase == phase0);
    m_subprogram = unit{name};
    m_implicit.reset();
    m_common_counts.clear();
    m_arithmetic_functions.clear();
    m_phase = phase1;
    return true;
}

parser::expected<bool> parser::end_subprogram() {
    assert(!m_current_subprogram_is_main);
    m_program.add_subprogram(std::move(m_subprogram));
    m_phase = phase0;
    return true;
}

datatype parser::inferred_type(symbol_info const &symbol) const {
    return
        symbol.type == datatype::unknown ? m_implicit.type_for(symbol.name)
                                         : symbol.type;
}

void parser::infer_type_and_update(symbol_info &symbol) {
    if (symbol.type == datatype::unknown) {
        symbol.type = m_implicit.type_for(symbol.name);
        m_subprogram.update_symbol(symbol);
    }
}

void parser::infer_types() {
    auto unknowns = m_subprogram.extract_symbols(has_unknown_type);
    for (auto &symbol : unknowns) {
        symbol.type = m_implicit.type_for(symbol.name);
        m_subprogram.update_symbol(std::move(symbol));
    }
}

bool parser::accept(keyword kw) {
    auto const bookmark = position();
    if (parse_keyword().value_or(keyword::unknown) == kw) return true;
    // whoops, back it up
    m_it = bookmark;
    return false;
}

bool parser::accept(operator_t op) {
    auto const bookmark = position();
    if (parse_operator().value_or(operator_t::none) == op) return true;
    // whoops, back it up
    m_it = bookmark;
    return false;
}

bool parser::match_assignment_statement() const {
    // See the second half of this answer:
    // https://retrocomputing.stackexchange.com/a/31625/4022

    auto const statement_field = std::string_view(m_it, m_statement.end());

    // A logical IF statement whose then clause is an assignment statement
    // might look like an assignment, but we want it treated like a regular
    // statement.
    if (statement_field.starts_with("IF(")) return false;

    // Is there an equal sign that's not inside parentheses?
    // If so, are there no commas after it that aren't inside parentheses (or
    // quotes)?
    // We don't support Hollerith strings, which would have to be taken into
    // account for determining `quoted`.
    auto parens = 0;
    auto quoted = false;
    auto has_eq = false;
    for (auto const ch : statement_field) {
        switch (ch) {
            case '(':  parens += 1;                                 break;
            case ')':  parens -= 1;                                 break;
            case '\'': quoted = !quoted;                            break;
            case '=':  has_eq = has_eq || (parens == 0 && !quoted); break;
            case ',':
                if (has_eq && parens == 0 && !quoted) return false;
                break;
        }
    }
    return has_eq;
}

bool parser::next_statement() {
    m_statement.clear();
    while (categorize_line(m_line) == line_type::comment && next_line()) {}
    if (categorize_line(m_line) != line_type::initial) return false;
    m_statement = m_line;
    if (!next_line()) return true;
    for (;;) {
        switch (categorize_line(m_line)) {
            case line_type::comment:
                if (!next_line()) return true;
                continue;
            case line_type::initial:
                return true;
            case line_type::continuation:
                m_statement.append(m_line.substr(column_7));
                if (!next_line()) return true;
                continue;
        }
    }
}

bool parser::next_line() {
    m_line.clear();
    if (!m_in) return false;
    std::getline(m_in, m_line);
    normalize_line(m_line);
    return true;
}

parser::line_type constexpr parser::categorize_line(std::string_view line) {
    if (line.empty()) return line_type::comment;
    if (line[column_1] == 'C') return line_type::comment;
    if (line[column_1] == '*') return line_type::comment;
    if (line[column_6] == ' ') return line_type::initial;
    if (line[column_6] == '0') return line_type::initial;
    return line_type::continuation;
}

void parser::normalize_line(std::string &line) {
    trim_leading_form_feeds(line);
    if (line.empty()) return;
    // Don't do any further processing for comments.
    if (line[0] == 'C' || line[0] == '*') return;
    // Non-standard ways to indicate a comment line:
    if (line[0] == 'c' || line[0] == '!') return;

    auto p = line.cbegin();
    auto fixed = std::string{};
    fixed.reserve(line.size() + 6);  // because leading tabs usually expand

    // Typical fixed-format uses blanks (spaces) to get from the statement
    // number field to the continuation flag or statement field.
    // The DEC Fortran IV compiler allowed a tab ('\t') to jump from the number
    // field to the statement field (column 7) or to the continuation flag in
    // column 6:
    //    "If a continuation line is desired when a TAB is used in the statement
    //     number field, a digit from 1 to 9 must immediately follow the TAB."
    while (p != line.cend() && fixed.size() < column_7) {
        if (*p == '\t') {
            // Tab advances us at least to column 6.
            fixed.append(column_6 - fixed.size(), ' '); ++p;
            if (p != line.cend() && '1' <= *p && *p <= '9') {
                fixed.push_back(*p++);
            } else {
                fixed.push_back(' ');
            }
            break;
        }
        fixed.push_back(*p++);
    }

    // Make sure all the characters are in the official character set.  If any
    // aren't, replace them with '^'.
    for ( ; p != line.cend(); ++p) {
        if (*p == '\t')                  fixed.push_back(' ');
        else if ('a' <= *p && *p <= 'z') fixed.push_back(*p - 'a' + 'A');
        else if ((*p < ' ' || 'Z' < *p)) fixed.push_back('^');
        else                             fixed.push_back(*p);
    }

    // If line contained only whitespace, we just normalized it to six spaces,
    // but it would be best if it were empty.
    if (fixed == "      ") fixed.clear();

    line.swap(fixed);
}

void parser::trim_leading_form_feeds(std::string &line) {
    if (line.empty()) return;
    auto const i = line.find_first_not_of('\f');
    if (i == 0) return;
    line = line.substr(i);
}

void parser::crush_statement(std::string &s) {
    if (s.size() < column_7) return;
    // Fortran statements don't need any spaces except those in quoted text or
    // Hollerith data (which we don't support).
    auto quoted = false;
    auto target = s.begin() + column_7;
    for (auto source = s.cbegin() + column_7; source != s.cend(); ++source) {
        switch (*source) {
            case ' ':  if (!quoted) continue;   break;
            case '\'': quoted = !quoted;        break;
        }
        *target++ = *source;
    }
    s.resize(std::distance(s.begin(), target));
}

parser::openkey parser::look_up_open_keyword(std::string_view token) {
    static auto const map = std::map<std::string_view, openkey>{
        {"UNIT",        openkey::UNIT       },
        {"ACCESS",      openkey::ACCESS     },
        {"FILE",        openkey::FILE       },
        {"NAME",        openkey::NAME       }
    };

    auto const it = map.find(token);
    return (it != map.end()) ? it->second : openkey::unknown;
}

}
