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
#include <map>
#include <print>
#include <string_view>

namespace aid::fortran {

parser::expected<program> parser::parse_files(
    std::span<std::filesystem::path> const &paths
) {
    if (paths.empty()) return error("no source code provided");
    // Concatenate the files into a single stream.
    std::string buffer;
    for (auto const &path : paths) {
        auto in = std::ifstream(path);
        if (!in) return error("count not find or open \"{}\"", path.string());
        buffer.append(std::istreambuf_iterator<char>{in},
                      std::istreambuf_iterator<char>{});
    }
    auto stream = std::stringstream{std::move(buffer)};
    auto const parsed = parse_stream(stream);
    if (!parsed.has_value()) return error(parsed.error());
    auto program = parsed.value();

    // If the program wasn't given a name, use the first source file's name.
    if (program.unit_name().empty()) {
        auto const &first = paths.front();
        if (first.has_stem()) {
            auto const stem = to_upper_ascii(first.stem().string());
            auto const name = symbol_name{stem};
            program.set_unit_name(name);
        }
    }
    return program;
}

parser::expected<program> parser::parse_stream(std::istream &in) {
    auto p = parser{in};
    return p.parse_statements();
}

parser::expected<program> parser::parse_statements() {
    while (next_statement()) {
        //std::print("{}\n", m_statement);
        auto const stmt = parse_full_statement();
        if (!stmt.has_value()) {
            return error("{}\n{}\n", stmt.error().message(), m_statement);
        }
        if (stmt.value() == nullptr) continue;
        m_current_unit->add_statement(stmt.value());
    }
    // We do not call end_program here.  That was done when the END statement of
    // the program was reached.  And there were probably subprograms after that.
    return m_program;
}

parser::expected<statement_t> parser::parse_full_statement() {
    auto const number = parse_statement_number_field();
    if (!number.has_value()) return error(number.error());
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
        auto const keyword = parse_keyword();
        if (!keyword.has_value()) return error(keyword.error());
        kw = keyword.value();
    }
    assert(kw != keyword::unknown);
    auto const statement = parse_identified_statement(kw, number);
    if (!statement.has_value()) return error(statement.error());
    auto const stmt = statement.value();
    if (stmt != nullptr) stmt->set_statement_number(number);
    return stmt;
}

parser::expected<statement_t>
parser::parse_identified_statement(keyword kw, statement_number_t number) {
    auto const original_phase = m_phase;

    // In phase 0, we're looking to start the program or subprogram.
    if (m_phase == phase0) switch (kw) {
        case keyword::PROGRAM:      return parse_program();
        case keyword::INTEGER:      return parse_function(datatype::INTEGER);
        case keyword::LOGICAL:      return parse_function(datatype::LOGICAL);
        case keyword::FUNCTION:     return parse_function();
        case keyword::SUBROUTINE:   return parse_subroutine();
        default:
            // Adventure dives right in without a PROGRAM statement.
            begin_program();  // advances the phase
            break;
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
            m_current_unit->infer_types();
            m_phase = phase3;
            break;
    }

    // DATA statements can happen after the specifications.
    if (phase3 <= m_phase && m_phase <= phase4) switch (kw) {
        case keyword::DATA:         return parse_data();
    }

    // Phase 3 is for statement function definitions, which look like
    // assignments.  Note that if we're here and we get an actual, executable
    // assignment statement, the phase will be advanced.
    if (m_phase == phase3) switch (kw) {
        case keyword::assignment:   return parse_assignment();

        case keyword::INTEGER:
            warn("INTEGER statement in Phase 3. This statement should come "
                 "before any DATA statement or statement function "
                 "definition.");
            return parse_type_specification(datatype::INTEGER);
        case keyword::LOGICAL:
            warn("LOGICAL statement in Phase 3. This statement should come "
                 "before any DATA statement or statement function "
                 "definition.");
            return parse_type_specification(datatype::LOGICAL);

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

    // Phase 5 is like Phase 0, except you cannot have another PROGRAM.  (You
    // enter Phase 5 when the previous unit reaches its END statement.)
    if (m_phase == phase5) {
        switch (kw) {
            case keyword::INTEGER:
                if (!accept(keyword::FUNCTION)) break;
                return parse_function(datatype::INTEGER);
            case keyword::LOGICAL:
                if (!accept(keyword::FUNCTION)) break;
                return parse_function(datatype::LOGICAL);
            case keyword::FUNCTION:  return parse_function();
            case keyword::SUBROUTINE:return parse_subroutine();
        }
        return error("expected FUNCTION or SUBROUTINE");
    }

    m_phase = original_phase;
    return error("unhandled keyword--wrong phase?");
}

parser::expected<statement_t> parser::parse_program() {
    // Adventure doesn't begin with a PROGRAM statement.  In fact, PROGRAM
    // statements may have been a later addition to Fortran.  Nevertheless,
    // we handle it for parser symmetry.
    auto const name = parse_identifier();
    if (name.empty()) return error("PROGRAM requires a name");
    if (!at_eol()) return error("unexpected token after PROGRAM statement");
    begin_program(name);
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_function(datatype type) {
    auto const name = parse_identifier();
    if (name.empty()) return error("FUNCTION requires a name");
    auto const params = parse_parameter_list();
    if (!params.has_value()) return error(params.error());
    if (params.value().empty()) {
        return error("FUNCTION {} must have at least one parameter", name);
    }
    if (!at_eol()) return error("unexpected token after FUNCTION statement");

    if (!begin_subprogram(name)) return error("problem starting a FUNCTION");

    // The function name is used for the return value.
    auto retval = m_current_unit->find_symbol(name);
    retval.kind = symbolkind::retval;
    retval.type = type;
    m_current_unit->update_symbol(retval);

    // We also add its parameters, but we don't know their types yet.
    auto index = 1u;
    for (auto const &param : params.value()) {
        auto symbol = m_current_unit->find_symbol(param);
        symbol.kind = symbolkind::argument;
        symbol.index = index++;
        m_current_unit->update_symbol(symbol);
    }

    // Make sure the function is described correctly in the main program's
    // symbol table.
    auto func = m_program.find_symbol(name);
    if (func.type != datatype::unknown && func.type != type) {
        return error("return type of function {} does not match that of the "
                     "main program", name);
    }
    func.type = type;
    func.kind = symbolkind::subprogram;
    func.index = static_cast<unsigned>(params.value().size());
    m_program.update_symbol(func);
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_subroutine() {
    auto const name = parse_identifier();
    if (name.empty()) return error("SUBROUTINE requires a name");

    // Unlike a FUNCTION, a SUBROUTINE does not require any parameters nor even
    // empty parentheses.
    auto params = parameter_list_t{};
    if (match('(')) {
        // But if it does have an opening parenthesis, it better be balanced.
        auto const param_list = parse_parameter_list();
        if (!param_list.has_value()) return error(param_list.error());
        params = param_list.value();
    }

    if (!at_eol()) return error("unexpected token after SUBROUTINE statement");
    if (!begin_subprogram(name)) return error("problem starting a SUBROUTINE");

    // Add the subroutine's parameters.  We don't know their types yet.
    auto index = 1u;
    for (auto const &param : params) {
        auto symbol = m_current_unit->find_symbol(param);
        symbol.kind = symbolkind::argument,
        symbol.index = index++;
        m_current_unit->update_symbol(symbol);
    }

    // The subroutine should already be in the program's symbol table, but just
    // in case...
    if (m_program.has_symbol(name)) {
        auto const symbol = m_program.find_symbol(name);
        if (symbol.kind != symbolkind::subprogram ||
            symbol.type != datatype::none
        ) {
            return error("SUBROUTINE {} has a conflicting return type",  name);
        }
    } else {
        m_program.update_symbol(symbol_info{
            .name = name,
            .kind = symbolkind::subprogram,
            .index = static_cast<unsigned>(params.size()),
            .type = datatype::none
        });
    }
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_end() {
    if (!at_eol()) return error("unexpected token after END statement");
    if (m_current_unit == &m_subprogram) {
        end_subprogram();
    } else if (m_current_unit == &m_program) {
        end_program();
    } else {
        return error("END statement while outside translation unit");
    }
    // If we return an actual statement, the top level code will attempt to
    // add it to the unit after the unit has been closed.
    return nullptr;
}

parser::expected<statement_t>
parser::parse_statement_function_definition(symbol_name const &name) {
    auto const parameters = parse_parameter_list();
    if (!parameters.has_value()) return error(parameters.error());
    if (parameters.value().empty()) {
        return error("statement function {} requires at least one parameter",
                     name);
    }
    if (!accept('=')) {
        return error("expected '=' in statement function definition");
    }
    auto const definition = parse_expression();
    if (!definition.has_value()) return error(definition.error());
    if (!at_eol()) {
        return error("unexpected token after statement function definition");
    }
    auto symbol = m_current_unit->find_symbol(name);
    symbol.kind = symbolkind::subprogram;
    if (symbol.type == datatype::unknown) {
        symbol.type = m_current_unit->implicit_type(name);
    }
    symbol.index = static_cast<unsigned>(parameters.value().size());
    m_current_unit->update_symbol(symbol);

    // Can we get away with making these C preprocessor macros?
    return make<definition_statement>(
        name, parameters.value(), definition.value());
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

        auto index = m_current_unit->comdat_count(block);
        do {
            auto name = parse_identifier();
            if (name.empty()) continue;
            auto symbol = m_current_unit->find_symbol(name);
            symbol.kind = symbolkind::common;
            symbol.comdat = block;
            symbol.index = ++index;
            if (symbol.type == datatype::unknown) {
                symbol.type = m_current_unit->implicit_type(name);
            }

            // Although it's not used by Adventure, a variable in a COMMON
            // specification may include array dimensions instead of requiring
            // a separate DIMENSION specification.
            auto const shape = match('(') ? parse_array_shape() : symbol.shape;
            if (symbol.shape != shape) {
                if (!symbol.shape.empty()) {
                    return
                        error("in COMMON /{}/, dimensions for {} are not the "
                              "same as previously specified", block, name);
                }
                symbol.shape = shape;
            }

            m_current_unit->update_symbol(symbol);
        } while (accept(','));
        m_current_unit->set_comdat_count(block, index);
    } while (!at_eol());

    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_data() {
    do {
        auto const variable_list = parse_variable_list();
        if (!variable_list.has_value()) return error(variable_list.error());
        auto const data_list = parse_data_list();
        if (!data_list.has_value()) return error(data_list.error());

        // Now match up the variables with the data values.
        auto data_iter = data_list.value().begin();
        auto const data_end = data_list.value().end();
        for (auto const &item : variable_list.value()) {
            auto symbol = m_current_unit->find_symbol(item.variable);
            if (symbol.type == datatype::unknown) {
                symbol.type = m_current_unit->implicit_type(item.variable);
            }
            if (symbol.shape.empty()) {     // scalar
                assert(item.subscripts.empty());
                if (data_iter == data_end) {
                    return error("more variables than values in DATA "
                                 "statement");
                }
                symbol.init_data.push_back((*data_iter++).value);
            } else {                        // array
                // TODO:  Generalize for more than 1 dimension.
                assert(item.subscripts.size() == 1);
                auto const &control = item.index_control;
                if (control.step == 0) {
                    return error("induction step cannot be 0");
                }
                if ((control.init < control.final && control.step < 0) ||
                    (control.init > control.final && control.step > 0)
                ) {
                    return error("induction step is in the wrong direction");
                }
                auto const lower = std::min(control.init, control.final);
                auto const upper = std::max(control.init, control.final);
                auto const &subscript = item.subscripts[0];
                auto const &shape = symbol.shape;
                auto const count = array_size(shape);
                auto &init_data = symbol.init_data;
                init_data.reserve(count);
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
            m_current_unit->update_symbol(symbol);
        }
        if (data_iter != data_end) {
            return error("more values than variables in DATA statement");
        }
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after DATA statement");
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_dimension() {
    do {
        auto const variable = parse_identifier();
        if (variable.empty()) return error("missing variable in DIMENSION");
        auto const shape = parse_array_shape();
        if (shape.empty()) return error("missing dimensions in DIMENSION");
        auto symbol = m_current_unit->find_symbol(variable);
        if (symbol.shape == shape) continue;
        if (!symbol.shape.empty()) {
            return error("attempted to re-dimension {}", variable);
        }
        symbol.shape = shape;
        m_current_unit->update_symbol(std::move(symbol));
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after DIMENSION statement");
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_external() {
    do {
        auto const name = parse_identifier();
        if (name.empty()) continue;
        auto symbol = m_current_unit->find_symbol(name);
        symbol.kind = symbolkind::external;
        symbol.type = datatype::none;
        m_current_unit->update_symbol(symbol);
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after EXTERNAL statement");
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_format(statement_number_t number) {
    if (number == no_statement_number) {
        return error("FORMAT requires a statement number");
    }

    auto const fields = parse_field_list();
    if (!fields.has_value()) return error(fields.error());

    m_current_unit->add_format(number, fields.value());
    if (!at_eol()) return error("unexpected token after FORMAT statement");
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
        auto const prefixes = parse_implicit_prefixes();
        if (!prefixes.has_value()) return error(prefixes.error());
        for (auto const ch : prefixes.value()) {
            m_current_unit->set_implicit_type(ch, type);
        }
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after IMPLICIT statement");
    return make<nonexecutable_statement>();
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
    do {
        auto const variable = parse_identifier();
        if (variable.empty()) {
            return error("expected variable name in type specification");
        }
        auto symbol = m_current_unit->find_symbol(variable);
        if (symbol.type == type) continue;
        if (symbol.type != datatype::unknown) {
            return error("previous type of {} conflicts with this type "
                         "specification", variable);
        }
        symbol.type = type;
        m_current_unit->update_symbol(std::move(symbol));
    } while (accept(','));
    if (!at_eol()) return error("unexpected token after type specification");
    return make<nonexecutable_statement>();
}

parser::expected<statement_t> parser::parse_assignment() {
    auto const name = parse_identifier();
    if (name.empty()) return error("expected lvalue");
    auto symbol = m_current_unit->find_symbol(name);

    if (match('(') && symbol.shape.empty()) {
        // Looks like a statement function definition.
        if (m_phase > phase3) {
            return error(
                "statement function {} must be defined before the first "
                "executable statement\n", name);
        }
        return parse_statement_function_definition(name);
    }

    // A regular assignment is an executable statement, so bump the phase if
    // necessary.
    if (m_phase <= phase3) m_phase = phase4;

    auto lvalue = expression_t{nullptr};
    if (match('(')) {
        if (symbol.kind == symbolkind::subprogram) {
            return error("did you mean to CALL {}?", name);
        }
        auto const indices = parse_argument_list();
        if (!indices.has_value()) return error(indices.error());
        if (indices.value().size() != symbol.shape.size()) {
            return error("wrong number of indices for {}", name);
        }
        lvalue =
            std::make_shared<array_index_node>(name, symbol.shape,
                                               indices.value());
    } else {
        lvalue = std::make_shared<variable_node>(name);
    }

    if (!accept('=')) {
        assert(false);
        return error("statement was misidentified as an assignment?!");
    }

    if (symbol.type == datatype::unknown) {
        symbol.type = m_current_unit->implicit_type(name);
        m_current_unit->update_symbol(symbol);
    }

    auto const rhs = parse_expression();
    if (!rhs.has_value()) return error(rhs.error());

    if (!at_eol()) return error("unexpected token after assignment");

    return make<assignment_statement>(lvalue, rhs.value());
}

parser::expected<statement_t> parser::parse_accept() {
    auto const unit = std::make_shared<constant_node>(0);
    auto const f = parse_statement_number();
    if (!f.has_value()) return error(f.error());
    auto const format = f.value();

    auto io_list = io_list_t{};
    if (accept(',')) {
        auto const list = parse_io_list();
        if (!list.has_value()) return error(list.error());
        io_list = list.value();
    }
    // Re-using read_statement with a special unit number.
    return make<read_statement>(unit, format, io_list);
}

parser::expected<statement_t> parser::parse_call() {
    auto const name = parse_identifier();
    if (name.empty()) return error("CALL requires a subprogram");
    auto const arguments =
        match('(') ? parse_argument_list() : argument_list_t{};
    if (!arguments.has_value()) return error(arguments.error());
    if (!at_eol()) return error("unexpected token after CALL statement");

    auto symbol = m_current_unit->find_symbol(name);
    if (!m_current_unit->has_symbol(name)) {
        // We infer that it's a subroutine.
        symbol.type = datatype::none;
        symbol.kind = symbolkind::subprogram;
        symbol.index = static_cast<unsigned>(arguments.value().size());
        m_current_unit->update_symbol(symbol);
    }

    switch (symbol.kind) {
        case symbolkind::subprogram:
            if (arguments.value().size() != symbol.index) {
                return error("wrong number of arguments in CALL of {}", name);
            }
            return make<call_statement>(name, arguments.value());
        case symbolkind::argument:
            return make<indirect_call_statement>(name, arguments.value());
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
    auto const ends_at = parse_statement_number();
    if (!ends_at.has_value()) return error(ends_at.error());
    auto const control = parse_index_control();
    if (!control.has_value()) return error(control.error());
    if (!at_eol()) return error("unexpected token after DO statement");

    auto const loop = std::make_shared<do_statement>(control.value());
    while (next_statement()) {
        //std::print("{}\n", m_statement);  // UGH!
        auto const stmt = parse_full_statement();
        if (!stmt.has_value()) return error(stmt.error());
        if (stmt.value() != nullptr) loop->add(stmt.value());
        if (m_statement_number == ends_at.value()) return loop;
    }
    return error("expected statement number {} at end of loop body",
                 ends_at.value());
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
    auto const target = parse_statement_number();
    if (!target.has_value()) return error(target.error());
    if (!at_eol()) return error("unexpected tokens after unconditional GOTO");
    add_label(target.value());
    return make<goto_statement>(target.value());
}

parser::expected<statement_t> parser::parse_computed_goto() {
    if (!accept('(')) return error("bug parsing computed GOTO statement");
    auto targets = std::vector<statement_number_t>{};
    do {
        auto const target = parse_statement_number();
        if (!target.has_value()) return error(target.error());
        targets.push_back(target.value());
        add_label(target.value());
    } while (accept(','));
    if (!accept(')')) return error("missing ')' in computed GOTO");
    accept(',');  // Adventure (sometimes?) omits this comma.
    auto const expr = parse_expression();
    if (!expr.has_value()) return error(expr.error());
    if (!at_eol()) return error("syntax error in computed GOTO");
    return make<computed_goto_statement>(targets, expr.value());
}

parser::expected<statement_t> parser::parse_if() {
    if (!accept('(')) return error("expected parenthesized condition in IF");
    auto const condition = parse_expression();
    if (!condition.has_value()) return error(condition.error());
    if (!accept(')')) return error("missing ')' after IF condition");
    if (match_digit()) {
        auto const nega = parse_statement_number();
        if (!nega.has_value()) return error(nega.error());
        if (!accept(',')) return error("expected ',' in numeric IF statement");
        auto const zero = parse_statement_number();
        if (!zero.has_value()) return error(zero.error());
        if (!accept(',')) return error("expected ',' in numeric IF statement");
        auto const posi = parse_statement_number();
        if (!posi.has_value()) return error(posi.error());
        if (!at_eol()) return error("unexpected token after numeric IF");
        add_label(nega.value());
        add_label(zero.value());
        add_label(posi.value());
        return make<numeric_if_statement>(
            condition.value(), nega.value(), zero.value(), posi.value());
    }
    auto const then = parse_statement(no_statement_number);
    if (!then.has_value()) return error(then.error());
    if (then.value() == nullptr) {
        return error("missing then clause of logical IF statement");
    }
    // Technically, there are a handful of statement types you cannot use as the
    // then-clause of an IF statement: IF, DO, non-executable statements, etc.
    // But that's not worth checking now.
    return make<if_statement>(condition.value(), then.value());
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
    auto const unit = parse_expression();
    if (!unit.has_value()) return error(unit.error());
    auto access = std::string{"SEQUENTIAL"};
    auto file_name = std::filesystem::path{};
    while (accept(',')) {
        auto const key = parse_open_keyword();
        if (!key.has_value() || !accept('=')) {
            return error("parameters after UNIT must be named");
        }
        switch (key.value()) {
            case openkey::ACCESS: {
                auto const param = parse_literal();
                if (!param.has_value()) return error(param.error());
                access = param.value();
                break;
            }
            case openkey::FILE: {
                auto const param = parse_literal();
                if (!param.has_value()) return error(param.error());
                file_name = std::filesystem::path{param.value()};
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
    return make<open_statement>(unit.value(), file_name);
}

parser::expected<statement_t> parser::parse_pause() {
    auto message = std::string("TYPE G TO CONTINUE OR X TO EXIT");
    if (match('\'')) {
        auto const text = parse_literal();
        if (!text.has_value()) return error(text.error());
        message = text.value();
    } else if (match_digit(8)) {
        auto const number = parse_integer(8, 6);
        if (!number.has_value()) return error(number.error());
        message = std::format("{:o}", number.value());
    }
    if (!at_eol()) return error("unexpected token after PAUSE statement");
    return make<pause_statement>(message);
}

parser::expected<statement_t> parser::parse_read() {
    auto unit = expression_t{};
    auto format = statement_number_t{};
    if (accept('(')) {
        auto const u = parse_expression();
        if (!u.has_value()) return error(u.error());
        unit = u.value();
        if (accept(',')) {
            auto const f = parse_statement_number();
            if (!f.has_value()) return error(f.error());
            format = f.value();
        }
        if (!accept(')')) return error("expected ')' in READ statement");
    } else {
        auto const f = parse_statement_number();
        if (!f.has_value()) return error(f.error());
        format = f.value();
    }

    auto const list = parse_io_list();
    if (!list.has_value()) return error(list.error());
    auto const io_list = list.value();
    if (!at_eol()) return error("unexpected token after READ statement");

    return make<read_statement>(unit, format, io_list);
}

parser::expected<statement_t> parser::parse_return() {
    if (!at_eol()) return error("extra tokens after RETURN");
    auto const retvals =
        m_current_unit->extract_symbols(
            [] (auto const &info) {
                return info.kind == symbolkind::retval;
            });

    return retvals.empty() ? make<return_statement>()
                           : make<return_statement>(retvals.front().name);
}

parser::expected<statement_t> parser::parse_stop() {
    if (!at_eol()) return error("extra tokens after STOP");
    return make<stop_statement>();
}

parser::expected<statement_t> parser::parse_type() {
    auto const format = parse_statement_number();
    if (!format.has_value()) return error(format.error());
    auto io_list = io_list_t{};
    if (accept(',')) {
        auto const list = parse_io_list();
        if (!list.has_value()) return error(list.error());
        io_list = list.value();
    }
    if (!at_eol()) return error("unexpected token after TYPE statement");
    return make<type_statement>(format.value(), io_list);
}

parser::expected<expression_t> parser::parse_expression() {
    auto alternative = parse_alternative_expression();
    if (!alternative.has_value()) return error(alternative.error());
    auto lhs = alternative.value();
    for (;;) {
        auto const op = accept(operator_t::logic_xor) ? operator_t::logic_xor :
                        accept(operator_t::logic_eqv) ? operator_t::logic_eqv
                                                      : operator_t::none;
        if (op == operator_t::none) break;
        auto const rhs = parse_alternative_expression();
        if (!rhs.has_value()) return error(rhs.error());
        lhs = std::make_shared<binary_node>(lhs, op, rhs.value());
    }
    return lhs;
}

parser::expected<expression_t> parser::parse_alternative_expression() {
    auto const compound = parse_compound_expression();
    if (!compound.has_value()) return error(compound.error());
    auto lhs = compound.value();
    while (accept(operator_t::logic_or)) {
        auto const rhs = parse_compound_expression();
        if (!rhs.has_value()) return error(rhs.error());
        lhs = std::make_shared<binary_node>(lhs, operator_t::logic_or, rhs.value());
    }
    return lhs;
}

parser::expected<expression_t> parser::parse_compound_expression() {
    auto const comparison = parse_comparison();
    if (!comparison.has_value()) return error(comparison.error());
    auto lhs = comparison.value();
    while (accept(operator_t::logic_and)) {
        auto const rhs = parse_comparison();
        if (!rhs.has_value()) return error(rhs.error());
        lhs = std::make_shared<binary_node>(lhs, operator_t::logic_and, rhs.value());
    }
    return lhs;
}

parser::expected<expression_t> parser::parse_comparison() {
    auto const lhs = parse_arithmetic_expression();
    if (!lhs.has_value()) return error(lhs.error());
    if (!match('.')) return lhs;
    auto const bookmark = position();
    auto const op = parse_operator().value_or(operator_t::none);
    if (op != operator_t::compare_eq && op != operator_t::compare_ne  &&
        op != operator_t::compare_gt && op != operator_t::compare_gte &&
        op != operator_t::compare_lt && op != operator_t::compare_lte
    ) {
        m_it = bookmark;
        return lhs;
    }
    auto const rhs = parse_arithmetic_expression();
    if (!rhs.has_value()) return error(rhs.error());
    return std::make_shared<binary_node>(lhs.value(), op, rhs.value());
}

parser::expected<expression_t> parser::parse_arithmetic_expression() {
    auto const term = parse_term();
    if (!term.has_value()) return error(term.error());
    auto lhs = term.value();
    while (match('+') || match('-')) {
        auto const op = parse_operator().value_or(operator_t::none);
        auto const rhs = parse_term();
        if (!rhs.has_value()) return error(rhs.error());
        lhs = std::make_shared<binary_node>(lhs, op, rhs.value());
    }
    return lhs;
}

parser::expected<expression_t> parser::parse_term() {
    auto const factor = parse_factor();
    if (!factor.has_value()) return error(factor.error());
    auto lhs = factor.value();
    while (match('*') || match('/')) {
        auto const op = parse_operator().value_or(operator_t::none);
        if (op == operator_t::exponentiate) {
            return error("exponentiation is not supported");
        }
        auto const rhs = parse_factor();
        if (!rhs.has_value()) return error(rhs.error());
        lhs = std::make_shared<binary_node>(lhs, op, rhs.value());
    }
    return lhs;
}

parser::expected<expression_t> parser::parse_factor() {
    if (accept(operator_t::logic_not)) {
        auto const factor = parse_factor();
        if (!factor.has_value()) return error(factor.error());
        return std::make_shared<unary_node>(operator_t::logic_not, factor.value());
    }
    if (accept('-')) {
        auto const factor = parse_factor();
        if (!factor.has_value()) return error(factor.error());
        return std::make_shared<unary_node>(operator_t::negate, factor.value());
    }
    while (accept('+')) { /* unnecessary unary '+' */ }
    return parse_atom();
}

parser::expected<expression_t> parser::parse_atom() {
    if (accept('(')) {
        auto const expr = parse_expression();
        if (!expr.has_value()) return error(expr.error());
        if (!accept(')')) return error("expected ')' in expression");
        return expr.value();
    }
    if (match_letter()) {
        auto const name = parse_identifier();
        if (name.empty()) return error("expected identifier in expression");
        auto symbol = m_current_unit->find_symbol(name);
        if (!match('(')) {
            if (symbol.kind == symbolkind::external) {
                return std::make_shared<external_node>(name);
            }
            if (symbol.type == datatype::unknown) {
                symbol.type = m_current_unit->implicit_type(name);
                m_current_unit->update_symbol(symbol);
            }
            return std::make_shared<variable_node>(name);
        }

        auto const arguments = parse_argument_list();
        if (!arguments.has_value()) return error(arguments.error());
        auto const args = arguments.value();
        if (!symbol.shape.empty()) {
            if (args.size() != symbol.shape.size()) {
                return error("array has {} dimension(s) but only {} index(es) "
                             "were provided", symbol.shape.size(), args.size());
            }
            return std::make_shared<array_index_node>(name, symbol.shape, args);
        }
        if (symbol.kind == symbolkind::local) {
            // Either the program has an error, or this is actually a subprogram
            // (specifically a FUNCTION) that hasn't yet been defined.
            symbol.kind = symbolkind::subprogram;
            if (symbol.type == datatype::unknown) {
                // Yeah, this is almost certainly a FUNCTION.
                symbol.type = m_current_unit->implicit_type(name);
            }
            assert(symbol.index == 0);
            symbol.index = static_cast<unsigned>(args.size());
            m_current_unit->update_symbol(symbol);
        }
        if (symbol.kind == symbolkind::subprogram) {
            if (symbol.type == datatype::none) {
                return error("cannot invoke SUBROUTINE {} in an expression",
                             symbol.name);
            }
            return std::make_shared<function_invocation_node>(name, args);
        }
        return error("syntax error in expression near '{}'", name);
    }
    auto const constant = parse_constant();
    if (!constant.has_value()) return error(constant.error());
    return std::make_shared<constant_node>(constant.value().value);
}

array_shape parser::parse_array_shape() {
    if (!accept('(')) return {};
    auto shape = array_shape{};
    do {
        auto d = parse_one_dimension();
        if (!d.has_value()) return {};
        shape.push_back(d.value());
    } while (accept(','));
    if (!(accept(')'))) shape.clear();
    return shape;
}

parser::expected<dimension> parser::parse_one_dimension() {
    auto const limit1 = parse_integer_constant();
    if (!limit1.has_value()) return error(limit1.error());
    if (accept('/')) {
        auto const limit2 = parse_integer_constant();
        if (!limit2.has_value()) return error(limit2.error());
        if (limit2.value() < limit1.value()) {
            return error("maximum dimension {} cannot be less than minimum {}",
                         limit2.value(), limit1.value());
        }
        return dimension{limit1.value(), limit2.value()};
    }
    if (limit1.value() < 1) {
        return error("maximum dimension {} is less than the default minimum 1",
                     limit1.value());
    }
    return dimension{1, limit1.value()};
}

parser::expected<io_list_t> parser::parse_io_list() {
    auto list = io_list_t{};
    do {
        auto const item = parse_io_list_item();
        if (!item.has_value()) return error(item.error());
        list.push_back(item.value());
    } while (accept(','));
    return list;
}

parser::expected<io_list_item> parser::parse_io_list_item() {
    if (accept('(')) {  // should be indexed array expression and index control
        auto const array = parse_identifier();
        if (array.empty()) return error("expected array name for i/o item");
        auto const symbol = m_current_unit->find_symbol(array);
        if (symbol.shape.empty()) {
            return error("expected array name, saw '{}'", array);
        }
        auto const subscripts = parse_argument_list();
        if (!subscripts.has_value()) return error(subscripts.error());
        if (subscripts.value().size() != symbol.shape.size()) {
            return error("expected {} subscripts for '{}'",
                         symbol.shape.size(), array);
        }
        if (!accept(',')) {
            return error("missing ',' separating the indexed expression from "
                         "the index control");
        }
        auto const control = parse_index_control();
        if (!control.has_value()) return error(control.error());
        if (!accept(')')) return error("missing ')' after index control");
        return io_list_item{array, subscripts.value(), control.value()};
    }

    auto const name = parse_identifier();
    if (name.empty()) {
        return error("expected variable name in input-output list");
    }
    auto symbol = m_current_unit->find_symbol(name);
    if (symbol.type == datatype::unknown) {
        symbol.type = m_current_unit->implicit_type(name);
        m_current_unit->update_symbol(symbol);
    }
    if (symbol.kind == symbolkind::subprogram ||
        symbol.kind == symbolkind::external
    ) {
        return error("cannot use a subprogram in an input-output list");
    }
    
    if (symbol.shape.empty()) return io_list_item{name};
    if (match('(')) {
        auto const indices = parse_argument_list();
        if (!indices.has_value()) return error(indices.error());
        return io_list_item{name, indices.value()};
    }

    // Implicitly apply to the entire array.
    auto const induction_name = symbol_name{"_I_"};
    auto induction_symbol = m_current_unit->find_symbol(induction_name);
    if (induction_symbol.type == datatype::unknown) {
        induction_symbol.type = datatype::INTEGER;
        m_current_unit->update_symbol(induction_symbol);
    }
    auto const induction = std::make_shared<variable_node>(induction_name);
    return io_list_item{
        name, argument_list_t{induction},
        {.index=induction_name,
         .init=std::make_shared<constant_node>(symbol.shape[0].minimum),
         .final=std::make_shared<constant_node>(symbol.shape[0].maximum),
         .step=std::make_shared<constant_node>(1)}
    };
}

#if 0
parser::expected<array_with_indices> parser::parse_array_with_indices() {
    // <array_name> '(' <argument_list> ')'
    auto const array = parse_identifier();
    if (array.empty()) return error("missing array name");
    auto symbol = m_current_unit->find_symbol(array);
    if (symbol.shape.empty()) return error("'{}' is not an array", array);
    if (!match('(')) {
        return error("expected index(es) into array '{}'", array);
    }
    auto const index = parse_argument_list();
    if (!index.has_value()) return error(index.error());
    return array_with_indices(array, index.value());
}
#endif

parser::expected<index_control_t> parser::parse_index_control() {
    auto const index = parse_identifier();
    if (index.empty()) return error("missing index variable");

    auto symbol = m_current_unit->find_symbol(index);
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
    m_current_unit->update_symbol(symbol);

    if (!accept('=')) return error("expected '=' in index control");
    auto const init = parse_expression();
    if (!init.has_value()) return error(init.error());
    if (!accept(',')) return error("expected ',' in index control");
    auto const final = parse_expression();
    if (!final.has_value()) return error(final.error());
    auto const step =
        accept(',') ? parse_expression() : std::make_shared<constant_node>(1);
    if (!step.has_value()) return error(step.error());
    return index_control_t{index, init.value(), final.value(), step.value()};
}

parser::expected<data_list_t> parser::parse_data_list() {
    if (!accept('/')) return error("expected data list between slashes");
    auto data_list = data_list_t{};
    do {
        auto item = parse_constant();
        if (!item.has_value()) return error(item.error());
        if (accept('*') && item.value().type == datatype::INTEGER) {
            auto const repeat_count = item.value().value;
            if (repeat_count < 1) {
                return error("data item repeat count {} is less than 1",
                             repeat_count);
            }
            item = parse_constant();
            if (!item.has_value()) return error(item.error());
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
        auto const arg = parse_argument();
        if (!arg.has_value()) return error(arg.error());
        arguments.push_back(arg.value());
    } while (accept(','));
    if (!accept(')')) return error("expected ')' at end of argument list");
    return arguments;
}

parser::expected<expression_t> parser::parse_argument() {
    auto const bookmark = position();
    auto const name = parse_identifier();
    if (!name.empty() && (match(',') || match(')'))) {
        // It's just a variable name, so make sure it's added to the symbol
        // table. (If it's not already in the symbol table, then it's probably
        // an output argument that should be implicitly declared as a local
        // variable, just like we'd do for the left side of an assignment.)
        auto symbol = m_current_unit->find_symbol(name);
        if (symbol.type == datatype::unknown) {
            symbol.type = m_current_unit->implicit_type(name);
            assert(symbol.kind == symbolkind::local);
            m_current_unit->update_symbol(symbol);
        }
    }
    // Back up and parse the argument as an expression.
    m_it = bookmark;
    return parse_expression();
}

parser::expected<parser::keyword> parser::parse_keyword() {
    // Keywords may contain only letters, no digits.
    auto const begin = position();
    auto token = std::string_view(begin, position());
    while (match_letter()) {
        advance();
        token = std::string_view(begin, position());
        if (auto const kw = look_up_keyword(token); kw != keyword::unknown) {
            return kw;
        }
    }
    return error("expected a keyword but found '{}'", token);
}

parser::expected<variable_list_t> parser::parse_variable_list() {
    auto list = variable_list_t{};
    do {
        auto const item = parse_variable_list_item();
        if (!item.has_value()) return error(item.error());
        list.push_back(item.value());
    } while (accept(','));
    return list;
}

parser::expected<variable_list_item_t> parser::parse_variable_list_item() {
    if (accept('(')) {  // should be indexed array expression and index control
        auto const array = parse_identifier();
        if (array.empty()) return error("expected array name");
        auto const symbol = m_current_unit->find_symbol(array);
        if (symbol.shape.empty()) {
            return error("expected array name, saw '{}'", array);
        }
        auto const subscripts = parse_subscript_list();
        if (!subscripts.has_value()) return error(subscripts.error());
        if (subscripts.value().size() != symbol.shape.size()) {
            return error("expected {} subscripts for '{}'",
                         symbol.shape.size(), array);
        }
        if (!accept(',')) {
            return error("missing ',' separating the indexed expression from "
                         "the index control");
        }
        auto const control = parse_constant_index_control();
        if (!control.has_value()) return error(control.error());
        if (!accept(')')) return error("missing ')' after index control");
        return variable_list_item_t{array, subscripts.value(), control.value()};
    }

    auto const name = parse_identifier();
    if (name.empty()) {
        return error("expected variable name in variable list");
    }
    auto symbol = m_current_unit->find_symbol(name);
    if (symbol.type == datatype::unknown) {
        symbol.type = m_current_unit->implicit_type(name);
        m_current_unit->update_symbol(symbol);
    }
    if (symbol.kind == symbolkind::subprogram ||
        symbol.kind == symbolkind::external
    ) {
        return error("cannot use a subprogram in a variable list");
    }
    
    if (symbol.shape.empty()) return variable_list_item_t{name};
    if (match('(')) {
        auto const subscripts = parse_subscript_list();
        if (!subscripts.has_value()) return error(subscripts.error());
        return variable_list_item_t{name, subscripts.value()};
    }

    // Implicitly apply to the entire array.
    // TODO:  Generalize for more than 1 dimension.
    auto const subscripts = subscript_list_t{
        subscript_t{
            .induction = symbol_name{"_I_"},
            .coefficient = 1,
            .offset = 0
        }
    };
    auto const index_control = constant_index_control_t{
        .index = symbol_name{"_I_"},
        .init  = symbol.shape[0].minimum,
        .final = symbol.shape[0].maximum,
        .step  = 1
    };
    return variable_list_item_t{name, subscripts, index_control};
}

parser::expected<constant_index_control_t>
parser::parse_constant_index_control() {
    auto const index = parse_identifier();
    if (index.empty()) return error("missing index variable");

    auto symbol = m_current_unit->find_symbol(index);
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
    m_current_unit->update_symbol(symbol);

    if (!accept('=')) return error("expected '=' in index control");
    auto const k0 = parse_constant();
    if (!k0.has_value()) return error(k0.error());
    if (k0.value().type != datatype::INTEGER) {
        return error("initial value for induction must be an INTEGER constant");
    }
    auto const init = k0.value().value;
    if (!accept(',')) return error("expected ',' in index control");
    auto const k1 = parse_constant();
    if (!k1.has_value()) return error(k1.error());
    if (k1.value().type != datatype::INTEGER) {
        return error("final value for induction must be an INTEGER constant");
    }
    auto const final = k1.value().value;
    auto step = machine_word_t{1};
    if (accept(',')) {
        auto const k2 = parse_constant();
        if (!k2.has_value()) return error(k2.error());
        if (k2.value().type != datatype::INTEGER) {
            return error("increment for induction must be an INTEGER constant");
        }
        step = k2.value().value;
    }
    return constant_index_control_t{index, init, final, step};
}

parser::expected<subscript_list_t> parser::parse_subscript_list() {
    auto list = subscript_list_t{};
    if (!accept('(')) return error("expected '(' for subscripting");
    do {
        auto const subscript = parse_subscript();
        if (!subscript.has_value()) return error(subscript.error());
        list.push_back(subscript.value());
    } while (accept(','));
    if (!accept(')')) return error("expected ')' after subscript");
    return list;
}

parser::expected<subscript_t> parser::parse_subscript() {
    // TODO:  Allow any permutation of:  coefficient * index +|- offset
    auto const induction = parse_identifier();
    if (induction.empty()) return error("expected induction variable");
    machine_word_t coefficient = 1;
    machine_word_t offset = 0;
    return subscript_t{
        .induction = induction,
        .coefficient = coefficient,
        .offset = offset
    };
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
    if (accept('+')) return operator_t::add;
    if (accept('-')) return operator_t::subtract;
    if (accept('*')) return accept('*') ? operator_t::exponentiate
                                        : operator_t::multiply;
    if (accept('/')) return operator_t::divide;
    if (accept('.')) {
        if (accept('G')) {
            if (accept('T') && accept('.')) return operator_t::compare_gt;
            if (accept('E') && accept('.')) return operator_t::compare_gte;
            return error("expected .GT. or .GE.");
        }
        if (accept('L')) {
            if (accept('T') && accept('.')) return operator_t::compare_lt;
            if (accept('E') && accept('.')) return operator_t::compare_lte;
            return error("expected .LT. or .LE.");
        }
        if (accept('E')) {
            if (accept('Q')) {
                if (accept('.')) return operator_t::compare_eq;
                if (accept('V') && accept('.')) return operator_t::logic_eqv;
            }
            return error("expected .EQ. OR .EQV.");
        }
        if (accept('N')) {
            if (accept('E') && accept('.')) return operator_t::compare_ne;
            if (accept('O') && accept('T') && accept('.')) {
                return operator_t::logic_not;
            }
            return error("expected .NE. or .NOT.");
        }
        if (accept('A')) {
            if (accept('N') && accept('D') && accept('.')) {
                return operator_t::logic_and;
            }
            return error("expected .AND.");
        }
        if (accept('O')) {
            if (accept('R') && accept('.')) return operator_t::logic_or;
            return error("expected .OR.");
        }
        if (accept('X')) {
            if (accept('O') && accept('R') && accept('.')) {
                return operator_t::logic_xor;
            }
            return error("expected .XOR.");
        }
    }
    return error("expected an operator");
}

parser::expected<constant_t> parser::parse_constant() {
    switch (current()) {
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        case '-': case '"': {
            auto const k = parse_integer_constant();
            if (!k.has_value()) return error(k.error());
            return constant_t{k.value(), datatype::INTEGER};
        }
        case '\'': {
            auto const k = parse_literal_constant();
            if (!k.has_value()) return error(k.error());
            return constant_t{k.value(), datatype::LITERAL};
        }
        case '.': case 'T': case 'F': {
            auto const k = parse_logical_constant();
            if (!k.has_value()) return error(k.error());
            return constant_t{k.value(), datatype::LOGICAL};
        }
    }
    return error("expected a constant");
}

parser::expected<machine_word_t> parser::parse_integer_constant() {
    auto const base = accept('"') ? 8 : 10;
    return parse_integer(base);
}

parser::expected<machine_word_t> parser::parse_literal_constant() {
    // Packs a short character string into a machine word.  Does not handle
    // Hollerith data.
    auto const text = parse_literal();
    if (!text.has_value()) return error(text.error());
    if (text.value().size() > 5) {
        return error("literal constant cannot exceed 5 characters");
    }
    return make_literal(text.value());
}

parser::expected<machine_word_t> parser::parse_logical_constant() {
    if (accept('.')) {
        if (match('T')) {
            if (accept('T') && accept('R') && accept('U') && accept('E') &&
                accept('.'))
            {
                return LOGICAL_TRUE;
            }
        } else if (match('F')) {
            if (accept('F') && accept('A') && accept('L') && accept('S') &&
                accept('E') && accept('.'))
            {
                return LOGICAL_FALSE;
            }
        }
    } else if (accept('T')) {
        return LOGICAL_TRUE;
    } else if (accept('F')) {
        return LOGICAL_FALSE;
    }

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

void parser::add_label(statement_number_t number) {
    if (number == no_statement_number) return;
    if (m_current_unit == nullptr) return;
    auto symbol = m_current_unit->find_symbol(symbol_name{number});
    symbol.kind = symbolkind::label;
    if (symbol.type == datatype::unknown) {
        symbol.type = datatype::none;
    }
    // We're adding labels only for targets of GOTO, numerical IF, etc., so by
    // definition, they are referenced.
    symbol.referenced = true;
    m_current_unit->update_symbol(symbol);
}

bool parser::begin_program(symbol_name const &name) {
    assert(m_current_unit == nullptr);
    if (!name.empty()) m_program.set_unit_name(name);
    m_current_unit = &m_program;
    m_phase = phase1;
    return true;
}

bool parser::end_program() {
    assert(m_current_unit == &m_program);
    m_current_unit = nullptr;
    m_phase = phase5;
    return true;
}

bool parser::begin_subprogram(symbol_name const &name) {
    if (m_current_unit != nullptr) {
        std::print(std::cerr,
            "cannot nest subprogram {} within {}\n",
            name, m_current_unit->unit_name());
        return false;
    }
    m_subprogram.set_unit_name(name);
    m_current_unit = &m_subprogram;
    m_phase = phase1;
    return true;
}

bool parser::end_subprogram() {
    m_current_unit = nullptr;
    m_program.add_subprogram(std::move(m_subprogram));
    m_subprogram = unit{};
    m_phase = phase5;
    return true;
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

parser::keyword parser::look_up_keyword(std::string_view token) {
    static auto const map = std::map<std::string_view, keyword>{
        {"ACCEPT",      keyword::ACCEPT     },
        {"CALL",        keyword::CALL       },
        {"COMMON",      keyword::COMMON     },
        {"CONTINUE",    keyword::CONTINUE   },
        {"DATA",        keyword::DATA       },
        {"DIMENSION",   keyword::DIMENSION  },
        {"DO",          keyword::DO         },
        {"END",         keyword::END        },
        {"EXTERNAL",    keyword::EXTERNAL   },
        {"FORMAT",      keyword::FORMAT     },
        {"FUNCTION",    keyword::FUNCTION   },
        {"GOTO",        keyword::GOTO       },
        {"IF",          keyword::IF         },
        {"IMPLICIT",    keyword::IMPLICIT   },
        {"INTEGER",     keyword::INTEGER    },
        {"LOGICAL",     keyword::LOGICAL    },
        {"OPEN",        keyword::OPEN       },
        {"PAUSE",       keyword::PAUSE      },
        {"PROGRAM",     keyword::PROGRAM    },
        {"SUBROUTINE",  keyword::SUBROUTINE },
        {"READ",        keyword::READ       },
        {"REAL",        keyword::REAL       },
        {"RETURN",      keyword::RETURN     },
        {"STOP",        keyword::STOP       },
        {"TYPE",        keyword::TYPE       }
    };

    auto const it = map.find(token);
    return (it != map.end()) ? it->second : keyword::unknown;
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
