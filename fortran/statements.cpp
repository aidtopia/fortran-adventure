#include "statements.h"

#include "unit.h"
#include "utility.h"

#include <format>

namespace aid::fortran {

namespace {

    static std::string format_arguments(argument_list_t const &args) {
        auto text = std::string{};
        for (auto const &arg : args) {
            if (!text.empty()) text.append(", ");
            text.append(arg->generate_address());
        }
        return text;
    }

    static void mark_index_control_referenced(
        index_control_t const &control,
        unit &u,
        unsigned &t
    ) {
        if (control.index.empty()) return;
        u.mark_symbol_referenced(control.index);
        control.init->mark_referenced(u, t);
        control.limit->mark_referenced(u, t);
        control.step->mark_referenced(u, t);
    }

    static void mark_io_list_item_referenced(
        io_list_item const &item,
        unit &u,
        unsigned &t
    ) {
        u.mark_symbol_referenced(item.variable);
        for (auto &index : item.indices) index->mark_referenced(u, t);
        mark_index_control_referenced(item.index_control, u, t);
    }

    static void mark_io_list_referenced(
        io_list_t const &list,
        unit &u,
        unsigned &t
    ) {
        for (auto const &item : list) mark_io_list_item_referenced(item, u, t);
    }

}  // anonymous namespace

std::string assignment_statement::do_generate(unit const &) const {
    return std::format(
        "core[{}] = {};",
        m_lvalue->generate_address(), m_rhs->generate_value());
}

void assignment_statement::do_mark_reachable(unit &u, unsigned &t) {
    m_lvalue->mark_referenced(u, t);
    m_rhs->mark_referenced(u, t);
}


std::string call_statement::do_generate(unit const &) const {
    return std::format("sub{}({});", m_name, format_arguments(m_args));
}

void call_statement::do_mark_reachable(unit &u, unsigned &t) {
    u.mark_symbol_referenced(m_name);
    for (auto const &arg : m_args) {
        arg->mark_referenced(u, t);
    }
}


std::string indirect_call_statement::do_generate(unit const &) const {
    return std::format("(*(psub{})(void*)(core[v{}]))({});",
                       m_args.size(), m_name, format_arguments(m_args));
}

void indirect_call_statement::do_mark_reachable(unit &u, unsigned &t) {
    call_statement::do_mark_reachable(u, t);
    u.add_subroutine_pointer_type(m_args.size());
}


std::string continue_statement::do_generate(unit const &) const { return ";"; }


std::string do_statement::do_generate(unit const &u) const {
    if (m_body.empty()) return ";";
    auto body = std::string{};
    for (auto const &stmt : m_body) {
        body.append(std::format("   {}\n", stmt->generate(u)));
    }
    // Structured as a do-while because a Fortran DO loop always does the first
    // iteration, regardless of the limit and increment.
    // NOTE: This assumes the limit and the step can be evaluated once before
    // the first iteration.  I'm not sure whether that's how Fortran really
    // works.
    return std::format(
        "{{\n"
        "  const word_t limit{0} = {2};\n"
        "  const word_t step{0} = {3};\n"
        "  const word_t dir{0} = sign(step{0});\n"
        "  core[v{0}] = {1};\n"
        "  do {{\n{4}"
        "   core[v{0}] += step{0};\n"
        "  }} while (dir{0}*(core[v{0}] - limit{0}) <= 0);\n"
        " }}",
        m_index_control.index,
        m_index_control.init->generate_value(),
        m_index_control.limit->generate_value(),
        m_index_control.step->generate_value(),
        body
    );
}

void do_statement::do_mark_reachable(unit &u, unsigned &t) {
    assert(!m_index_control.index.empty());
    mark_index_control_referenced(m_index_control, u, t);
    for (auto const &statement : m_body) {
        statement->mark_reachable(u);
    }
    // A DO statement should NOT mark the _number_ of its final statement as a
    // referenced symbol.
}


std::string goto_statement::do_generate(unit const &) const {
    return std::format("goto L{};", m_target);
}

void goto_statement::do_mark_reachable(unit &u, unsigned &) {
    u.mark_symbol_referenced(symbol_name{m_target});
}

std::string computed_goto_statement::do_generate(unit const &) const {
    return std::format("switch ({}) {{\n{} }}",
        m_expression->generate_value(), cases());
}

void computed_goto_statement::do_mark_reachable(unit &u, unsigned &t) {
    m_expression->mark_referenced(u, t);
    for (auto const &target : m_targets) {
        u.mark_symbol_referenced(symbol_name{target});
    }
}

std::string computed_goto_statement::cases() const {
    auto result = std::string{};
    result.reserve(32 * m_targets.size());
    for (std::size_t i = 0; i < m_targets.size(); ++i) {
        result.append(std::format("  case {}: goto L{};\n",
                                    i + 1, m_targets[i]));
    }
    result.append(std::format("  default: break;\n"));
    return result;
}


std::string if_statement::do_generate(unit const &u) const {
    return std::format("if (truth({})) {}",
        m_condition->generate_value(), m_then->generate(u));
}

void if_statement::do_mark_reachable(unit &u, unsigned &t) {
    m_condition->mark_referenced(u, t);
    // Note that the then clause is a separate statement, so its temp counter
    // will be reset.  I think this is OK.    
    m_then->mark_reachable(u);
}


std::string numeric_if_statement::do_generate(unit const &) const {
    return std::format(
        "switch (sign({})) {{\n"
        "  case -1: goto L{};\n"
        "  case  0: goto L{};\n"
        "  case  1: goto L{};\n"
        " }}",
        m_condition->generate_value(),
        m_negative, m_zero, m_positive);
}

void numeric_if_statement::do_mark_reachable(unit &u, unsigned &t) {
    m_condition->mark_referenced(u, t);
    u.mark_symbol_referenced(symbol_name{m_negative});
    u.mark_symbol_referenced(symbol_name{m_zero});
    u.mark_symbol_referenced(symbol_name{m_positive});
}


std::string open_statement::do_generate(unit const &) const {
    return std::format("io_open({}, \"{}.DAT\");",
        m_iounit->generate_value(), escape_file_name(m_name));
}

void open_statement::do_mark_reachable(unit &u, unsigned &t) {
    m_iounit->mark_referenced(u, t);
}


std::string pause_statement::do_generate(unit const &) const {
    return std::format("host_pause(\"\\n{}\");\n", escape_string(m_message));
}


std::string read_statement::do_generate(unit const &u) const {
    auto const preamble = std::format(
        "  io_loadrecord({});\n"
        "  io_selectformat(fmt{});\n",
        m_iounit->generate_value(), m_format);

    auto inputs = std::string{};
    for (auto const &item : m_items) {
        auto input_item = std::string{};
        if (item.index_control.index.empty()) {
            auto variable_ref = std::string{};
            if (item.indices.empty()) {
                auto const variable_expr = variable_node{item.variable};
                variable_ref = variable_expr.generate_address();
            } else {
                auto const symbol = u.find_symbol(item.variable);
                auto const array_expr =
                    array_subscript_node{item.variable, item.indices};
                variable_ref = array_expr.generate_address();
            }
            input_item = std::format("  io_input({});\n", variable_ref);
        } else {
            auto const symbol = u.find_symbol(item.variable);
            auto const array_expr =
                array_subscript_node{item.variable, item.indices};
            input_item = std::format(
                "  for (core[v{0}] = {1}; "
                       "in_range(core[v{0}], {1}, {2}); "
                       "core[v{0}] += {3}) {{\n"
                "   io_input({4});\n"
                "  }}\n",
                item.index_control.index,
                item.index_control.init->generate_value(),
                item.index_control.limit->generate_value(),
                item.index_control.step->generate_value(),
                array_expr.generate_address());
        }
        inputs.append(input_item);
    }

    return std::format("{{\n{}{} }}", preamble, inputs);
}

void read_statement::do_mark_reachable(unit &u, unsigned &t) {
    m_iounit->mark_referenced(u, t);
    u.mark_symbol_referenced(symbol_name{m_format});
    mark_io_list_referenced(m_items, u, t);
}


std::string return_statement::do_generate(unit const &) const {
    return m_retval.empty() ? "return;"
                            : std::format("return core[v{}];", m_retval);
}

void return_statement::do_mark_reachable(unit &u, unsigned &) {
    if (!m_retval.empty()) u.mark_symbol_referenced(m_retval);
}


std::string stop_statement::do_generate(unit const &) const {
    return "host_exit(EXIT_SUCCESS);";
}


std::string type_statement::do_generate(unit const &u) const {
    auto const preamble =
        std::format("  io_selectformat(fmt{});\n", m_format);

    auto outputs = std::string{};
    for (auto const &item : m_items) {
        auto output_item = std::string{};
        if (item.index_control.index.empty()) {
            auto variable_ref = std::string{};
            if (item.indices.empty()) {
                auto const variable_expr = variable_node{item.variable};
                variable_ref = variable_expr.generate_address();
            } else {
                auto const symbol = u.find_symbol(item.variable);
                auto const array_expr =
                    array_subscript_node{item.variable, item.indices};
                variable_ref = array_expr.generate_address();
            }
            output_item = std::format("  io_output(0, {});\n", variable_ref);
        } else {
            auto const symbol = u.find_symbol(item.variable);
            auto const array_expr =
                array_subscript_node{item.variable, item.indices};
            output_item = std::format(
                "  for (core[v{0}] = {1}; "
                       "in_range(core[v{0}], {1}, {2}); "
                        "core[v{0}] += {3}) {{\n"
                "   io_output(0, {4});\n"
                "  }}\n",
                item.index_control.index,
                item.index_control.init->generate_value(),
                item.index_control.limit->generate_value(),
                item.index_control.step->generate_value(),
                array_expr.generate_address());
        }
        outputs.append(output_item);
    }
    outputs.append("  io_output(0, 0);\n");

    return std::format("{{\n{}{} }}", preamble, outputs);
}

void type_statement::do_mark_reachable(unit &u, unsigned &t) {
    u.mark_symbol_referenced(symbol_name{m_format});
    mark_io_list_referenced(m_items, u, t);
}

}
