#include "statements.h"

#include "unit.h"
#include "utility.h"

#include <format>

namespace aid::fortran {

std::string nonexecutable_statement::do_generate(unit const &) const {
    return "";
}

namespace {

    static std::string format_arguments(argument_list_t const &args) {
        auto text = std::string{};
        for (auto const &arg : args) {
            if (!text.empty()) text.append(", ");
            text.append(arg->generate_reference());
        }
        return text;
    }

    static void mark_index_control_referenced(
        index_control_t const &control,
        unit &u
    ) {
        if (control.index.empty()) return;
        u.mark_symbol_referenced(control.index);
        control.init->mark_referenced(u);
        control.final->mark_referenced(u);
        control.step->mark_referenced(u);
    }

    static void mark_io_list_item_referenced(io_list_item const &item, unit &u) {
        u.mark_symbol_referenced(item.variable);
        mark_index_control_referenced(item.index_control, u);
    }

    static void mark_io_list_referenced(io_list_t const &list, unit &u) {
        for (auto const &item : list) mark_io_list_item_referenced(item, u);
    }

}  // anonymous namespace

std::string assignment_statement::do_generate(unit const &) const {
    return std::format(
        "*{} = TMP_WRAP({});",
        m_lvalue->generate_reference(), m_rhs->generate_value());
}

void assignment_statement::do_mark_referenced(unit &u) const {
    m_lvalue->mark_referenced(u);
    m_rhs->mark_referenced(u);
}


std::string call_statement::do_generate(unit const &) const {
    // Do not use TMP_WRAP here: a subroutine call is not an expression.
    return std::format("{{ tmp_push(); sub{}({}); tmp_pop(0); }}", m_name,
                       format_arguments(m_args));
}

void call_statement::do_mark_referenced(unit &u) const {
    u.mark_symbol_referenced(m_name);
    for (auto const &arg : m_args) {
        arg->mark_referenced(u);
    }
}


std::string indirect_call_statement::do_generate(unit const &) const {
    return
        std::format("{{ tmp_push(); (*(psub{})v{})({}); tmp_pop(0); }}",
                    m_args.size(), m_name, format_arguments(m_args));
}

std::string continue_statement::do_generate(unit const &) const { return ";"; }

std::string definition_statement::do_generate(unit const &) const {
    return std::format("#define fn{}({}) TMP_WRAP({})",
        m_macro, format_parameters(m_params),
        m_definition->generate_value());
}

void definition_statement::do_mark_referenced(unit &u) const {
    // The definition statement does not reference the macro name.  It just
    // defines it.
    m_definition->mark_referenced(u);
}

std::string definition_statement::format_parameters(
    parameter_list_t const &params
) {
    if (params.empty()) return {};
    auto result = std::format("v{}", params[0]);
    for (std::size_t i = 1; i < params.size(); ++i) {
        result.append(std::format(", v{}", params[i]));
    }
    return result;
}


std::string do_statement::do_generate(unit const &u) const {
    if (m_body.empty()) return ";";
    auto loop = std::format(
        "for (*v{0} = TMP_WRAP({1}); "
             "in_range(*v{0}, TMP_WRAP({1}), TMP_WRAP({2})); "
             "*v{0} += TMP_WRAP({3})) {{\n",
        m_index_control.index,
        m_index_control.init->generate_value(),
        m_index_control.final->generate_value(),
        m_index_control.step->generate_value()
    );
    for (auto const &stmt : m_body) {
        loop.append(std::format("  {}\n", stmt->generate(u)));
    }
    loop.append(" }");
    return loop;
}

void do_statement::do_mark_referenced(unit &u) const {
    assert(!m_index_control.index.empty());
    mark_index_control_referenced(m_index_control, u);
    for (auto const &statement : m_body) {
        statement->mark_referenced(u);
    }
    // A DO statement should NOT mark the number of its final statement as a
    // referenced symbol.
}


std::string goto_statement::do_generate(unit const &) const {
    return std::format("goto L{};", m_target);
}


std::string computed_goto_statement::do_generate(unit const &) const {
    return std::format("switch (TMP_WRAP({})) {{\n{} }}",
        m_expression->generate_value(), cases());
}

void computed_goto_statement::do_mark_referenced(unit &u) const {
    m_expression->mark_referenced(u);
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
    return std::format("if (truth(TMP_WRAP({}))) {}",
        m_condition->generate_value(), m_then->generate(u));
}

void if_statement::do_mark_referenced(unit &u) const {
    m_condition->mark_referenced(u);
    m_then->mark_referenced(u);
}


std::string numeric_if_statement::do_generate(unit const &) const {
    return std::format(
        "switch (sign(TMP_WRAP({}))) {{\n"
        "  case -1: goto L{};\n"
        "  case  0: goto L{};\n"
        "  case  1: goto L{};\n"
        " }}",
        m_condition->generate_value(),
        m_negative, m_zero, m_positive);
}

void numeric_if_statement::do_mark_referenced(unit &u) const {
    m_condition->mark_referenced(u);
}


std::string open_statement::do_generate(unit const &) const {
    return std::format("io_open(TMP_WRAP({}), \"{}.DAT\");",
        m_unit->generate_value(), escape_file_name(m_name));
}

void open_statement::do_mark_referenced(unit &u) const {
    m_unit->mark_referenced(u);
}


std::string pause_statement::do_generate(unit const &) const {
    return std::format(
        "do {{ \n"
        "  puts(\"\\n{}\");\n"
        "  char buf[4] = {{0}};\n"
        "  const char *response = fgets(buf, sizeof(buf), stdin);\n"
        "  const int ch = response == NULL ? ' ' : response[0];\n"
        "  if (ch == 'G' || ch == 'g') break;\n"
        "  if (ch == 'X' || ch == 'x') exit(EXIT_SUCCESS);\n"
        "  puts(\"\\nPROGRAM IS PAUSED. TYPE 'G' (RETURN) TO GO OR "
                "'X' (RETURN) TO EXIT.\");\n"
        " }} while (1);", m_message);
}


std::string read_statement::do_generate(unit const &u) const {
    auto const preamble = std::format(
        "  io_loadrecord(TMP_WRAP({}));\n"
        "  io_selectformat(fmt{});\n",
        m_unit->generate_value(), m_format);

    auto inputs = std::string{};
    for (auto const &item : m_items) {
        auto input_item = std::string{};
        if (item.index_control.index.empty()) {
            auto variable_ref = std::string{};
            if (item.indices.empty()) {
                auto const variable_expr = variable_node{item.variable};
                variable_ref = variable_expr.generate_reference();
            } else {
                auto const symbol = u.find_symbol(item.variable);
                auto const array_expr =
                    array_index_node{item.variable, symbol.shape, item.indices};
                variable_ref = array_expr.generate_reference();
            }
            input_item = std::format("  io_input({});\n", variable_ref);
        } else {
            auto const symbol = u.find_symbol(item.variable);
            auto const array_expr =
                array_index_node{item.variable, symbol.shape, item.indices};
            input_item = std::format(
                "  for (*v{0} = TMP_WRAP({1}); "
                       "in_range(*v{0}, TMP_WRAP({1}), TMP_WRAP({2})); "
                       "*v{0} += TMP_WRAP({3})) {{\n"
                "   tmp_push();\n"
                "   io_input({4});\n"
                "   tmp_pop(0);\n"
                "  }}\n",
                item.index_control.index,
                item.index_control.init->generate_value(),
                item.index_control.final->generate_value(),
                item.index_control.step->generate_value(),
                array_expr.generate_reference());
        }
        inputs.append(input_item);
    }

    return std::format("{{\n{}{} }}", preamble, inputs);
}

void read_statement::do_mark_referenced(unit &u) const {
    m_unit->mark_referenced(u);
    mark_io_list_referenced(m_items, u);
}


std::string return_statement::do_generate(unit const &) const {
    return "goto Lreturn;";
}

void return_statement::do_mark_referenced(unit &u) const {
    if (!m_retval.empty()) u.mark_symbol_referenced(m_retval);
    // Since we implement RETURN as a goto to a special label, make sure that
    // label exists and is referenced.
    auto label = u.find_symbol(symbol_name{"return"});
    label.kind = symbolkind::label;
    label.referenced = true;
    u.update_symbol(label);
}


std::string stop_statement::do_generate(unit const &) const {
    return "goto Lstop;";
}

void stop_statement::do_mark_referenced(unit &u) const {
    // Since we implement STOP as a goto to a special label, make sure that
    // label exists and is referenced.
    auto label = u.find_symbol(symbol_name{"stop"});
    label.kind = symbolkind::label;
    label.referenced = true;
    u.update_symbol(label);
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
                variable_ref = variable_expr.generate_reference();
            } else {
                auto const symbol = u.find_symbol(item.variable);
                auto const array_expr =
                    array_index_node{item.variable, symbol.shape, item.indices};
                variable_ref = array_expr.generate_reference();
            }
            output_item = std::format("  io_output(0, {});\n", variable_ref);
        } else {
            auto const symbol = u.find_symbol(item.variable);
            auto const array_expr =
                array_index_node{item.variable, symbol.shape, item.indices};
            output_item = std::format(
                "  for (*v{0} = TMP_WRAP({1}); "
                       "in_range(*v{0}, TMP_WRAP({1}), TMP_WRAP({2})); "
                        "*v{0} += TMP_WRAP({3})) {{\n"
                "   tmp_push();\n"
                "   io_output(0, {4});\n"
                "   tmp_pop(0);\n"
                "  }}\n",
                item.index_control.index,
                item.index_control.init->generate_value(),
                item.index_control.final->generate_value(),
                item.index_control.step->generate_value(),
                array_expr.generate_reference());
        }
        outputs.append(output_item);
    }
    outputs.append("  io_output(0, NULL);\n");

    return std::format("{{\n{}{} }}", preamble, outputs);
}

void type_statement::do_mark_referenced(unit &u) const {
    mark_io_list_referenced(m_items, u);
}

}
