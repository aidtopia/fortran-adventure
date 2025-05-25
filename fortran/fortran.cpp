#include "parser.h"
#include "generator.h"
#include "utility.h"

#include <array>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <iostream>
#include <print>
#include <vector>

void show_usage(std::ostream &out) {
    std::print(out, "Usage: fortran <file>...\n");
}

int main(int argc, char const *argv[]) {
    if (argc < 2) {
        show_usage(std::cerr);
        return EXIT_FAILURE;
    }

    // Resolve the source file names.
    auto constexpr extensions = std::array<std::string_view, 5>{
        ".f", ".for", ".f4", ".fiv", ".fortran"
    };
    auto paths = std::vector<std::filesystem::path>{};
    for (int i = 1; i < argc; ++i) {
        paths.push_back(aid::resolve_filename(argv[i], extensions));
    }

    // Parse the code.
    auto const parsed = aid::fortran::parser::parse_files(paths);
    if (!parsed.has_value()) {
        std::print(std::cerr, "{}\n", parsed.error().message());
        return EXIT_FAILURE;
    }

    // Show the symbol table.
    auto const &program = parsed.value();
    program.print_symbol_table();

    // Generate the C translation.
    auto const directory = std::filesystem::path("./target/");
    std::filesystem::create_directory(directory);
    auto const path = directory / program.unit_name().view();
    aid::fortran::generator::generate(path, program);

    return 0;
}
