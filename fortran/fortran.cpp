#include "parser.h"
#include "generator.h"

#include <cstdlib>
#include <filesystem>
#include <format>
#include <iostream>
#include <print>

int main(int argc, char const *argv[]) {
    if (argc != 2) {
        std::print(std::cerr, R"(
Usage: fortran <file>

<file>: name of a Fortran IV source file (e.g., ADVENT.FOR)
)");
        return EXIT_FAILURE;
    }
    auto const parsed = aid::fortran::parser::parse_file(argv[1]);
    if (!parsed.has_value()) {
        std::print(std::cerr, "{}\n", parsed.error().message());
        exit(EXIT_FAILURE);
    }
    auto const program = parsed.value();
    program.print_symbol_table();

    auto const directory = std::filesystem::path("./target/");
    std::filesystem::create_directory(directory);
    auto const path = directory / program.unit_name().view();
    aid::fortran::generator::generate(path, program);

    return 0;
}
