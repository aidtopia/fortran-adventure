#ifndef AID_EXPERIMENTAL_FORTRAN_UTILITY_H
#define AID_EXPERIMENTAL_FORTRAN_UTILITY_H

#include <filesystem>
#include <string>
#include <string_view>

namespace aid::fortran {

std::string escape_file_name(std::filesystem::path const &name);
std::string escape_string(std::string_view s);

}

#endif

