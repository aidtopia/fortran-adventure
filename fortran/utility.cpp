#include "aid/experimental/fortran/utility.h"

namespace aid::fortran {

std::string escape_file_name(std::filesystem::path const &name) {
    std::string result;
    for (auto const ch : name.string()) {
        if (ch == '\\') result.push_back('\\');
        result.push_back(ch);
    }
    return result;
}

std::string escape_string(std::string_view s) {
    std::string escaped;
    escaped.reserve(s.size());
    for (auto const ch : s) {
        switch (ch) {
            case '\t':
                escaped.push_back(' ');
                break;
            case '\\':  [[fallthrough]];
            case '"':
                escaped.push_back('\\');
                escaped.push_back(ch);
                break;
            default:
                if (' ' <= ch && ch <= '~') escaped.push_back(ch);
                break;
        }
    }
    return escaped;
}


}
