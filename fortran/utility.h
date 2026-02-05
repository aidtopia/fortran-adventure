#ifndef AID_FORTRAN_UTILITY_H
#define AID_FORTRAN_UTILITY_H

#include <array>
#include <concepts>
#include <filesystem>
#include <string>
#include <string_view>

namespace aid {

std::string escape_file_name(std::filesystem::path const &name);
std::string escape_string(std::string_view s);

std::string constexpr to_upper_ascii(std::string_view text) {
    std::string result;
    result.reserve(text.size());
    for (const auto &ch : text) {
        if ('a' <= ch && ch <= 'z') result.push_back(ch - 'a' + 'A');
        else if (ch < 128) result.push_back(ch);
    }
    return result;
}

template <std::integral T>
std::string_view plural(
    T count,
    std::string_view not_one = "s",
    std::string_view one = ""
) {
    return count != T{1} ? not_one : one;
}

template <typename ContainerT>
    requires requires (ContainerT c) { std::size(c); }
std::string_view plural(
    ContainerT const &c,
    std::string_view not_one = "s",
    std::string_view one = ""
) { return plural(std::size(c), not_one, one); }

template <typename ExtensionsT>
std::filesystem::path resolve_filepath(
    std::filesystem::path path,
    ExtensionsT const &extensions
) {
    if (!path.has_filename()) return {};
    if (std::filesystem::exists(path)) return path;

    auto const begin = std::begin(extensions);
    auto const end   = std::end(extensions);

    if (path.has_extension()) {
        // TODO:  On Windows, this find should (usually) be case-blind.
        if (std::find(begin, end, path.extension()) != end) {
            // The path has an approved extension; the file just doesn't exist.
            return path;
        } else {
            // The file is not found, the filename has an extension, but that
            // extension is not one of the defaults.  Before trying the default
            // extensions, we'll promote the current extension to the stem of
            // the filename, leaving us with an empty extension.
            path.replace_extension(
                path.extension().native() +
                std::filesystem::path(".").native());
        }
    }

    // Try each of the extensions provided.
    for (auto const &extension : extensions) {
        path.replace_extension(extension);
        if (std::filesystem::exists(path)) return path;
    }

    // No file exists with any of the default extensions, so use the first.
    if (begin != end) path.replace_extension(*begin);
    return path;
}

template <typename ExtensionsT>
std::filesystem::path resolve_filename(
    std::string_view filename,
    ExtensionsT extensions
) {
    return resolve_filepath(std::filesystem::path(filename).lexically_normal(),
                            extensions);
}

template <> inline
std::filesystem::path resolve_filename<std::string_view>(
    std::string_view filename,
    std::string_view default_extension
) {
    auto const extensions =
        std::array<std::filesystem::path, 1>{default_extension};
    return resolve_filename(filename, extensions);
}

}

#endif
