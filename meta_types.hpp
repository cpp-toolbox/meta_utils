#ifndef META_TYPES_HPP
#define META_TYPES_HPP

#include <string>
#include <vector>

using type = std::string;

namespace types {
inline type INT = "int";
inline type UNSIGNED_INT = "unsigned int";
inline type FLOAT = "float";
inline type DOUBLE = "double";
inline type SHORT = "short";
inline type LONG = "long";
inline type STRING = "std::string";

inline std::vector<type> all = {types::INT,    types::UNSIGNED_INT, types::FLOAT,
                                types::DOUBLE, types::SHORT,        types::LONG};
inline bool is_known_type(const std::string &s) { return std::ranges::find(all.begin(), all.end(), s) != all.end(); }
}; // namespace types

#endif // META_TYPES_HPP
