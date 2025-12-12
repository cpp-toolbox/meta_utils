#ifndef META_UTILS_HPP
#define META_UTILS_HPP

#include <fstream>
#include <iostream>
#include <iterator>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>
#include <functional>
#include <cstddef> // for size_t
#include <variant>

#include "sbpt_generated_includes.hpp"

/**
 * @todo I want logic that can automatically extract a function from source code that would be really handy, because
 * that's the glue of programming half the time.
 */
namespace meta_utils {

/**
 * @class MetaInclude
 * @brief Represents an include directive for C++ source code,
 *        providing control over whether it is a system or local include.
 *
 * The MetaInclude class is used to generate standardized C++ include statements.
 * It supports both system includes (`#include <...>`) and local includes
 * (`#include "..."`) depending on the specified type.
 */
class MetaInclude {
  public:
    /**
     * @enum Type
     * @brief Distinguishes between local and system include types.
     */
    enum class Type {
        Local, ///< Include file from the project directory (uses quotes: `"..."`)
        System ///< Include file from system paths (uses angle brackets: `<...>`)
    };

    /**
     * @brief Constructs a MetaInclude with the given path and type.
     *
     * @param project_relative_path The path to the header file, relative to the project root.
     * @param type The type of include (Local or System). Defaults to Local.
     *
     * Example usage:
     * @code
     * // Local include example (uses quotes)
     * MetaInclude local_inc("src/utility/glm_meta_types/glm_meta_types.hpp");
     * std::cout << local_inc.str() << std::endl;
     * // Output: #include "src/utility/glm_meta_types/glm_meta_types.hpp"
     *
     * // System include example (uses angle brackets)
     * MetaInclude system_inc("vector", MetaInclude::Type::System);
     * std::cout << system_inc.str() << std::endl;
     * // Output: #include <vector>
     *
     * // Relative include example (base path stripping)
     * MetaInclude relative_inc("project/src/utility/math.hpp");
     * std::cout << relative_inc.str("project") << std::endl;
     * // Output: #include "src/utility/math.hpp"
     * @endcode
     */
    MetaInclude(std::string project_relative_path, Type type = Type::Local)
        : path(std::move(project_relative_path)), type(type) {}

    /**
     * @brief Converts the MetaInclude into a valid C++ `#include` directive string.
     *
     * @param base The base directory to which the path should be made relative.
     *             Defaults to the current directory (`"."`).
     * @return A formatted include directive as a string.
     */
    std::string str(const std::filesystem::path &base = ".") const {
        if (type == Type::System) {
            return "#include <" + path + ">";
        } else {
            auto rel_path = std::filesystem::relative(std::filesystem::path(path), base);
            return "#include \"" + rel_path.generic_string() + "\"";
        }
    }

  private:
    std::string path; ///< The path to the include file (relative to project root).
    Type type;        ///< Whether the include is Local or System.
};

class MetaType;

using MetaTemplateParameter = std::variant<unsigned int, MetaType>;

/**
 * @class MetaType
 * @brief Represents a type with metadata used for reflection, serialization, and code generation.
 * @note this entire class is probably going to be replaced one day since this is my first attempt at this and I'm naive
 *
 * The `MetaType` class encapsulates information about a data type, including its conversion
 * functions, serialization routines, template parameters, and other metadata. It is used to
 * describe both concrete and generic types, allowing introspection and code generation for
 * type-related operations.
 *
 * ## Example
 * ```cpp
 * MetaType int_type("int", "[](std::string s){ return std::stoi(s); }",
 *                   "[](int v){ return std::to_string(v); }",
 *                   "[](int v){ ...serialize... }",
 *                   "[](int v){ return sizeof(int); }",
 *                   "[](std::vector<uint8_t> data){ ...deserialize... }",
 *                   R"(\d+)");
 * ```
 *
 * ## Notes
 * - For templated types (e.g., `std::vector<T>`), `template_parameters` contains metadata for each
 *   parameter, which may itself be another `MetaType`.
 * - The `variably_sized` flag indicates whether serialized size differs from `sizeof(T)` (e.g., strings or vectors).
 * - The `size_when_serialized_bytes_func_lambda` field exists because serialized sizes can differ from
 *   memory sizes due to padding removal.
 *
 *   @note we can't use MetaFunctions in here because we get a cyclic dependency. Can we though?
 *
 *   @note this is serializable?
 */
class MetaType {
  public:
    /// The base name of the type (e.g., `"int"`, `"std::vector"`).
    std::string base_type_name;

    /// Lambda expression (as a string) converting from a string to this type.
    std::string string_to_type_func_lambda;

    /// Lambda expression (as a string) converting from this type to a string.
    std::string type_to_string_func_lambda;

    /// Lambda expression (as a string) for serializing this type to bytes.
    std::string serialize_type_func_lambda;

    /// Lambda expression (as a string) returning the number of bytes this type occupies when serialized.
    /// Accounts for padding differences between memory layout and serialized representation.
    std::string size_when_serialized_bytes_func_lambda;

    /// Lambda expression (as a string) for deserializing this type from bytes.
    std::string deserialize_type_func_lambda;

    /// Regular expression string used to match literal representations of this type.
    std::string literal_regex;

    /// List of header files required to use this type.
    std::vector<MetaInclude> includes_required;

    /// Indicates whether the type is variably sized when serialized (e.g., `std::string`, `std::vector`).
    bool variably_sized = false;

    /// Set of type names known to be variably sized. @warning you must add it in here for it to work properly because
    /// when you construct a meta type via a string for some reason the only way we can know its variably sized is if
    /// its in this map, HORRIBLE FIX
    std::set<std::string> variably_sized_types = {"std::vector", "std::string", "std::unordered_map", "std::optional"};

    /// Template parameters of the type (e.g., the `T` in `std::vector<T>`).
    /// Each parameter can be either a `MetaType` (for type parameters) or an unsigned integer
    /// (for non-type template parameters). Integer is coming from std::array and such
    std::vector<MetaTemplateParameter> template_parameters;

    /**
     * @brief Equality operator comparing all relevant metadata fields.
     *
     * @param other The `MetaType` to compare with.
     * @return True if both types have the same base name, conversion functions, literal regex,
     *         and template parameters.
     */
    bool operator==(const MetaType &other) const {
        return base_type_name == other.base_type_name &&
               string_to_type_func_lambda == other.string_to_type_func_lambda &&
               type_to_string_func_lambda == other.type_to_string_func_lambda && literal_regex == other.literal_regex &&
               template_parameters == other.template_parameters;
    }

    MetaType() {}
    /**
     * @brief Constructs a `MetaType` with the given metadata and optional template parameters.
     *
     * @param name The base name of the type.
     * @param string_to_type_func Lambda string converting from string to type.
     * @param type_to_string_func Lambda string converting from type to string.
     * @param serialize_type_func Lambda string for serializing the type.
     * @param size_when_serialized_bytes_func Lambda string for computing serialized size.
     * @param deserialize_type_func Lambda string for deserializing the type.
     * @param literal_regex Regular expression matching type literals.
     * @param element_types Optional list of template parameter types.
     */
    MetaType(std::string name, std::string string_to_type_func, std::string type_to_string_func,
             std::string serialize_type_func, std::string size_when_serialized_bytes_func,
             std::string deserialize_type_func, std::string literal_regex,
             const std::vector<MetaTemplateParameter> &element_types = {})
        : base_type_name(std::move(name)), string_to_type_func_lambda(std::move(string_to_type_func)),
          type_to_string_func_lambda(std::move(type_to_string_func)), serialize_type_func_lambda(serialize_type_func),
          size_when_serialized_bytes_func_lambda(size_when_serialized_bytes_func),
          deserialize_type_func_lambda(deserialize_type_func), literal_regex(std::move(literal_regex)),
          template_parameters(element_types) {

        variably_sized = variably_sized_types.find(base_type_name) != variably_sized_types.end();
    }

    /**
     * @brief Returns a detailed string representation of this `MetaType` for debugging.
     *
     * @return A formatted multiline string describing this `MetaType` and its template parameters.
     */
    std::string to_string() const { return to_string_rec(*this); }

    /**
     * @brief Returns the fully qualified C++ type name represented by this `MetaType`.
     *
     * Includes nested template arguments, e.g., `"std::vector<std::string>"`.
     *
     * @return The type name as a string.
     */
    std::string get_type_name() const { return get_type_name_rec(*this); }

  private:
    /**
     * @brief Recursively constructs the full C++ type name for a `MetaType`.
     *
     * @param meta_type The type to process.
     * @return The fully qualified type name.
     */
    std::string get_type_name_rec(const MetaType &meta_type) const {
        if (meta_type.template_parameters.empty()) {
            return meta_type.base_type_name;
        }

        std::ostringstream oss;
        oss << meta_type.base_type_name << "<";

        for (size_t i = 0; i < meta_type.template_parameters.size(); ++i) {
            if (i > 0) {
                oss << ", ";
            }

            std::visit(
                [&](auto &&param) {
                    using T = std::decay_t<decltype(param)>;
                    if constexpr (std::is_same_v<T, unsigned int>) {
                        oss << param; // print integer literal directly
                    } else if constexpr (std::is_same_v<T, MetaType>) {
                        oss << get_type_name_rec(param); // recurse on nested type
                    }
                },
                meta_type.template_parameters[i]);
        }

        oss << ">";
        return oss.str();
    }

    /**
     * @brief Recursively builds a formatted string representation of the `MetaType`.
     *
     * Used internally by `to_string()` to print nested template parameters.
     *
     * @param meta_type The `MetaType` to convert.
     * @return A formatted string.
     */
    std::string to_string_rec(const MetaType &meta_type) const {
        std::ostringstream oss;
        oss << "MetaType {\n";
        oss << "  name: " << meta_type.base_type_name << "\n";
        oss << "  string_to_type_func: " << meta_type.string_to_type_func_lambda << "\n";
        oss << "  type_to_string_func: " << meta_type.type_to_string_func_lambda << "\n";
        oss << "  literal_regex: " << meta_type.literal_regex << "\n";

        if (!meta_type.template_parameters.empty()) {
            oss << "  element_types: [\n";
            for (const auto &elem : meta_type.template_parameters) {
                std::visit(
                    [&](auto &&param) {
                        using T = std::decay_t<decltype(param)>;
                        std::string elem_str;
                        if constexpr (std::is_same_v<T, unsigned int>) {
                            elem_str = "unsigned int: " + std::to_string(param);
                        } else if constexpr (std::is_same_v<T, MetaType>) {
                            elem_str = to_string_rec(param); // recurse
                        }

                        std::istringstream iss(elem_str);
                        std::string line;
                        while (std::getline(iss, line)) {
                            oss << "    " << line << "\n";
                        }
                    },
                    elem);
            }
            oss << "  ]\n";
        } else {
            oss << "  element_types: []\n";
        }

        oss << "}";
        return oss.str();
    }
};

inline std::string create_to_string_lambda(std::string type) {
    return "[](const " + type + " &v) { return std::to_string(v); }";
};

// CONCRETE
// NOTE: concrete types have the good property that given a type name, we can do
// a one to one mapping to one of the below concrete types
inline MetaType UNSIGNED_INT =
    MetaType("unsigned int", "[](const std::string &s) { return static_cast<unsigned int>(std::stoul(s)); }",
             create_to_string_lambda("unsigned int"),
             "[](const unsigned int &v) { "
             "  std::vector<uint8_t> buf(sizeof(unsigned int)); "
             "  std::memcpy(buf.data(), &v, sizeof(unsigned int)); "
             "  return buf; }",
             "[](const unsigned int &v) { return sizeof(unsigned int); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  unsigned int v; "
             "  std::memcpy(&v, buf.data(), sizeof(unsigned int)); "
             "  return v; }",
             regex_utils::unsigned_int_regex);

inline MetaType UINT8_T =
    MetaType("uint8_t", "[](const std::string &s) { return static_cast<uint8_t>(std::stoul(s)); }",
             create_to_string_lambda("uint8_t"),
             "[](const uint8_t &v) { "
             "  std::vector<uint8_t> buf(sizeof(uint8_t)); "
             "  std::memcpy(buf.data(), &v, sizeof(uint8_t)); "
             "  return buf; }",
             "[](const uint8_t &v) { return sizeof(uint8_t); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  uint8_t v; "
             "  std::memcpy(&v, buf.data(), sizeof(uint8_t)); "
             "  return v; }",
             regex_utils::unsigned_int_regex);

inline MetaType UINT32_T =
    MetaType("uint32_t", "[](const std::string &s) { return static_cast<uint32_t>(std::stoul(s)); }",
             create_to_string_lambda("uint32_t"),
             "[](const uint32_t &v) { "
             "  std::vector<uint8_t> buf(sizeof(uint32_t)); "
             "  std::memcpy(buf.data(), &v, sizeof(uint32_t)); "
             "  return buf; }",
             "[](const uint32_t &v) { return sizeof(uint32_t); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  uint32_t v; "
             "  std::memcpy(&v, buf.data(), sizeof(uint32_t)); "
             "  return v; }",
             regex_utils::unsigned_int_regex);

inline MetaType SIZE_T = MetaType("size_t", "[](const std::string &s) { return static_cast<size_t>(std::stoull(s)); }",
                                  create_to_string_lambda("size_t"),
                                  "[](const size_t &v) { "
                                  "  std::vector<uint8_t> buf(sizeof(size_t)); "
                                  "  std::memcpy(buf.data(), &v, sizeof(size_t)); "
                                  "  return buf; }",
                                  "[](const size_t &v) { return sizeof(size_t); }",
                                  "[](const std::vector<uint8_t> &buf) { "
                                  "  size_t v; "
                                  "  std::memcpy(&v, buf.data(), sizeof(size_t)); "
                                  "  return v; }",
                                  regex_utils::unsigned_int_regex);

inline MetaType CHAR = MetaType("char", "[](const std::string &s) { return static_cast<char>(s.empty() ? 0 : s[0]); }",
                                create_to_string_lambda("char"),
                                "[](const char &v) { "
                                "  std::vector<uint8_t> buf(sizeof(char)); "
                                "  std::memcpy(buf.data(), &v, sizeof(char)); "
                                "  return buf; }",
                                "[](const char &v) { return sizeof(char); }",
                                "[](const std::vector<uint8_t> &buf) { "
                                "  char v; "
                                "  std::memcpy(&v, buf.data(), sizeof(char)); "
                                "  return v; }",
                                regex_utils::char_literal);

inline MetaType INT =
    MetaType("int", "[](const std::string &s) { return std::stoi(s); }", create_to_string_lambda("int"),
             "[](const int &v) { "
             "  std::vector<uint8_t> buf(sizeof(int)); "
             "  std::memcpy(buf.data(), &v, sizeof(int)); "
             "  return buf; }",
             "[](const int &v) { return sizeof(int); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  int v; "
             "  std::memcpy(&v, buf.data(), sizeof(int)); "
             "  return v; }",
             regex_utils::int_regex);

inline MetaType SHORT = MetaType("short", "[](const std::string &s) { return static_cast<short>(std::stoi(s)); }",
                                 create_to_string_lambda("short"),
                                 "[](const short &v) { "
                                 "  std::vector<uint8_t> buf(sizeof(short)); "
                                 "  std::memcpy(buf.data(), &v, sizeof(short)); "
                                 "  return buf; }",
                                 "[](const short &v) { return sizeof(short); }",
                                 "[](const std::vector<uint8_t> &buf) { "
                                 "  short v; "
                                 "  std::memcpy(&v, buf.data(), sizeof(short)); "
                                 "  return v; }",
                                 regex_utils::int_regex);

inline MetaType LONG =
    MetaType("long", "[](const std::string &s) { return std::stol(s); }", create_to_string_lambda("long"),
             "[](const long &v) { "
             "  std::vector<uint8_t> buf(sizeof(long)); "
             "  std::memcpy(buf.data(), &v, sizeof(long)); "
             "  return buf; }",
             "[](const long &v) { return sizeof(long); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  long v; "
             "  std::memcpy(&v, buf.data(), sizeof(long)); "
             "  return v; }",
             regex_utils::int_regex);

inline MetaType FLOAT =
    MetaType("float", "[](const std::string &s) { return std::stof(s); }",
             "[](const float &v) {"
             "  std::ostringstream oss;"
             "  oss << std::fixed << std::setprecision(std::numeric_limits<float>::max_digits10) << v;"
             "  return oss.str();"
             "}",
             "[](const float &v) { "
             "  std::vector<uint8_t> buf(sizeof(float)); "
             "  std::memcpy(buf.data(), &v, sizeof(float)); "
             "  return buf; }",
             "[](const float &v) { return sizeof(float); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  float v; "
             "  std::memcpy(&v, buf.data(), sizeof(float)); "
             "  return v; }",
             regex_utils::float_regex);

inline MetaType DOUBLE =
    MetaType("double", "[](const std::string &s) { return std::stod(s); }",
             "[](const double &v) {"
             "  std::ostringstream oss;"
             "  oss << std::fixed << std::setprecision(std::numeric_limits<double>::max_digits10) << v;"
             "  return oss.str();"
             "}",
             "[](const double &v) { "
             "  std::vector<uint8_t> buf(sizeof(double)); "
             "  std::memcpy(buf.data(), &v, sizeof(double)); "
             "  return buf; }",
             "[](const double &v) { return sizeof(double); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  double v; "
             "  std::memcpy(&v, buf.data(), sizeof(double)); "
             "  return v; }",
             regex_utils::float_regex);

inline MetaType STRING =
    MetaType("std::string",
             "[](const std::string &s) { "
             "  if (s.size() >= 2 && s.front() == '\"' && s.back() == '\"') "
             "    return s.substr(1, s.size() - 2); "
             "  return s; }",
             "[](const std::string &s) { return s; }",
             "[](const std::string &v) { "
             "  std::vector<uint8_t> buf; "
             "  size_t len = v.size(); "
             "  buf.resize(sizeof(size_t) + len); "
             "  std::memcpy(buf.data(), &len, sizeof(size_t)); "
             "  std::memcpy(buf.data() + sizeof(size_t), v.data(), len); "
             "  return buf; }",
             "[](const std::string &v) { return sizeof(size_t) + v.size(); }",
             "[](const std::vector<uint8_t> &buf) { "
             "  if (buf.size() < sizeof(size_t)) return std::string(); "
             "  size_t len; "
             "  std::memcpy(&len, buf.data(), sizeof(size_t)); "
             "  if (buf.size() < sizeof(size_t) + len) return std::string(); "
             "  return std::string(reinterpret_cast<const char*>(buf.data() + sizeof(size_t)), len); }",
             regex_utils::string_literal);

inline MetaType FILESYSTEM_PATH = MetaType(
    "std::filesystem::path",
    // from string → path
    "[](const std::string &s) { "
    "  if (s.size() >= 2 && s.front() == '\"' && s.back() == '\"') "
    "    return std::filesystem::path(s.substr(1, s.size() - 2)); "
    "  return std::filesystem::path(s); }",
    // to string
    "[](const std::filesystem::path &p) { return p.string(); }",
    // to bytes
    "[](const std::filesystem::path &p) { "
    "  std::string s = p.string(); "
    "  std::vector<uint8_t> buf; "
    "  size_t len = s.size(); "
    "  buf.resize(sizeof(size_t) + len); "
    "  std::memcpy(buf.data(), &len, sizeof(size_t)); "
    "  std::memcpy(buf.data() + sizeof(size_t), s.data(), len); "
    "  return buf; }",
    // size in bytes
    "[](const std::filesystem::path &p) { "
    "  std::string s = p.string(); "
    "  return sizeof(size_t) + s.size(); }",
    // from bytes → path
    "[](const std::vector<uint8_t> &buf) { "
    "  if (buf.size() < sizeof(size_t)) return std::filesystem::path(); "
    "  size_t len; "
    "  std::memcpy(&len, buf.data(), sizeof(size_t)); "
    "  if (buf.size() < sizeof(size_t) + len) return std::filesystem::path(); "
    "  return std::filesystem::path(std::string(reinterpret_cast<const char*>(buf.data() + sizeof(size_t)), len)); }",
    regex_utils::string_literal);

// TODO: .pattern() isn't a real function so we can't get the pattern out of a regex and I don't have a way to deal with
// this yet. This is why you don't find regex in concrete types.
inline MetaType REGEX =
    MetaType("std::regex",
             // from string → regex
             "[](const std::string &s) { "
             "  if (s.size() >= 2 && s.front() == '\"' && s.back() == '\"') "
             "    return std::regex(s.substr(1, s.size() - 2)); "
             "  return std::regex(s); }",
             // to string
             "[](const std::regex &r) { return r.pattern(); }",
             // to bytes
             "[](const std::regex &r) { "
             "  std::string pattern = r.pattern(); "
             "  std::vector<uint8_t> buf; "
             "  size_t len = pattern.size(); "
             "  buf.resize(sizeof(size_t) + len); "
             "  std::memcpy(buf.data(), &len, sizeof(size_t)); "
             "  std::memcpy(buf.data() + sizeof(size_t), pattern.data(), len); "
             "  return buf; }",
             // size in bytes
             "[](const std::regex &r) { "
             "  std::string pattern = r.pattern(); "
             "  return sizeof(size_t) + pattern.size(); }",
             // from bytes → regex
             "[](const std::vector<uint8_t> &buf) { "
             "  if (buf.size() < sizeof(size_t)) return std::regex(); "
             "  size_t len; "
             "  std::memcpy(&len, buf.data(), sizeof(size_t)); "
             "  if (buf.size() < sizeof(size_t) + len) return std::regex(); "
             "  return std::regex(std::string(reinterpret_cast<const char*>(buf.data() + sizeof(size_t)), len)); }",
             regex_utils::string_literal);

inline MetaType BOOL = MetaType("bool", "[](const std::string &s) { return s == \"true\"; }",
                                "[](const bool &v) { return v ? \"true\" : \"false\"; }",
                                "[](const bool &v) { "
                                "  std::vector<uint8_t> buf(1); "
                                "  buf[0] = v ? 1 : 0; "
                                "  return buf; }",
                                // NOTE: we don't use sizeof(bool) because of how we serialize always one byte.
                                "[](const bool &v) { return sizeof(uint8_t); }",
                                "[](const std::vector<uint8_t> &buf) { "
                                "  return buf[0] != 0; }",
                                R"(true|false|1|0)");

inline meta_utils::MetaType meta_type_type("meta_utils::MetaType", "[](const meta_utils::MetaType &mt){}",
                                           "[](){ return \"\";}", "[](){}",
                                           "[](const meta_utils::MetaType &v) { return sizeof(meta_utils::MetaType); }",
                                           "[](){}", "MetaType");

// GENERICS
// NOTE: generic types cannot be defined directly as variables, there are
// "infinitely" many types possible so we can't actually make variables for
// them, instead we define a function that generates a concrete genric type for
// a given generic type

std::string create_string_to_optional_of_type_func(MetaType type_parameter);
std::string create_optional_of_type_to_string_func(MetaType type_parameter);
std::string create_optional_of_type_serialize_func(MetaType type_parameter);
std::string create_optional_of_type_serialized_size_func(MetaType type_parameter);
std::string create_optional_of_type_deserialize_func(MetaType type_parameter);

// NOTE: this function does not yet support recursive vector types (only one
// layer deep support)
std::string create_string_to_vector_of_type_func(MetaType type_parameter);
std::string create_vector_of_type_to_string_func(MetaType type_parameter);
std::string create_vector_of_type_serialize_func(MetaType type_parameter);
std::string create_vector_of_type_serialized_size_func(MetaType type_parameter);
std::string create_vector_of_type_deserialize_func(MetaType type_parameter);

std::string create_string_to_array_of_type_func(MetaType type_parameter, unsigned int size);
std::string create_array_of_type_to_string_func(MetaType type_parameter, unsigned int size);
std::string create_array_of_type_serialize_func(MetaType type_parameter, unsigned int size);
std::string create_array_of_type_serialized_size_func(MetaType type_parameter, unsigned int size);
std::string create_array_of_type_deserialize_func(MetaType type_parameter, unsigned int size);

inline MetaType construct_optional_metatype(MetaType generic_type) {
    auto string_to_optional_of_generic_type_func = create_string_to_optional_of_type_func(generic_type);
    auto optional_of_type_to_string_func = create_optional_of_type_to_string_func(generic_type);
    auto optional_of_type_serialize_func = create_optional_of_type_serialize_func(generic_type);
    auto optional_of_type_serialized_size_func = create_optional_of_type_serialized_size_func(generic_type);
    auto optional_of_type_deserialize_func = create_optional_of_type_deserialize_func(generic_type);

    return MetaType("std::optional",
                    string_to_optional_of_generic_type_func, // string → optional<T>
                    optional_of_type_to_string_func,         // optional<T> → string
                    optional_of_type_serialize_func,         // optional<T> → bytes
                    optional_of_type_serialized_size_func,   // optional<T> → size in bytes
                    optional_of_type_deserialize_func,       // bytes → optional<T>
                    regex_utils::any_char_greedy,            // literal regex (matches only nullopt string)
                    {generic_type}                           // contained type(s)
    );
}

inline MetaType construct_vector_metatype(MetaType generic_type) {
    auto string_to_vector_of_generic_type_func = create_string_to_vector_of_type_func(generic_type);
    auto vector_of_type_to_string_func = create_vector_of_type_to_string_func(generic_type);
    auto vector_of_type_serialize_func = create_vector_of_type_serialize_func(generic_type);
    auto vector_of_type_serialized_size_func = create_vector_of_type_serialized_size_func(generic_type);
    auto vector_of_type_deserialize_func = create_vector_of_type_deserialize_func(generic_type);

    return MetaType("std::vector",
                    string_to_vector_of_generic_type_func, // string → vector<T>
                    vector_of_type_to_string_func,         // vector<T> → string
                    vector_of_type_serialize_func,         // vector<T> → bytes
                    vector_of_type_serialized_size_func,   // vector<T> → size in bytes
                    vector_of_type_deserialize_func,       // bytes → vector<T>
                    regex_utils::any_char_greedy, {generic_type});
}

inline MetaType construct_array_metatype(MetaType generic_type, unsigned int size) {
    auto string_to_array_of_type_func = create_string_to_array_of_type_func(generic_type, size);
    auto array_of_type_to_string_func = create_array_of_type_to_string_func(generic_type, size);
    auto array_of_type_serialize_func = create_array_of_type_serialize_func(generic_type, size);
    auto array_of_type_serialized_size_func = create_array_of_type_serialized_size_func(generic_type, size);
    auto array_of_type_deserialize_func = create_array_of_type_deserialize_func(generic_type, size);

    return MetaType("std::array",
                    string_to_array_of_type_func,       // string → array<T, N>
                    array_of_type_to_string_func,       // array<T, N> → string
                    array_of_type_serialize_func,       // array<T, N> → bytes
                    array_of_type_serialized_size_func, // array<T, N> → size in bytes
                    array_of_type_deserialize_func,     // bytes → array<T, N>
                    regex_utils::any_char_greedy, {generic_type, size});
}

std::string create_string_to_unordered_map_func(MetaType key_type, MetaType value_type);
std::string create_unordered_map_to_string_func(MetaType key_type, MetaType value_type);
std::string create_unordered_map_serialize_func(MetaType key_type, MetaType value_type);
std::string create_unordered_map_serialized_size_func(MetaType key_type, MetaType value_type);
std::string create_unordered_map_deserialize_func(MetaType key_type, MetaType value_type);

// NOTE: I want to generailized this for other types of maps not just unoredred in the future
inline MetaType construct_unordered_map_metatype(MetaType key_type, MetaType value_type) {
    auto string_to_map_func = create_string_to_unordered_map_func(key_type, value_type);
    auto map_to_string_func = create_unordered_map_to_string_func(key_type, value_type);
    auto map_serialize_func = create_unordered_map_serialize_func(key_type, value_type);
    auto map_size_func = create_unordered_map_serialized_size_func(key_type, value_type);
    auto map_deserialize_func = create_unordered_map_deserialize_func(key_type, value_type);

    return MetaType("std::unordered_map",
                    string_to_map_func,   // string → map<K, V>
                    map_to_string_func,   // map<K, V> → string
                    map_serialize_func,   // map<K, V> → bytes
                    map_size_func,        // map<K, V> → size in bytes
                    map_deserialize_func, // bytes → map<K, V>
                    regex_utils::any_char_greedy, {key_type, value_type});
}

// NOTE: this is the only global state.
inline std::vector<MetaType> concrete_types = {CHAR,  INT,           UNSIGNED_INT, UINT8_T, UINT32_T, SIZE_T,
                                               FLOAT, DOUBLE,        SHORT,        LONG,    STRING,   FILESYSTEM_PATH,
                                               BOOL,  meta_type_type};

using MetaTemplateParameter = std::variant<unsigned int, MetaType>;

/**
 * @brief an unordered map that takes in a templated type, such as "std::vector" along with a vector of a
 * MetaTemplateParameters and attempts to construct the meta type representing the templated type
 *
 * @note whenever you create a new type that is variably sized you have to add it to some random vector elsewhere which
 * is just insanity
 */
inline std::unordered_map<std::string, std::function<MetaType(std::vector<MetaTemplateParameter>)>>
    generic_type_to_metatype_constructor = {
        // std::vector<T>
        {"std::vector",
         [](std::vector<MetaTemplateParameter> template_parameters) -> MetaType {
             if (template_parameters.size() != 1) {
                 throw std::invalid_argument("std::vector requires exactly 1 template parameter");
             }

             const MetaType &element_type = std::get<MetaType>(template_parameters[0]);
             return construct_vector_metatype(element_type);
         }},

        // std::optional<T>
        {"std::optional",
         [](std::vector<MetaTemplateParameter> template_parameters) -> MetaType {
             if (template_parameters.size() != 1) {
                 throw std::invalid_argument("std::optional requires exactly 1 template parameter");
             }

             const MetaType &element_type = std::get<MetaType>(template_parameters[0]);
             return construct_optional_metatype(element_type);
         }},

        // std::array<T, N>
        {"std::array",
         [](std::vector<MetaTemplateParameter> template_parameters) -> MetaType {
             if (template_parameters.size() != 2) {
                 throw std::invalid_argument("std::array requires exactly 2 template parameters");
             }

             const MetaType &element_type = std::get<MetaType>(template_parameters[0]);
             unsigned int size = std::get<unsigned int>(template_parameters[1]);

             return construct_array_metatype(element_type, size);
         }},

        // std::unordered_map<K, V>
        {"std::unordered_map",
         [](std::vector<MetaTemplateParameter> template_parameters) -> MetaType {
             if (template_parameters.size() != 2) {
                 throw std::invalid_argument("std::unordered_map requires exactly 2 template parameters");
             }

             const MetaType &key_type = std::get<MetaType>(template_parameters[0]);
             const MetaType &value_type = std::get<MetaType>(template_parameters[1]);

             return construct_unordered_map_metatype(key_type, value_type);
         }},
};

inline std::unordered_map<std::string, MetaType> create_type_name_to_meta_type_map(std::vector<MetaType> meta_types) {
    std::unordered_map<std::string, MetaType> map;
    for (const auto &meta_type : meta_types) {
        map.emplace(meta_type.base_type_name, meta_type);
    }
    return map;
}

std::optional<MetaType> parse_meta_type_from_string(const std::string &type_str);

enum class MetaQualifier {
    // using underscore postfix so compiler doesn't freak
    CONST_,
    VOLATILE_,
};

std::string clean_type_string(const std::string &raw_type);
std::vector<MetaQualifier> extract_meta_qualifiers(const std::string &raw_type);

/**
 * @todo I think internally this should just use a meta variable.
 */
class MetaParameter {
  public:
    std::vector<MetaQualifier> qualifiers;
    MetaType type;
    std::string name;

    MetaParameter() = default;
    MetaParameter(const std::string &input);

    bool operator==(const MetaParameter &other) const { return name == other.name && type == other.type; }

    std::string to_code() const {
        std::ostringstream oss;

        // Print qualifiers
        for (MetaQualifier q : qualifiers) {
            switch (q) {
            case MetaQualifier::CONST_:
                oss << "const ";
                break;
            case MetaQualifier::VOLATILE_:
                oss << "volatile ";
                break;
            }
        }

        // Type name
        oss << type.get_type_name();

        // Parameter name
        oss << " " << name;

        return oss.str();
    }

    std::string to_string() const {
        std::ostringstream oss;
        oss << "MetaParameter {\n";
        oss << "  name: " << name << "\n";

        std::string type_str = type.to_string();
        std::istringstream type_iss(type_str);
        std::string line;
        oss << "  type: \n";
        while (std::getline(type_iss, line)) {
            oss << "    " << line << "\n";
        }

        oss << "}";
        return oss.str();
    }
};

std::string generate_regex_to_match_valid_invocation_of_func(const std::string &signature);

template <typename T>
concept HasNameAndNamespace = requires(T a) {
    { a.name } -> std::same_as<std::string &>;
    { a.name_space_this_is_within } -> std::same_as<std::string &>;
};

template <HasNameAndNamespace T> std::string get_fully_qualified_name(const T &obj) {
    if (!obj.name_space_this_is_within.empty()) {
        return obj.name_space_this_is_within + "::" + obj.name;
    }
    return obj.name;
}

class MetaFunctionSignature {
  public:
    std::string name;
    std::string return_type;
    std::string param_list;
    std::vector<MetaParameter> parameters;
    std::string invocation_regex;
    std::string name_space_this_is_within;

    bool operator==(const MetaFunctionSignature &other) const {
        return name == other.name && parameters == other.parameters;
    }

    MetaFunctionSignature() {};

    MetaFunctionSignature(const std::string &input, const std::string &name_space)
        : name_space_this_is_within(name_space) {
        global_logger->info("meta function signature constructor: {}", input);

        auto cleaned_input = text_utils::join_multiline(input);
        std::string s = trim(cleaned_input);

        // --- Try to parse as a regular function ---
        bool parsed_regular = false;
        std::string local_return_type, local_name, local_param_list;

        try {
            // Find parameters
            auto param_end = s.rfind(')');
            if (param_end == std::string::npos) {
                throw std::invalid_argument("Function signature missing closing ')'");
            }

            auto param_start = s.rfind('(', param_end);
            if (param_start == std::string::npos) {
                throw std::invalid_argument("Function signature missing opening '('");
            }

            local_param_list = s.substr(param_start + 1, param_end - param_start - 1);
            std::string ret_and_name = trim(s.substr(0, param_start));

            // Split into return type and name
            auto last_space = ret_and_name.rfind(' ');
            if (last_space == std::string::npos) {
                throw std::invalid_argument("Cannot determine function name and return type");
            }

            local_return_type = trim(ret_and_name.substr(0, last_space));
            local_name = ret_and_name.substr(last_space + 1);

            parsed_regular = true;
        } catch (const std::invalid_argument &) {
            // fallthrough to constructor logic
        }

        if (parsed_regular) {
            return_type = local_return_type;
            name = local_name;
            param_list = local_param_list;
        } else {
            // --- Fall back to constructor logic (unchanged) ---
            std::smatch match;
            static const std::regex constructor_regex(regex_utils::constructor_signature_re);
            if (!std::regex_match(cleaned_input, match, constructor_regex)) {
                throw std::invalid_argument("Invalid function signature format: expected "
                                            "'ReturnType name(Type1 x, Type2 y)' or a constructor");
            } else { // constructor
                name = match[1];
                return_type = get_fully_qualified_name(*this);
                param_list = match[2];
            }
        }

        // --- Common parameter handling ---
        std::vector<std::string> param_tokens = split_comma_separated(param_list);
        for (const std::string &param_str : param_tokens) {
            if (!param_str.empty()) {
                parameters.emplace_back(param_str);
            }
        }

        invocation_regex = generate_regex_to_match_valid_invocation_of_func(cleaned_input);
    }

    std::string to_string() const {
        text_utils::StringAccumulator sa;

        sa.add(return_type, " ", name, "(");
        for (size_t i = 0; i < parameters.size(); ++i) {
            const auto &param = parameters[i];
            sa.add(param.to_code());
            if (i < parameters.size() - 1)
                sa.add(", ");
        }
        sa.add(")");
        return sa.str();
    }

    std::string to_string_repr() const {
        std::ostringstream oss;
        oss << "MetaFunctionSignature {\n";
        oss << "  name: " << name << "\n";
        oss << "  parameters: [\n";
        for (const auto &param : parameters) {
            std::istringstream iss(param.to_string());
            std::string line;
            while (std::getline(iss, line)) {
                oss << "    " << line << "\n";
            }
        }
        oss << "  ]\n";
        oss << "}";
        return oss.str();
    }

  private:
    static std::vector<std::string> split_comma_separated(const std::string &input) {
        std::vector<std::string> result;
        std::string current;

        int angle_depth = 0;
        int paren_depth = 0;
        int bracket_depth = 0;
        int brace_depth = 0;
        bool in_single_quote = false;
        bool in_double_quote = false;

        for (size_t i = 0; i < input.size(); ++i) {
            char c = input[i];

            // handle quotes
            if (c == '\'' && !in_double_quote) {
                in_single_quote = !in_single_quote;
                current.push_back(c);
                continue;
            }
            if (c == '"' && !in_single_quote) {
                in_double_quote = !in_double_quote;
                current.push_back(c);
                continue;
            }

            // if inside quotes, just copy
            if (in_single_quote || in_double_quote) {
                current.push_back(c);
                continue;
            }

            // track nesting
            if (c == '<')
                angle_depth++;
            else if (c == '>' && angle_depth > 0)
                angle_depth--;
            else if (c == '(')
                paren_depth++;
            else if (c == ')' && paren_depth > 0)
                paren_depth--;
            else if (c == '[')
                bracket_depth++;
            else if (c == ']' && bracket_depth > 0)
                bracket_depth--;
            else if (c == '{')
                brace_depth++;
            else if (c == '}' && brace_depth > 0)
                brace_depth--;

            // split only on commas at top-level
            if (c == ',' && angle_depth == 0 && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0) {
                // flush current token
                std::string trimmed = text_utils::trim(current);
                if (!trimmed.empty())
                    result.push_back(trimmed);
                current.clear();
            } else {
                current.push_back(c);
            }
        }

        // flush last token
        std::string trimmed = text_utils::trim(current);
        if (!trimmed.empty())
            result.push_back(trimmed);

        return result;
    }

    static std::string trim(const std::string &s) {
        const auto begin = s.find_first_not_of(" \t\r\n");
        const auto end = s.find_last_not_of(" \t\r\n");
        return (begin == std::string::npos) ? "" : s.substr(begin, end - begin + 1);
    }
};

/**
 * @note it doesn't interally use a MetaType
 *
 */
class MetaVariable {
  public:
    enum class InitStyle { Assignment, Definition, Brace };

    MetaVariable(std::string type, std::string name, std::string value, InitStyle init_style = InitStyle::Assignment,
                 std::string name_space = "")
        : type(std::move(type)), name(std::move(name)), value(std::move(value)), init_style(std::move(init_style)),
          name_space_this_is_within(std::move(name_space)) {}

    std::string type;
    std::string name;
    std::string value;
    InitStyle init_style = InitStyle::Assignment;
    std::string name_space_this_is_within;

    std::string to_assignment() const { return type + " " + name + " = " + value + ";"; }

    std::string to_definition() const { return type + " " + name + "(" + value + ");"; }

    std::string to_brace() const { return type + " " + name + "{" + value + "};"; }

    std::string to_initialization() const {
        switch (init_style) {
        case InitStyle::Assignment:
            return to_assignment();
        case InitStyle::Definition:
            return to_definition();
        case InitStyle::Brace:
            return to_brace();
        }
        return {};
    }
};

class MetaFunction {
  public:
    MetaFunctionSignature signature;
    text_utils::MultilineStringAccumulator body;
    std::string name_space;

    MetaFunction() {};
    MetaFunction(MetaFunctionSignature signature, text_utils::MultilineStringAccumulator body,
                 std::string name_space = "")
        : signature(signature), body(body), name_space(name_space) {};

    MetaFunction(const std::string &func_str, const std::string &name_space = "") : name_space(name_space) {
        // Find the opening '{' that starts the body
        auto body_start = func_str.find('{');
        if (body_start == std::string::npos) {
            throw std::invalid_argument("Function string missing body");
        }

        // Split header and body
        std::string header = func_str.substr(0, body_start);
        auto body_end = func_str.rfind('}');
        std::string body_str = func_str.substr(body_start + 1, body_end - body_start - 1);
        body_str = trim(body_str); // Assuming you have a trim function

        // Add the body to the body object
        body.add_multiline(body_str);

        // --- Parse header right-to-left ---
        header = trim(header);

        // Find the last ')' to get end of parameters
        auto param_end = header.rfind(')');
        if (param_end == std::string::npos) {
            throw std::invalid_argument("Function header missing closing ')'");
        }

        // Find the matching '(' for parameters
        auto param_start = header.rfind('(', param_end);
        if (param_start == std::string::npos) {
            throw std::invalid_argument("Function header missing opening '('");
        }

        // Extract parameter list
        std::string param_list = header.substr(param_start + 1, param_end - param_start - 1);

        // Everything before '(' is "<return_type_and_name>"
        std::string ret_and_name = trim(header.substr(0, param_start));

        // Find the last whitespace in ret_and_name: function name is after it, return type is before it
        auto last_space = ret_and_name.rfind(' ');
        if (last_space == std::string::npos) {
            throw std::invalid_argument("Cannot determine function name and return type");
        }

        std::string func_name = ret_and_name.substr(last_space + 1);
        std::string return_type = trim(ret_and_name.substr(0, last_space));

        std::string header_signature = return_type + " " + func_name + "(" + param_list + ")";

        signature = MetaFunctionSignature(header_signature, name_space);
    }

    std::string to_lambda_string() const {
        std::ostringstream oss;
        oss << "[=](";

        for (size_t i = 0; i < signature.parameters.size(); ++i) {
            const auto &param = signature.parameters[i];
            oss << param.to_code();
            if (i + 1 < signature.parameters.size()) {
                oss << ", ";
            }
        }

        oss << ") -> " << signature.return_type << " {\n" << body.str() << "\n}";
        return oss.str();
    }

    std::string to_string() const {
        std::ostringstream oss;
        oss << signature.return_type << " ";

        if (!name_space.empty()) {
            oss << name_space << "::";
        }

        oss << signature.name << "(";

        for (size_t i = 0; i < signature.parameters.size(); ++i) {
            const auto &param = signature.parameters[i];
            oss << param.to_code();
            if (i + 1 < signature.parameters.size()) {
                oss << ", ";
            }
        }

        oss << ") {\n" << body.str() << "\n}";
        return oss.str();
    }

  private:
    static std::string trim(const std::string &s) {
        const auto begin = s.find_first_not_of(" \t\r\n");
        const auto end = s.find_last_not_of(" \t\r\n");
        return (begin == std::string::npos) ? "" : s.substr(begin, end - begin + 1);
    }
};

enum class AccessSpecifier { Public, Protected, Private };

inline std::string to_string(AccessSpecifier access) {
    switch (access) {
    case AccessSpecifier::Public:
        return "public";
    case AccessSpecifier::Protected:
        return "protected";
    case AccessSpecifier::Private:
        return "private";
    }
    return {};
}

class MetaAttribute {
  public:
    MetaVariable variable;
    AccessSpecifier access = AccessSpecifier::Private;
    bool is_static = false;
    bool is_const = false;

    MetaAttribute(MetaVariable variable, AccessSpecifier access = AccessSpecifier::Private, bool is_static = false,
                  bool is_const = false)
        : variable(std::move(variable)), access(access), is_static(is_static), is_const(is_const) {}

    std::string to_string() const {
        std::ostringstream oss;
        oss << meta_utils::to_string(access) << ":\n";
        oss << (is_static ? "static " : "");
        oss << (is_const ? "const " : "");
        oss << variable.to_initialization();
        return oss.str();
    }
};

class MetaMethod {
  public:
    MetaFunction function;
    AccessSpecifier access = AccessSpecifier::Public;
    bool is_static = false;
    bool is_virtual = false;
    bool is_const = false;
    bool is_override = false;

    MetaMethod() {};
    MetaMethod(MetaFunction function, AccessSpecifier access = AccessSpecifier::Public, bool is_static = false,
               bool is_virtual = false, bool is_const = false, bool is_override = false)
        : function(std::move(function)), access(access), is_static(is_static), is_virtual(is_virtual),
          is_const(is_const), is_override(is_override) {}

    std::string to_string() const {
        std::ostringstream oss;
        oss << meta_utils::to_string(access) << ":\n";

        if (is_static)
            oss << "static ";
        if (is_virtual)
            oss << "virtual ";

        // Normal function signature
        oss << function.signature.return_type << " ";
        if (!function.name_space.empty()) {
            oss << function.name_space << "::";
        }
        oss << function.signature.name << "(";
        for (size_t i = 0; i < function.signature.parameters.size(); ++i) {
            const auto &param = function.signature.parameters[i];
            oss << param.to_code();
            if (i + 1 < function.signature.parameters.size())
                oss << ", ";
        }
        oss << ")";
        if (is_const)
            oss << " const";
        if (is_override)
            oss << " override";

        oss << " {\n" << function.body.str() << "\n}";

        return oss.str();
    }
};

class MetaConstructor {
  public:
    std::string class_name; // store the class name
    std::vector<MetaParameter> parameters;
    std::vector<std::string> initializer_list;
    text_utils::MultilineStringAccumulator body;
    AccessSpecifier access = AccessSpecifier::Public;

    MetaConstructor() = default;

    // Updated constructor takes the class name
    MetaConstructor(const std::string &class_name, const std::vector<MetaParameter> &params,
                    const std::string &body_str, AccessSpecifier access = AccessSpecifier::Public,
                    const std::vector<std::string> &init_list = {})
        : class_name(class_name), parameters(params), initializer_list(init_list), access(access) {
        body.add_multiline(body_str);
    }

    // to_string no longer needs class_name as input
    std::string to_string(size_t indent_level = 1) const {
        std::ostringstream oss;
        std::string pad(indent_level * 4, ' '); // 4 spaces per indent level

        // Constructor signature
        oss << pad << class_name << "(";
        for (size_t i = 0; i < parameters.size(); ++i) {
            const auto &param = parameters[i];
            oss << param.to_code();
            if (i + 1 < parameters.size())
                oss << ", ";
        }
        oss << ")";

        // Initializer list
        if (!initializer_list.empty()) {
            oss << " : ";
            for (size_t i = 0; i < initializer_list.size(); ++i) {
                oss << initializer_list[i];
                if (i + 1 < initializer_list.size())
                    oss << ", ";
            }
        }

        // Body
        oss << " {\n" << text_utils::indent(body.str(), indent_level + 1) << pad << "}\n";
        return oss.str();
    }
};

class MetaEnum {
  public:
    std::string name;
    std::string type;
    std::vector<std::string> enum_names;
};

class MetaClass {
  public:
    std::string name;
    std::string name_space_this_is_within;
    bool is_final = false;

    std::vector<MetaAttribute> attributes;
    std::vector<MetaConstructor> constructors;
    std::vector<MetaMethod> methods;

    MetaClass(std::string name, std::string name_space = "", bool is_final = false)
        : name(std::move(name)), name_space_this_is_within(std::move(name_space)), is_final(is_final) {}

    bool has_any_private_attributes() const {
        for (const auto &attr : attributes) {
            if (attr.access == AccessSpecifier::Private)
                return true;
        }
        return false;
    }

    void add_attribute(const MetaAttribute &attr) { attributes.push_back(attr); }
    void add_method(const MetaMethod &method) {
        auto sig_str = method.function.signature.to_string();

        // check if we already have this signature
        for (const auto &m : methods) {
            if (m.function.signature.to_string() == sig_str) {
                return; // duplicate → do nothing
            }
        }

        methods.push_back(method);
    }

    std::string get_definition() const {
        std::ostringstream oss;

        // NOTE: I decided not to do the namespace here because it should probably be at a file level I think
        // Namespace
        // if (!name_space_this_is_within.empty()) {
        //     oss << "namespace " << name_space_this_is_within << " {\n";
        // }

        // Class header
        oss << "class " << name;
        if (is_final) {
            oss << " final";
        }
        oss << " {\n";

        if (!constructors.empty()) {
            oss << "public:\n";
            for (const auto &ctor : constructors) {
                oss << text_utils::indent(ctor.to_string(), 1); // using the indent_text function
            }
            oss << "\n";
        }

        output_section(oss, AccessSpecifier::Public, attributes, methods);
        output_section(oss, AccessSpecifier::Protected, attributes, methods);
        output_section(oss, AccessSpecifier::Private, attributes, methods);

        oss << "};\n";

        // if (!name_space_this_is_within.empty()) {
        //     oss << "} // namespace " << name_space_this_is_within << "\n";
        // }

        return oss.str();
    }

  private:
    static void output_section(std::ostringstream &oss, AccessSpecifier section,
                               const std::vector<MetaAttribute> &attrs, const std::vector<MetaMethod> &meths) {
        bool has_content = false;

        for (const auto &a : attrs)
            if (a.access == section) {
                has_content = true;
                break;
            }
        if (!has_content) {
            for (const auto &m : meths)
                if (m.access == section) {
                    has_content = true;
                    break;
                }
        }

        if (!has_content)
            return;

        oss << meta_utils::to_string(section) << ":\n";

        for (const auto &attr : attrs) {
            if (attr.access == section) {
                oss << "    " << (attr.is_static ? "static " : "") << (attr.is_const ? "const " : "");

                if (!attr.variable.value.empty()) {
                    oss << attr.variable.to_initialization();
                } else {
                    oss << attr.variable.type << " " << attr.variable.name << ";";
                }

                oss << "\n";
            }
        }

        for (const auto &method : meths) {
            if (method.access == section) {
                oss << "    ";
                if (method.is_static)
                    oss << "static ";
                if (method.is_virtual)
                    oss << "virtual ";

                // constructor special case: no return type
                if (!method.function.signature.return_type.empty())
                    oss << method.function.signature.return_type << " ";

                oss << method.function.signature.name << "(";

                for (size_t i = 0; i < method.function.signature.parameters.size(); ++i) {
                    const auto &param = method.function.signature.parameters[i];
                    oss << param.to_code();
                    if (i + 1 < method.function.signature.parameters.size()) {
                        oss << ", ";
                    }
                }

                oss << ")";
                if (method.is_const)
                    oss << " const";
                if (method.is_override)
                    oss << " override";

                oss << " {\n" << text_utils::indent(method.function.body.str(), 2) << "\n    }\n";
            }
        }
    }
};

enum class FilterMode { None, Whitelist, Blacklist };

bool is_system_header(const std::string &line);
bool is_local_header(const std::string &line);
std::vector<std::string> get_system_headers(const std::vector<std::string> &headers);

// NOTE: this encompasses a header+source file usually
class MetaCodeCollection {
  public:
    std::string name; // used for ifndef guards

    // NOTE: not sure if we need both of these or just one
    std::vector<MetaFunctionSignature> declared_function_signatures_in_header_file;

  public:
    const std::vector<MetaFunction> &get_functions() const { return functions; }
    const std::vector<MetaVariable> &get_variables() const { return variables; }
    const std::vector<MetaClass> &get_classes() const { return classes; }

  private:
    std::vector<MetaFunction> functions;
    std::vector<MetaVariable> variables;
    std::vector<MetaClass> classes;

  public:
    std::vector<std::string> includes_required_for_declaration;
    std::vector<std::string> includes_required_for_definition;
    std::string name_space;

    // startfold code ordering in the file
    enum class HeaderFileCodeOrdering {
        /// the order in which they are added to the mcc is the order in which they will be found in the generated
        /// header file
        AsAdded,
        /// the order will be grouped by function/classes/variables.
        Grouped,
    };

    HeaderFileCodeOrdering header_file_code_ordering = MetaCodeCollection::HeaderFileCodeOrdering::Grouped;

    enum class CppEntityType { Variable, Class, Function };

    struct CppEntityAddition {
        CppEntityType cpp_entity_type;
        size_t index; // index into the corresponding vector
    };

    std::vector<CppEntityAddition> cpp_entity_insertion_order;

    // endfold

    MetaCodeCollection() = default;

    void add_function(MetaFunction mf) {
        mf.name_space = name_space;
        functions.push_back(std::move(mf));
        cpp_entity_insertion_order.push_back({MetaCodeCollection::CppEntityType::Function, functions.size() - 1});
    }

    void add_class(MetaClass mc) {
        mc.name_space_this_is_within = name_space;
        classes.push_back(std::move(mc));
        cpp_entity_insertion_order.push_back({MetaCodeCollection::CppEntityType::Class, classes.size() - 1});
    }

    void add_variable(MetaVariable mv) {
        mv.name_space_this_is_within = name_space;
        variables.push_back(std::move(mv));
        cpp_entity_insertion_order.push_back({MetaCodeCollection::CppEntityType::Variable, variables.size() - 1});
    }

    MetaCodeCollection(const std::string &header_file_path, const std::string &cpp_file_path,
                       const std::vector<std::string> &string_signatures_to_filter_on = {},
                       FilterMode mode = FilterMode::None) {

        std::string header_source = read_file(header_file_path);
        std::string cpp_source = read_file(cpp_file_path);

        name = extract_function_collection_name(header_source).value_or("meta_function_collection");
        extract_includes(header_source, includes_required_for_declaration);
        extract_includes(cpp_source, includes_required_for_definition);

        name_space = extract_top_level_namespace(cpp_source).value_or("");

        // extract_functions(cpp_file_path, name_space, signatures_to_filter_on, mode);
        extract_function_signatures(header_file_path, name_space, string_signatures_to_filter_on, mode);
    }

    void write_to_header_and_source(const std::string &header_file_path, const std::string &cpp_file_path) {
        std::string header_str = generate_header_file_string();
        std::string cpp_str = generate_cpp_file_string();

        fs_utils::create_file_with_content_if_different(header_file_path, header_str);
        fs_utils::create_file_with_content_if_different(cpp_file_path, cpp_str);
    }

    /**
     * @generates a header file based on this meta code collection
     *
     */
    std::string generate_header_file_string() {
        std::ostringstream oss;

        std::string guard_macro = name;
        std::transform(guard_macro.begin(), guard_macro.end(), guard_macro.begin(),
                       [](unsigned char c) { return std::isalnum(c) ? static_cast<char>(std::toupper(c)) : '_'; });
        guard_macro += "_HPP";

        oss << "#ifndef " << guard_macro << "\n";
        oss << "#define " << guard_macro << "\n\n";

        for (const auto &include : includes_required_for_declaration) {
            oss << include << "\n";
        }

        oss << "\n";

        if (!name_space.empty()) {
            oss << "namespace " << name_space << " {\n\n";
        }

        if (header_file_code_ordering == HeaderFileCodeOrdering::Grouped) {
            // TODO: was about to turn the following into a function that takes oss by reference, then check chatgpt for
            // the other one

            for (const auto &var : variables) {
                // NOTE: you can't define vars in the header, so we use extern
                oss << "extern " << var.type << " " << var.name << ";\n";
            }

            oss << "\n";

            for (const auto &cls : classes) {
                oss << cls.get_definition() << "\n\n";
            }

            // NOTE: I don't think we need this loop?
            for (const auto &func : functions) {
                oss << func.signature.return_type << " " << func.signature.name << "(";
                for (size_t i = 0; i < func.signature.parameters.size(); ++i) {
                    const auto &param = func.signature.parameters[i];
                    oss << param.to_code();
                    if (i < func.signature.parameters.size() - 1)
                        oss << ", ";
                }
                oss << ");\n";
            }

            // NOTE: either this or the above is unncessary...
            for (const auto &sig : declared_function_signatures_in_header_file) {
                oss << sig.return_type << " " << sig.name << "(";
                for (size_t i = 0; i < sig.parameters.size(); ++i) {
                    const auto &param = sig.parameters[i];
                    oss << param.to_code();
                    if (i < sig.parameters.size() - 1)
                        oss << ", ";
                }
                oss << ");\n";
            }
        } else if (header_file_code_ordering == HeaderFileCodeOrdering::AsAdded) {
            for (auto &item : cpp_entity_insertion_order) {
                switch (item.cpp_entity_type) {

                case CppEntityType::Variable: {
                    const auto &var = variables[item.index];
                    oss << "extern " << var.type << " " << var.name << ";\n";
                    break;
                }

                case CppEntityType::Class: {
                    const MetaClass &mc = classes[item.index];
                    oss << mc.get_definition() << "\n\n";
                    break;
                }

                case CppEntityType::Function: {
                    MetaFunction &func = functions[item.index];
                    oss << func.signature.return_type << " " << func.signature.name << "(";
                    for (size_t i = 0; i < func.signature.parameters.size(); ++i) {
                        const auto &param = func.signature.parameters[i];
                        oss << param.to_code();
                        if (i < func.signature.parameters.size() - 1)
                            oss << ", ";
                    }
                    oss << ");\n";
                    break;
                }

                default:
                    break;
                }
            }
        }

        if (!name_space.empty()) {
            oss << "\n} // namespace " << name_space << "\n";
        }

        oss << "\n#endif // " << guard_macro << "\n";
        return oss.str();
    }

    std::string generate_cpp_file_string() {
        std::ostringstream oss;

        for (const auto &include : includes_required_for_definition) {
            oss << include << "\n";
        }

        oss << "\n";

        if (!name_space.empty()) {
            oss << "namespace " << name_space << " {\n\n";
        }

        for (const auto &var : variables) {
            oss << var.to_initialization() << "\n";
        }
        oss << "\n";

        for (const auto &func : functions) {
            oss << func.signature.return_type << " " << func.signature.name << "(";
            for (size_t i = 0; i < func.signature.parameters.size(); ++i) {
                const auto &param = func.signature.parameters[i];
                oss << param.to_code();
                if (i < func.signature.parameters.size() - 1)
                    oss << ", ";
            }
            oss << ") {\n";
            oss << func.body.str() << "\n";
            oss << "}\n\n";
        }

        if (!name_space.empty()) {
            oss << "} // namespace " << name_space << "\n";
        }

        return oss.str();
    }

  private:
    static std::string read_file(const std::string &file_path) {
        std::ifstream file(file_path);
        if (!file.is_open()) {
            throw std::runtime_error("Failed to open file: " + file_path);
        }
        std::ostringstream ss;
        ss << file.rdbuf();
        return ss.str();
    }

    static std::optional<std::string> extract_function_collection_name(const std::string &src) {
        std::regex define_guard_regex(R"(^\s*#ifndef\s+([A-Z0-9_]+))");
        std::istringstream stream(src);
        std::string line;

        while (std::getline(stream, line)) {
            std::smatch match;
            if (std::regex_search(line, match, define_guard_regex)) {
                std::string guard_name = match[1];

                std::transform(guard_name.begin(), guard_name.end(), guard_name.begin(),
                               [](unsigned char c) { return std::tolower(c); });

                const std::string suffix = "_hpp";
                if (guard_name.ends_with(suffix)) {
                    guard_name.erase(guard_name.size() - suffix.size());
                }

                return guard_name;
            }
        }

        return std::nullopt;
    }

    // TODO: eventually remove this because we have cpp_parser now
    static void extract_includes(const std::string &src, std::vector<std::string> &out) {
        std::regex include_regex(R"(^\s*(#include\s+["<].*[">]))");
        std::istringstream stream(src);
        std::string line;
        while (std::getline(stream, line)) {
            std::smatch match;
            if (std::regex_search(line, match, include_regex)) {
                out.push_back(match[1]);
            }
        }
    }

    void extract_function_signatures(const std::string &hpp_file_path, const std::string &name_space,
                                     const std::vector<std::string> &string_signatures_to_filter_on, FilterMode mode) {

        bool namespace_wrapped = name_space != "";

        std::vector<std::string> function_declarations =
            cpp_parsing::extract_top_level_function_declarations(hpp_file_path);

        for (const std::string &func_decl : function_declarations) {

            try {

                auto clean_func_decl = [](const std::string &s) -> std::string {
                    auto result = text_utils::trim(s);
                    if (result.back() == ';') {
                        result.pop_back();
                    }
                    result = text_utils::trim(result);
                    text_utils::remove_consecutive_duplicates(result);
                    return result;
                };

                bool match_found = std::any_of(
                    string_signatures_to_filter_on.begin(), string_signatures_to_filter_on.end(),
                    [&](const std::string &fs) { return clean_func_decl(fs) == clean_func_decl(func_decl); });

                bool allowed = (mode == FilterMode::Whitelist)   ? match_found
                               : (mode == FilterMode::Blacklist) ? !match_found
                                                                 : true;

                if (allowed) {
                    // NOTE: we only ever construct a meta function signature if its not blocked, helps deal with the
                    // problem when there is a type which you don't handle yet and you use the filter to explicitly
                    // block it.
                    MetaFunctionSignature mfs(clean_func_decl(func_decl), name_space);
                    declared_function_signatures_in_header_file.push_back(std::move(mfs));
                }
            } catch (const std::exception &e) {
                std::cout << "invalid func\n" << func_decl << std::endl;
            }
        }
    }

    // void extract_functions(const std::string &cpp_file_path, const std::string &name_space,
    //                        const std::vector<std::string> &strings_signatures_to_filter_on, FilterMode mode) {
    //
    //     bool namespace_wrapped = name_space != "";
    //
    //     std::vector<std::string> function_strings = cpp_parsing::extract_top_level_functions(cpp_file_path);
    //
    //     for (const std::string &func_str : function_strings) {
    //
    //         try {
    //             MetaFunction mf(func_str, name_space);
    //             const auto &sig = mf.signature;
    //
    //             bool match_found =
    //                 std::any_of(strings_signatures_to_filter_on.begin(), strings_signatures_to_filter_on.end(),
    //                             [&](const MetaFunctionSignature &fs) { return fs == sig; });
    //
    //             bool allowed = (mode == FilterMode::Whitelist)   ? match_found
    //                            : (mode == FilterMode::Blacklist) ? !match_found
    //                                                              : true;
    //
    //             if (allowed) {
    //                 functions.push_back(std::move(mf));
    //             }
    //         } catch (const std::exception &e) {
    //             std::cout << "invalid func\n" << func_str << std::endl;
    //         }
    //     }
    // }

    std::optional<std::string> extract_top_level_namespace(const std::string &source_code) {
        std::regex namespace_regex(R"(\bnamespace\s+(\w+)\s*\{)");
        std::smatch match;

        if (std::regex_search(source_code, match, namespace_regex)) {
            if (match.size() >= 2) {
                return match[1].str(); // the captured namespace name
            }
        }

        return std::nullopt; // no namespace found
    }
};

class MetaFunctionInvocation {
  public:
    std::string name;
    std::vector<MetaType> argument_types;
};

inline std::string vector_include = "#include <vector>";
inline std::string string_include = "#include <string>";
inline std::string optional_include = "#include <optional>";
inline std::string regex_include = "#include <regex>";

std::string generate_string_invoker_for_function_with_string_return_type(const MetaFunctionSignature &sig);

struct StringToTypeConversions {
    std::vector<std::string> lambda_conversions;
    std::vector<std::string> arg_to_var_conversions;
    std::vector<std::string> arguments_to_final_func_call;
};

/**
 * @brief Generate an invoker function from it's meta function signature
 *
 * This function creates a new function in string form that takes in a string that's supposed to be an invocation of the
 * passed in function and if it is a valid invocation it will call the real internal function
 *
 */
std::string generate_string_invoker_for_function(const MetaFunctionSignature &sig,
                                                 const std ::string &func_postfix = "_string_invoker");

/**
 * @brief Generate a deferred invoker function from it's meta function signature
 *
 * This function creates a new function in string form that takes in a string that's supposed to be an invocation of the
 * passed in function and if it is a valid invocation it will pass back a function that's preloaded to do the requested
 * function call, the point of this is so that we ddon't always have to call a function immediately rather than calling
 * it the moment we get the string for it.
 *
 */
std::string
generate_deferred_string_invoker_for_function(const MetaFunctionSignature &sig,
                                              const std ::string &func_postfix = "_deferred_string_invoker");

std::string generate_string_invoker_for_function_collection_that_has_same_return_type(
    std::vector<MetaFunctionSignature> mfss_with_same_return_type, std::string return_type, std::string func_postfix);

std::string generate_deferred_string_invoker_for_function_collection_that_has_same_return_type(
    std::vector<MetaFunctionSignature> mfss_with_same_return_type, std::string return_type, std::string func_postfix);

struct CustomTypeExtractionSettings {
    CustomTypeExtractionSettings(const std::string &header_file_path,
                                 const std::vector<std::string> &type_names_for_potential_filtering = {},
                                 FilterMode mode = FilterMode::None)
        : header_file_path(header_file_path), type_names_for_potential_filtering(type_names_for_potential_filtering),
          mode(mode) {}
    const std::string header_file_path;
    const std::vector<std::string> type_names_for_potential_filtering;
    meta_utils::FilterMode mode;
};

/**
 * @todo we still can't support types of this form:
 *
 * @startcode
 * class X {
 *  std::unique_ptr<X> child;
 * };
 * @endcode
 *
 * this is because we don't have support for unique ptrs yet and the way we construct classes needs to be updated to
 * allow for recursion by putting in a recursive placeholder or something and then updating its implementation after the
 * fact
 *
 */
void register_custom_types_into_meta_types(const std::vector<CustomTypeExtractionSettings> &settings_list);
void register_custom_types_into_meta_types(const CustomTypeExtractionSettings &custom_type_extraction_settings);

/**
 * the top level invoker is an invoer that return std_string allowing all functions declared in the header to be called
 * as we know what the return type is
 */
struct StringInvokerGenerationSettingsForHeaderSource {
    StringInvokerGenerationSettingsForHeaderSource(
        std::string header_file_path, std::string source_file_path, bool create_top_level_invoker = false,
        bool create_type_grouped_invokers = false,
        std::vector<std::string> string_signatures_for_potential_filtering = {},
        meta_utils::FilterMode mode = meta_utils::FilterMode::None)
        : header_file_path(std::move(header_file_path)), source_file_path(std::move(source_file_path)),
          create_top_level_invoker(create_top_level_invoker),
          create_type_grouped_invokers(create_type_grouped_invokers),
          string_signatures_for_potential_filtering(std::move(string_signatures_for_potential_filtering)), mode(mode) {}
    std::string header_file_path;
    std::string source_file_path;
    bool create_top_level_invoker;
    bool create_type_grouped_invokers;
    const std::vector<std::string> string_signatures_for_potential_filtering;
    meta_utils::FilterMode mode;
};

/**
 * @brief generates the meta program
 *
 * @todo this system is in the very early stages, there are alot of things that are bad about this system, but we can't
 * ignore the fact that it works. Nevertheless I'm going to list the major issues and things we need to do looking ahead
 * here
 *
 * @note one thing that I actively don't like about this is that the meta program that gets generated cannot have a
 * default constructor, this is because the metaclass holds metafunctions of things which are generated upon
 * construction and if not all the meta types are registered then you get an issue during initialization, so we have to
 * find a way to fix this in the future so that the meta program can become a global variable.
 *
 * A flaw currently here is that meta types need to be registered to be used, the reason they need to be registered is
 * so that in this function somewhere internally it will use meta_types to resolve a meta_type from a string (so the
 * system needs to know about the active types currently defined), but at the same time we only generate from_string,
 * ... etc for things that are registered, and so for type sthat are just like std::vector<int> which isn't registered
 * we won't have a to_string for that...
 *
 * another major flaw is that we can't concretely define our to_string functions for non-concrete types before hand,
 * this is true because if you consider something like unordered map, it would be num_types ^ 2 many different functions
 * which is pretty horrible, instead that needs to be a function or something where it takes in two meta types and is
 * albe to use the recursive to_string functions during that function, for every templated type it has to be like this.
 *
 * one option is to do something like this:
 *
 * @startcode
 * #include <string>
 * #include <unordered_map>
 * #include <functional>
 * #include <stdexcept>
 * #include <sstream>
 * #include <vector>
 *
 * // --- Base from_string specializations ---
 * template <typename T>
 * T from_string(const std::string& s);
 *
 * template <>
 * int from_string<int>(const std::string& s) {
 *     return std::stoi(s);
 * }
 *
 * template <>
 * float from_string<float>(const std::string& s) {
 *     return std::stof(s);
 * }
 *
 * template <>
 * bool from_string<bool>(const std::string& s) {
 *     return s == "true" || s == "1";
 * }
 *
 * template <>
 * std::string from_string<std::string>(const std::string& s) {
 *     return s;
 * }
 *
 * // --- Helper: split string by delimiter ---
 * inline std::vector<std::string> split(const std::string& str, char delim) {
 *     std::vector<std::string> tokens;
 *     std::stringstream ss(str);
 *     std::string token;
 *     while (std::getline(ss, token, delim)) {
 *         if (!token.empty())
 *             tokens.push_back(token);
 *     }
 *     return tokens;
 * }
 *
 * // --- The unordered_map specialization ---
 * template <typename K, typename V>
 * std::unordered_map<K, V> from_string(const std::string& s) {
 *     std::unordered_map<K, V> result;
 *
 *     auto pairs = split(s, ',');
 *     for (const auto& pair_str : pairs) {
 *         auto kv = split(pair_str, ':');
 *         if (kv.size() != 2) {
 *             throw std::invalid_argument("Invalid map entry: " + pair_str);
 *         }
 *
 *         K key = from_string<K>(kv[0]);
 *         V value = from_string<V>(kv[1]);
 *         result.emplace(std::move(key), std::move(value));
 *     }
 *
 *     return result;
 * }
 * @endcode
 *
 * Then that allow us to create a generic from_string function for every type by using templates, this is what I like to
 * call a "Baked" solution because you're using the built-in template system to make the logic happen, a different
 * solution might be like this:
 *
 * Instead of using MetaTypes that internally hold the string representation of what they need to do instead, they hold
 * the actual function that does it, then when a MetaType is Templated (ie has at least one Template Parameter), then
 * instead there is a function to create that meta type that takes in those metatypes required and uses that to build
 * the real meta type. In this system you can create custom MetaTypes during runtime and immediately get the
 * from_string, etc... functions, but that requires constructing the type, so we need to decided whether this or the
 * baked version is better.
 *
 * What about "CompiledMetaType"s, like why should we hold the string inside when we could possibly just have the real
 * function, this could be a replacement to MetaType and allow us to dynamically create new MetaTypes that have their
 * own functions defined actively by combining functions of the concrete types etc...
 *
 * still generating metatypes based on source code would still have to be done, but we could use this system to do it as
 * well. It's like we're shoving some of the state only held in non-complied form into the compiled form
 */
void generate_string_invokers_program_wide(std::vector<StringInvokerGenerationSettingsForHeaderSource> settings,
                                           const std::vector<MetaType> &all_types);

MetaCodeCollection
generate_string_invokers_from_header_and_source(const StringInvokerGenerationSettingsForHeaderSource &sigsfhs);

MetaCodeCollection generate_string_invokers_from_header_and_source(
    const std::string &input_header_path, const std::string &input_source_path, bool create_top_level_invoker = false,
    bool create_type_grouped_invokers = false, const std::vector<std::string> &string_signatures = {},
    FilterMode mode = FilterMode::None);

MetaFunction generate_interactive_invoker();

// TODO: this is the meta program thing that we have to pass in at the top level
class MetaTypes {
  private:
    std::vector<MetaType> concrete_types = meta_utils::concrete_types;
    std::unordered_map<std::string, MetaType> concrete_type_name_to_meta_type =
        create_type_name_to_meta_type_map(concrete_types);

    std::unordered_map<std::string, std::function<meta_utils::MetaType(std::vector<MetaTemplateParameter>)>>
        generic_type_to_meta_type_constructor = meta_utils::generic_type_to_metatype_constructor;

  public:
    void add_new_concrete_type(const MetaType &meta_type) {
        concrete_types.push_back(meta_type);
        concrete_type_name_to_meta_type = meta_utils::create_type_name_to_meta_type_map(concrete_types);
    }

    const std::vector<MetaType> &get_concrete_types() const { return concrete_types; }

    const std::unordered_map<std::string, std::function<meta_utils::MetaType(std::vector<MetaTemplateParameter>)>> &
    get_generic_type_to_meta_type_constructor() const {
        return generic_type_to_meta_type_constructor;
    }

    const std::unordered_map<std::string, MetaType> &get_concrete_type_name_to_meta_type() const {
        return concrete_type_name_to_meta_type;
    }
};

// TODO: remove this?
inline MetaTypes meta_types;

MetaType resolve_meta_type(const std::string &type_str, const MetaTypes &types = meta_utils::meta_types);

meta_utils::MetaClass create_meta_class_from_source(const std::string &source);

// NOTE: this creates a class because I didn't feel it was necessary to create a MetaStruct yet, because a struct is
// really a specific type of class, so in the meta world I consider a struct a class, but then if I want to write to
// file we would lose the fact that it's a struct... consider this later when that's an issue.
meta_utils::MetaClass create_meta_struct_from_source(const std::string &source);
meta_utils::MetaType create_meta_type_from_using(const std::string &source, const meta_utils::MetaTypes &types);

/**
 * @brief Constructs a MetaType representation for a concrete class type.
 *
 * This function generates all the serialization, deserialization, and
 * string conversion lambda functions for a given `MetaClass` definition.
 * It uses existing `MetaType` information from the provided `MetaTypes` registry
 * to recursively build conversion logic for each attribute of the class.
 */
meta_utils::MetaType construct_class_metatype(const MetaClass &cls, const MetaTypes &types);

}; // namespace meta_utils

// Helper function to combine hashes (like boost::hash_combine)
inline void hash_combine(std::size_t &seed, std::size_t value) noexcept {
    seed ^= value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

// Forward declare so we can use recursively
namespace std {
template <> struct hash<meta_utils::MetaType> {
    size_t operator()(const meta_utils::MetaType &mt) const noexcept {
        size_t seed = 0;

        // Hash each string member
        hash_combine(seed, std::hash<std::string>{}(mt.base_type_name));
        hash_combine(seed, std::hash<std::string>{}(mt.string_to_type_func_lambda));
        hash_combine(seed, std::hash<std::string>{}(mt.type_to_string_func_lambda));
        hash_combine(seed, std::hash<std::string>{}(mt.literal_regex));

        // Hash vector elements recursively
        for (const auto &elem : mt.template_parameters) {
            std::visit(
                [&](auto &&param) {
                    using T = std::decay_t<decltype(param)>;
                    if constexpr (std::is_same_v<T, unsigned int>) {
                        hash_combine(seed, std::hash<unsigned int>{}(param));
                    } else if constexpr (std::is_same_v<T, meta_utils::MetaType>) {
                        hash_combine(seed, std::hash<meta_utils::MetaType>{}(param));
                    }
                },
                elem);
        }

        return seed;
    }
};

} // namespace std

#endif // META_UTILS_HPP
