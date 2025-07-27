#ifndef META_UTILS_HPP
#define META_UTILS_HPP

#include <optional>
#include <string>
#include <vector>
#include <sstream>
#include "../regex_utils/regex_utils.hpp"
#include "../text_utils/text_utils.hpp"

class MetaVar {
    std::string name;
};

class MetaFunction {
  public:
    std::string name;
    std::string return_type;
    std::vector<std::string> params;
    std::string body;

    // Default constructor
    MetaFunction() = default;

    // Constructor that parses a function string
    explicit MetaFunction(const std::string &cpp_func_str) { parse_from_string(cpp_func_str); }

    // Utility: trim whitespace from both ends of a string
    static std::string trim(const std::string &str) {
        const auto begin = str.find_first_not_of(" \t\n\r");
        const auto end = str.find_last_not_of(" \t\n\r");
        return (begin == std::string::npos || end == std::string::npos) ? "" : str.substr(begin, end - begin + 1);
    }

    void parse_from_string(const std::string &cpp_func_str) {
        static const std::regex func_regex(R"(([\w:<>\s&\*\[\]]+?)\s+(\w+)\s*\(([^)]*)\)\s*\{([\s\S]*)\})",
                                           std::regex::ECMAScript);

        std::smatch match;
        if (!std::regex_search(cpp_func_str, match, func_regex)) {
            throw std::invalid_argument("Invalid function format.");
        }

        return_type = trim(match[1]);
        name = trim(match[2]);

        std::string param_str = trim(match[3]);
        body = trim(match[4]);

        params.clear();
        if (!param_str.empty()) {
            std::stringstream ss(param_str);
            std::string param;
            while (std::getline(ss, param, ',')) {
                params.push_back(trim(param));
            }
        }
    }

    std::string to_lambda_string() const {
        std::ostringstream oss;
        oss << "[=](";
        for (size_t i = 0; i < params.size(); ++i) {
            oss << params[i];
            if (i + 1 < params.size()) {
                oss << ", ";
            }
        }
        oss << ") -> " << return_type << " {\n" << body << "\n}";
        return oss.str();
    }
};

// NOTE: we can't use MetaFunctions in here because we get a cyclic dependency
class MetaType {
  public:
    std::string name;
    std::string string_to_type_func;
    std::string type_to_string_func;
    std::string literal_regex;

    // Optional pointer to underlying element type (e.g., for vector<T>)
    // also note that when something is generic, we talk about the version with nullptr here, concrete types are when
    // this is not a nullptr
    std::vector<MetaType> element_types;

    bool operator==(const MetaType &other) const {
        return name == other.name && string_to_type_func == other.string_to_type_func &&
               type_to_string_func == other.type_to_string_func && literal_regex == other.literal_regex &&
               element_types == other.element_types;
    }

    MetaType() {}
    MetaType(std::string name, std::string string_to_type_func, std::string type_to_string_func,
             std::string literal_regex, const std::vector<MetaType> &element_types = {})
        : name(std::move(name)), string_to_type_func(std::move(string_to_type_func)),
          type_to_string_func(std::move(type_to_string_func)), literal_regex(std::move(literal_regex)),
          element_types(element_types) {}

    std::string to_string() const { return to_string_rec(*this); }

  private:
    std::string to_string_rec(const MetaType &meta_type) const {
        std::ostringstream oss;
        oss << "MetaType {\n";
        oss << "  name: " << meta_type.name << "\n";
        oss << "  string_to_type_func: " << meta_type.string_to_type_func << "\n";
        oss << "  type_to_string_func: " << meta_type.type_to_string_func << "\n";
        oss << "  literal_regex: " << meta_type.literal_regex << "\n";

        if (!meta_type.element_types.empty()) {
            oss << "  element_types: [\n";
            for (const auto &elem : meta_type.element_types) {
                std::string elem_str = to_string_rec(elem);
                std::istringstream iss(elem_str);
                std::string line;
                while (std::getline(iss, line)) {
                    oss << "    " << line << "\n";
                }
            }
            oss << "  ]\n";
        } else {
            oss << "  element_types: []\n";
        }

        oss << "}";
        return oss.str();
    }
};

namespace meta_utils {

inline std::string create_to_string_lambda(std::string type) {
    return "[](const " + type + " &v) { return std::to_string(v); }";
};

// CONCRETE
// NOTE: concrete types have the good property that given a type name, we can do a one to one mapping to one of the
// below concrete types
inline MetaType UNSIGNED_INT =
    MetaType("unsigned int", "[](const std::string &s) { return static_cast<unsigned int>(std::stoul(s)); }",
             create_to_string_lambda("unsigned int"), regex_utils::unsigned_int_regex);

inline MetaType INT = MetaType("int", "[](const std::string &s) { return std::stoi(s); }",
                               create_to_string_lambda("int"), regex_utils::int_regex);

inline MetaType SHORT = MetaType("short", "[](const std::string &s) { return static_cast<short>(std::stoi(s)); }",
                                 create_to_string_lambda("short"), regex_utils::int_regex);

inline MetaType LONG = MetaType("long", "[](const std::string &s) { return std::stol(s); }",
                                create_to_string_lambda("long"), regex_utils::int_regex);

inline MetaType FLOAT = MetaType("float", "[](const std::string &s) { return std::stof(s); }",
                                 create_to_string_lambda("float"), regex_utils::float_regex);

inline MetaType DOUBLE = MetaType("double", "[](const std::string &s) { return std::stod(s); }",
                                  create_to_string_lambda("double"), regex_utils::float_regex);

inline MetaType STRING = MetaType("std::string", "", "", "");

// GENERICS
// NOTE: generic types cannot be defined directly as variables, there are "infinitely" many types possible so we can't
// actually make variables for them, instead we define a function that generates a concrete genric type for a given
// generic type

// NOTE: this function does not yet support recursive vector types (only one layer deep support)
std::string create_string_to_vector_of_type_func(MetaType type_parameter);
std::string create_vector_of_type_to_string_func(MetaType type_parameter);
inline MetaType construct_vector_metatype(MetaType generic_type) {
    auto vector_of_type_to_string_func = create_vector_of_type_to_string_func(generic_type);
    auto string_to_vector_of_generic_type_func = create_string_to_vector_of_type_func(generic_type);
    return MetaType("std::vector", string_to_vector_of_generic_type_func, vector_of_type_to_string_func,
                    regex_utils::any_char_greedy, {generic_type});
};

inline std::vector<MetaType> concrete_types = {INT, UNSIGNED_INT, FLOAT, DOUBLE, SHORT, LONG, STRING};
inline std::unordered_map<std::string, std::function<MetaType(MetaType)>> generic_type_to_metatype_constructor = {
    {"std::vector", [](MetaType mt) -> MetaType { return construct_vector_metatype(mt); }}};

inline std::unordered_map<std::string, MetaType> create_type_name_to_meta_type_map(std::vector<MetaType> meta_types) {
    std::unordered_map<std::string, MetaType> map;
    for (const auto &meta_type : meta_types) {
        map.emplace(meta_type.name, meta_type);
    }
    return map;
}

inline const std::unordered_map<std::string, MetaType> concrete_type_name_to_meta_type =
    create_type_name_to_meta_type_map(concrete_types);
// inline bool is_known_type(const std::string &s) {
//     return std::ranges::any_of(concrete_types, [&](const MetaType &mt) { return mt.name == s; });
// }

std::optional<MetaType>
parse_meta_type_from_string(const std::string &type_str,
                            std::unordered_map<std::string, MetaType> concrete_type_name_to_meta_type =
                                meta_utils::concrete_type_name_to_meta_type);

class MetaParameter {
  public:
    std::string name;
    MetaType type;

    MetaParameter(const std::string &input,
                  const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                      meta_utils::concrete_type_name_to_meta_type) {
        std::istringstream iss(input);
        std::string type_str;
        if (!(iss >> type_str >> name)) {
            throw std::invalid_argument("Invalid MetaParameter input: expected format 'Type Name'");
        }

        type = parse_meta_type_from_string(type_str, concrete_type_name_to_meta_type).value();
    }

    bool operator==(const MetaParameter &other) const { return name == other.name && type == other.type; }

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

class MetaFunctionSignature {
  public:
    std::string name;
    std::vector<MetaParameter> parameters;
    std::string invocation_regex;

    bool operator==(const MetaFunctionSignature &other) const {
        return name == other.name && parameters == other.parameters;
    }

    MetaFunctionSignature() {};
    MetaFunctionSignature(const std::string &input,
                          const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                              meta_utils::concrete_type_name_to_meta_type) {
        static const std::regex signature_regex(R"(^\s*([\w:<>]+)\s+(\w+)\s*\(([^)]*)\)\s*$)");
        std::smatch match;
        if (!std::regex_match(input, match, signature_regex)) {
            throw std::invalid_argument(
                "Invalid function signature format: expected 'ReturnType name(Type1 x, Type2 y)'");
        }

        // match[1] = return type (ignored here)
        name = match[2];

        std::string param_list = match[3];
        std::vector<std::string> param_tokens = split_comma_separated(param_list);

        for (const std::string &param_str : param_tokens) {
            std::string trimmed = MetaFunctionSignature::trim(param_str);
            if (!trimmed.empty()) {
                parameters.emplace_back(trimmed, concrete_type_name_to_meta_type);
            }
        }

        invocation_regex = generate_regex_to_match_valid_invocation_of_func(input);
    }

    std::string to_string() const {
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
        std::istringstream ss(input);
        std::string token;
        while (std::getline(ss, token, ',')) {
            result.push_back(token);
        }
        return result;
    }

    static std::string trim(const std::string &s) {
        const auto begin = s.find_first_not_of(" \t\r\n");
        const auto end = s.find_last_not_of(" \t\r\n");
        return (begin == std::string::npos) ? "" : s.substr(begin, end - begin + 1);
    }
};

class MetaFunctionInvocation {
  public:
    std::string name;
    std::vector<MetaType> argument_types;
};

std::string generate_invoker(const std::string &signature, const std::vector<MetaType> available_types);

}; // namespace meta_utils

#endif // META_UTILS_HPP
