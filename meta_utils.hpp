#ifndef META_UTILS_HPP
#define META_UTILS_HPP

#include <iostream>
#include <optional>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <stdexcept>
#include <iterator>
#include <unordered_set>

#include "sbpt_generated_includes.hpp"

namespace meta_utils {

class MetaVar {
    std::string name;
};

// NOTE: we can't use MetaFunctions in here because we get a cyclic dependency
class MetaType {
  public:
    std::string base_type_name;
    std::string string_to_type_func;
    std::string type_to_string_func;
    std::string literal_regex;

    // Optional pointer to underlying element type (e.g., for vector<T>)
    // also note that when something is generic, we talk about the version with nullptr here, concrete types are when
    // this is not a nullptr
    std::vector<MetaType> element_types;

    bool operator==(const MetaType &other) const {
        return base_type_name == other.base_type_name && string_to_type_func == other.string_to_type_func &&
               type_to_string_func == other.type_to_string_func && literal_regex == other.literal_regex &&
               element_types == other.element_types;
    }

    MetaType() {}
    MetaType(std::string name, std::string string_to_type_func, std::string type_to_string_func,
             std::string literal_regex, const std::vector<MetaType> &element_types = {})
        : base_type_name(std::move(name)), string_to_type_func(std::move(string_to_type_func)),
          type_to_string_func(std::move(type_to_string_func)), literal_regex(std::move(literal_regex)),
          element_types(element_types) {}

    std::string to_string() const { return to_string_rec(*this); }

    std::string get_type_name() const { return get_type_name_rec(*this); }

  private:
    std::string get_type_name_rec(const MetaType &meta_type) const {
        if (meta_type.element_types.empty()) {
            return meta_type.base_type_name;
        }

        std::ostringstream oss;
        oss << meta_type.base_type_name << "<";

        for (size_t i = 0; i < meta_type.element_types.size(); ++i) {
            if (i > 0) {
                oss << ", ";
            }
            oss << get_type_name_rec(meta_type.element_types[i]);
        }

        oss << ">";
        return oss.str();
    }

    std::string to_string_rec(const MetaType &meta_type) const {
        std::ostringstream oss;
        oss << "MetaType {\n";
        oss << "  name: " << meta_type.base_type_name << "\n";
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

inline MetaType STRING = MetaType("std::string", "[](const std::string &s) { return s; }",
                                  "[](const std::string &s) { return s; }", regex_utils::string_literal);

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
        map.emplace(meta_type.base_type_name, meta_type);
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

std::string clean_type_string(const std::string &raw_type);

class MetaParameter {
  public:
    std::string name;
    MetaType type;

    MetaParameter(const std::string &input,
                  const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                      meta_utils::concrete_type_name_to_meta_type) {

        // std::cout << "MetaParameter start " << input << std::endl;

        std::istringstream iss(input);
        std::vector<std::string> tokens{std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>{}};

        if (tokens.size() < 2) {
            throw std::invalid_argument("Invalid MetaParameter input: expected format 'Type Name'");
        }

        // std::cout << "1" << std::endl;

        name = tokens.back();

        std::string raw_type_str;
        for (size_t i = 0; i < tokens.size() - 1; ++i) {
            if (i > 0)
                raw_type_str += " ";
            raw_type_str += tokens[i];
        }

        // std::cout << "2" << std::endl;

        // std::cout << "raw_type_str: " << raw_type_str << std::endl;
        std::string cleaned_type_str = clean_type_string(raw_type_str);
        // std::cout << "cleaned_type_str: " << cleaned_type_str << std::endl;

        // std::cout << "3" << std::endl;

        auto parsed_type = parse_meta_type_from_string(cleaned_type_str, concrete_type_name_to_meta_type);
        if (!parsed_type.has_value()) {
            throw std::invalid_argument("Failed to parse MetaType from string: '" + cleaned_type_str + "'");
        }

        // std::cout << "4" << std::endl;

        type = parsed_type.value();

        // std::cout << "5" << std::endl;

        // std::cout << "MetaParameter end " << input << std::endl;
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

std::string generate_regex_to_match_valid_invocation_of_func(
    const std::string &signature, const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                                      meta_utils::concrete_type_name_to_meta_type);

class MetaFunctionSignature {
  public:
    std::string name;
    std::string return_type;
    std::vector<MetaParameter> parameters;
    std::string invocation_regex;

    bool operator==(const MetaFunctionSignature &other) const {
        return name == other.name && parameters == other.parameters;
    }

    MetaFunctionSignature() {};
    MetaFunctionSignature(const std::string &input,
                          const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                              meta_utils::concrete_type_name_to_meta_type) {

        // std::cout << "MetaFunctionSignature start " << input << std::endl;
        // std::cout << "1" << std::endl;
        static const std::regex signature_regex(R"(^\s*([\w:<>]+)\s+(\w+)\s*\(([^)]*)\)\s*$)");
        std::smatch match;
        if (!std::regex_match(input, match, signature_regex)) {
            throw std::invalid_argument(
                "Invalid function signature format: expected 'ReturnType name(Type1 x, Type2 y)'");
        }

        // std::cout << "2" << std::endl;

        return_type = match[1];
        name = match[2];

        // std::cout << "3" << std::endl;

        std::string param_list = match[3];
        std::vector<std::string> param_tokens = split_comma_separated(param_list);

        for (const std::string &param_str : param_tokens) {
            std::string trimmed = MetaFunctionSignature::trim(param_str);
            if (!trimmed.empty()) {
                parameters.emplace_back(trimmed, concrete_type_name_to_meta_type);
            }
        }

        // std::cout << "4" << std::endl;

        invocation_regex = generate_regex_to_match_valid_invocation_of_func(input, concrete_type_name_to_meta_type);

        // std::cout << "5" << std::endl;

        // std::cout << "MetaFunctionSignature end " << input << std::endl;
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

class MetaFunction {
  public:
    MetaFunctionSignature signature;
    text_utils::MultilineStringAccumulator body;

    MetaFunction(const std::string &func_str,
                 const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                     meta_utils::concrete_type_name_to_meta_type) {

        static const std::regex function_regex(R"(([\w:<>]+)\s+(\w+)\s*\(([^)]*)\)\s*\{([\s\S]*)\})");

        std::smatch match;
        if (!std::regex_search(func_str, match, function_regex)) {
            throw std::invalid_argument("Function string is not a valid function definition");
        }

        std::string header_signature = match[1].str() + " " + match[2].str() + "(" + match[3].str() + ")";
        signature = MetaFunctionSignature(header_signature, concrete_type_name_to_meta_type);

        std::string trimmed_body = MetaFunction::trim(match[4]);
        body.add_multiline(trimmed_body);
    }

    std::string to_lambda_string() const {
        std::ostringstream oss;
        oss << "[=](";

        for (size_t i = 0; i < signature.parameters.size(); ++i) {
            const auto &param = signature.parameters[i];
            oss << param.type.get_type_name() << " " << param.name;
            if (i + 1 < signature.parameters.size()) {
                oss << ", ";
            }
        }

        oss << ") -> " << signature.return_type << " {\n" << body.str() << "\n}";
        return oss.str();
    }

    std::string to_string() const {
        std::ostringstream oss;
        oss << signature.return_type << " " << signature.name << "(";

        for (size_t i = 0; i < signature.parameters.size(); ++i) {
            const auto &param = signature.parameters[i];
            oss << param.type.get_type_name() << " " << param.name;
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

enum class FilterMode { None, Whitelist, Blacklist };

// TODO: was about to add the white/blacklist logic now

class MetaFunctionCollection {
  public:
    std::string name;
    std::vector<MetaFunction> functions;
    std::vector<std::string> includes_required_for_declaration;
    std::vector<std::string> includes_required_for_definition;

    MetaFunctionCollection() {};

    void add_function(MetaFunction mf) { functions.push_back(mf); }

    MetaFunctionCollection(const std::string &header_file_path, const std::string &cpp_file_path,
                           const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                               meta_utils::concrete_type_name_to_meta_type,
                           const std::unordered_set<std::string> &signature_set = {},
                           FilterMode mode = FilterMode::None) {
        std::string header_source = read_file(header_file_path);
        std::string cpp_source = read_file(cpp_file_path);

        name = extract_function_collection_name(header_source).value();
        extract_includes(header_source, includes_required_for_declaration);
        extract_includes(cpp_source, includes_required_for_definition);
        extract_functions(cpp_source, concrete_type_name_to_meta_type);
    }

    std::string generate_header_file_string() {
        std::ostringstream oss;

        // Assume this->name exists and is a valid string identifier
        std::string guard_macro = name;
        std::transform(guard_macro.begin(), guard_macro.end(), guard_macro.begin(), [](unsigned char c) {
            if (std::isalnum(c))
                return static_cast<char>(std::toupper(c));
            else
                return '_';
        });
        guard_macro += "_HPP";

        oss << "#ifndef " << guard_macro << "\n";
        oss << "#define " << guard_macro << "\n\n";

        for (const auto &include : includes_required_for_declaration) {
            oss << include << "\n";
        }

        oss << "\n";

        for (const auto &func : functions) {
            oss << func.signature.return_type << " " << func.signature.name << "(";
            for (size_t i = 0; i < func.signature.parameters.size(); ++i) {
                const auto &param = func.signature.parameters[i];
                oss << param.type.get_type_name() << " " << param.name;
                if (i < func.signature.parameters.size() - 1)
                    oss << ", ";
            }
            oss << ");\n";
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

        for (const auto &func : functions) {
            oss << func.signature.return_type << " " << func.signature.name << "(";
            for (size_t i = 0; i < func.signature.parameters.size(); ++i) {
                const auto &param = func.signature.parameters[i];
                oss << param.type.get_type_name() << " " << param.name;
                if (i < func.signature.parameters.size() - 1)
                    oss << ", ";
            }
            oss << ") {\n";
            oss << func.body.str() << "\n";
            oss << "}\n\n";
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

                // Convert to lowercase
                std::transform(guard_name.begin(), guard_name.end(), guard_name.begin(),
                               [](unsigned char c) { return std::tolower(c); });

                // Remove trailing "_hpp" if present
                const std::string suffix = "_hpp";
                if (guard_name.size() >= suffix.size() &&
                    guard_name.compare(guard_name.size() - suffix.size(), suffix.size(), suffix) == 0) {
                    guard_name.erase(guard_name.size() - suffix.size());
                }

                return guard_name;
            }
        }

        return std::nullopt;
    }

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

    void extract_functions(const std::string &cpp_source,
                           const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type) {
        std::regex func_regex(R"(([\w:<>]+)\s+(\w+)\s*\(([^)]*)\)\s*\{([\s\S]*?)\})");
        auto begin = std::sregex_iterator(cpp_source.begin(), cpp_source.end(), func_regex);
        auto end = std::sregex_iterator();

        for (auto it = begin; it != end; ++it) {
            // std::cout << "in loop" << std::endl;
            try {
                std::string whole_function = it->str();
                functions.emplace_back(whole_function, concrete_type_name_to_meta_type);
            } catch (const std::exception &e) {
                // optionally log and skip
            }
        }
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

std::string generate_string_invoker_for_function_with_string_return_type(const MetaFunctionSignature &sig,
                                                                         const std::vector<MetaType> &available_types);
std::string generate_string_invoker_for_function(const MetaFunctionSignature &sig,
                                                 const std::vector<MetaType> &available_types);

std::string generate_string_invoker_for_function_collection(const MetaFunctionCollection &mfc);

void generate_string_invokers_from_source_code(const std::string &input_header_path,
                                               const std::string &input_source_path,
                                               const std::vector<meta_utils::MetaType> &extended_types,
                                               bool create_top_level_invoker = false);

}; // namespace meta_utils

#endif // META_UTILS_HPP
