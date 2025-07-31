#ifndef META_UTILS_HPP
#define META_UTILS_HPP

#include <fstream>
#include <iostream>
#include <iterator>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

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
    // also note that when something is generic, we talk about the version with
    // nullptr here, concrete types are when this is not a nullptr
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
// NOTE: concrete types have the good property that given a type name, we can do
// a one to one mapping to one of the below concrete types
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
// NOTE: generic types cannot be defined directly as variables, there are
// "infinitely" many types possible so we can't actually make variables for
// them, instead we define a function that generates a concrete genric type for
// a given generic type

// NOTE: this function does not yet support recursive vector types (only one
// layer deep support)
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
//     return std::ranges::any_of(concrete_types, [&](const MetaType &mt) {
//     return mt.name == s; });
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

        // Match:
        // - leading whitespace
        // - greedy type match that includes optional &, *, &&, const at the end
        // - single space
        // - variable name (word characters)
        // - optional default assignment
        static const std::regex param_re(R"(^\s*(.+?[\s*&]+)\s*(\w+)(\s*=.*)?$)");

        std::smatch match;
        if (!std::regex_match(input, match, param_re)) {
            throw std::invalid_argument("Invalid MetaParameter input: expected format 'Type Name [= default]'. Got: " +
                                        input);
        }

        std::string raw_type_str = match[1];
        name = match[2];

        std::string cleaned_type_str = clean_type_string(raw_type_str);

        auto parsed_type = parse_meta_type_from_string(cleaned_type_str, concrete_type_name_to_meta_type);
        if (!parsed_type.has_value()) {
            throw std::invalid_argument("Failed to parse MetaType from string: '" + cleaned_type_str + "'");
        }

        type = parsed_type.value();
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
    std::string param_list;
    std::vector<MetaParameter> parameters;
    std::string invocation_regex;
    std::string name_space;

    std::string get_fully_qualified_name() const {
        if (!name_space.empty()) {
            return name_space + "::" + name;
        }
        return name;
    }

    bool operator==(const MetaFunctionSignature &other) const {
        return name == other.name && parameters == other.parameters;
    }

    MetaFunctionSignature() {};
    MetaFunctionSignature(const std::string &input, const std::string &name_space,
                          const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                              meta_utils::concrete_type_name_to_meta_type)
        : name_space(name_space) {

        auto cleaned_input = text_utils::join_multiline(input);
        static const std::regex signature_regex(regex_utils::function_signature_re);
        // static const std::regex signature_regexx(
        //     regex_utils::start_of_line + regex_utils::optional_ws +
        //     regex_utils::capture(regex_utils::one_or_more(regex_utils::type_char_class)) +
        //     regex_utils::one_or_more_ws + regex_utils::capture(regex_utils::word) + regex_utils::optional_ws +
        //     regex_utils::wrap_parentheses(
        //         regex_utils::capture(regex_utils::zero_or_more(regex_utils::negated_character_class({")"})))) +
        //     regex_utils::optional_ws + regex_utils::end_of_line);
        std::smatch match;
        if (!std::regex_match(cleaned_input, match, signature_regex)) {

            static const std::regex constructor_regex(regex_utils::constructor_signature_re);
            if (!std::regex_match(cleaned_input, match, constructor_regex)) {
                throw std::invalid_argument("Invalid function signature format: expected "
                                            "'ReturnType name(Type1 x, Type2 y)' or a constructor");
            } else { // constructor
                return_type = match[1];
                name = match[1];
                param_list = match[2];
            }
        } else { // regular function
            return_type = match[1];
            name = match[2];
            param_list = match[3];
        }

        std::vector<std::string> param_tokens = split_comma_separated(param_list);

        for (const std::string &param_str : param_tokens) {
            if (!param_str.empty()) {
                parameters.emplace_back(param_str, concrete_type_name_to_meta_type);
            }
        }

        invocation_regex =
            generate_regex_to_match_valid_invocation_of_func(cleaned_input, concrete_type_name_to_meta_type);
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
    std::string name_space;

    MetaFunction(const std::string &func_str,
                 const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                     meta_utils::concrete_type_name_to_meta_type,
                 const std::string &name_space = "")
        : name_space(name_space) {

        static const std::regex function_regex(R"(([\w:<>]+)\s+(\w+)\s*\(([^)]*)\)\s*\{([\s\S]*)\})");

        std::smatch match;
        if (!std::regex_search(func_str, match, function_regex)) {
            throw std::invalid_argument("Function string is not a valid function definition");
        }

        std::string header_signature = match[1].str() + " " + match[2].str() + "(" + match[3].str() + ")";
        signature = MetaFunctionSignature(header_signature, name_space, concrete_type_name_to_meta_type);

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
        oss << signature.return_type << " ";

        if (!name_space.empty()) {
            oss << name_space << "::";
        }

        oss << signature.name << "(";

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

bool is_system_header(const std::string &line);
bool is_local_header(const std::string &line);
std::vector<std::string> get_system_headers(const std::vector<std::string> &headers);

class MetaFunctionCollection {
  public:
    std::string name;
    std::vector<MetaFunction> functions;
    std::vector<std::string> includes_required_for_declaration;
    std::vector<std::string> includes_required_for_definition;
    std::string name_space;

    MetaFunctionCollection() = default;

    void add_function(MetaFunction mf) { functions.push_back(std::move(mf)); }

    MetaFunctionCollection(const std::string &header_file_path, const std::string &cpp_file_path,
                           const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type =
                               meta_utils::concrete_type_name_to_meta_type,
                           const std::vector<std::string> &string_signatures = {}, FilterMode mode = FilterMode::None) {
        std::string header_source = read_file(header_file_path);
        std::string cpp_source = read_file(cpp_file_path);

        name = extract_function_collection_name(header_source).value_or("meta_function_collection");
        extract_includes(header_source, includes_required_for_declaration);
        extract_includes(cpp_source, includes_required_for_definition);

        name_space = extract_top_level_namespace(cpp_source).value_or("");

        std::vector<MetaFunctionSignature> parsed_signatures;
        for (const auto &str : string_signatures) {
            MetaFunctionSignature mfs(str, name_space, concrete_type_name_to_meta_type);
            parsed_signatures.push_back(mfs);
        }

        extract_functions(cpp_source, name_space, concrete_type_name_to_meta_type, parsed_signatures, mode);
    }

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

    std::string trim(const std::string &str) {
        size_t start = str.find_first_not_of(" \t\n\r");
        if (start == std::string::npos)
            return "";
        size_t end = str.find_last_not_of(" \t\n\r");
        return str.substr(start, end - start + 1);
    }

    bool is_comment_or_empty(const std::string &line) {
        std::string trimmed = trim(line);
        return trimmed.empty() || trimmed.rfind("//", 0) == 0 || trimmed.rfind("/*", 0) == 0 ||
               trimmed.rfind("*", 0) == 0;
    }

    std::string remove_comment_from_line(const std::string &line) {
        size_t pos = line.find("//");
        if (pos != std::string::npos) {
            return line.substr(0, pos);
        }
        return line;
    }

    std::pair<int, int> count_braces_in_line(const std::string &line) {
        int open_braces = 0;
        int close_braces = 0;
        bool in_string = false;
        bool in_char = false;
        bool escaped = false;

        for (size_t j = 0; j < line.length(); ++j) {
            char c = line[j];

            if (escaped) {
                escaped = false;
                continue;
            }

            if (c == '\\') {
                escaped = true;
                continue;
            }

            if (c == '"' && !in_char) {
                in_string = !in_string;
            } else if (c == '\'' && !in_string) {
                in_char = !in_char;
            } else if (!in_string && !in_char) {
                if (c == '{')
                    open_braces++;
                else if (c == '}')
                    close_braces++;
            }
        }

        return {open_braces, close_braces};
    }

    bool is_block_terminator(const std::string &line) {
        std::string cleaned = trim(remove_comment_from_line(line));
        if (cleaned.empty())
            return false;

        return cleaned == "}" || cleaned == "};" || cleaned.rfind("};", cleaned.length() - 2) != std::string::npos;
    }

    bool is_standalone_statement(const std::string &line) {
        std::string cleaned = trim(remove_comment_from_line(line));
        if (cleaned.empty())
            return false;

        // Skip preprocessor directives
        if (cleaned[0] == '#')
            return true;

        // Statements that typically end with semicolon and aren't part of function
        // signatures
        if (cleaned.back() == ';') {
            // But allow function declarations/prototypes
            if (cleaned.find('(') != std::string::npos && cleaned.find(')') != std::string::npos) {
                return false; // Could be function prototype, don't treat as terminator
            }
            return true;
        }

        // Class/struct/namespace definitions
        if (cleaned.rfind("namespace", 0) == 0 || cleaned.rfind("class ", 0) == 0 || cleaned.rfind("struct ", 0) == 0 ||
            cleaned.rfind("enum ", 0) == 0 || cleaned.rfind("typedef", 0) == 0 || cleaned.rfind("using", 0) == 0) {
            return true;
        }

        return false;
    }

    bool looks_like_function_definition(const std::string &line) {
        std::string cleaned = trim(remove_comment_from_line(line));
        if (cleaned.empty())
            return false;

        // Must have an opening brace to be a definition
        if (cleaned.find('{') == std::string::npos)
            return false;

        // Skip variable initializations with braces
        if (cleaned.find('=') != std::string::npos && cleaned.find("operator") == std::string::npos) {
            return false;
        }

        // Skip class/struct/namespace definitions
        if (cleaned.find("class") != std::string::npos || cleaned.find("struct") != std::string::npos ||
            cleaned.find("namespace") != std::string::npos || cleaned.find("enum") != std::string::npos) {
            return false;
        }

        return true;
    }

    std::vector<std::string> extract_top_level_functions(const std::string &source_code,
                                                         bool namespace_wrapped = false) {
        std::vector<std::string> functions;
        std::vector<std::string> lines;
        std::istringstream stream(source_code);
        std::string line;

        while (std::getline(stream, line)) {
            lines.push_back(line);
        }

        std::cout << "Total lines to process: " << lines.size() << std::endl;
        std::cout << "Looking for functions at depth: " << (namespace_wrapped ? 1 : 0) << std::endl;

        int brace_depth = 0;
        int target_depth = namespace_wrapped ? 1 : 0;

        for (size_t i = 0; i < lines.size(); ++i) {
            const std::string &current_line = lines[i];

            // Count braces in current line
            auto [open_braces, close_braces] = count_braces_in_line(current_line);

            // Log brace depth changes
            if (open_braces > 0 || close_braces > 0) {
                std::cout << "Line " << (i + 1) << " (depth " << brace_depth << "): \"" << trim(current_line)
                          << "\" - open:" << open_braces << " close:" << close_braces << std::endl;
            }

            // First apply closing braces
            brace_depth -= close_braces;

            // Check if we found a function definition at the target depth
            if (brace_depth == target_depth && open_braces > 0 && looks_like_function_definition(current_line)) {
                std::cout << "*** POTENTIAL FUNCTION at line " << (i + 1) << " (depth " << brace_depth << ") ***"
                          << std::endl;
                std::cout << "Line content: \"" << trim(current_line) << "\"" << std::endl;

                // Go backwards to collect the complete signature
                std::vector<std::string> signature_lines;
                int sig_start = static_cast<int>(i);

                // Go backwards to find the complete function signature
                int back_index = static_cast<int>(i) - 1;

                std::cout << "Looking backwards for function signature..." << std::endl;

                while (back_index >= 0) {
                    const std::string &back_line = lines[back_index];
                    std::string trimmed_back = trim(back_line);

                    std::cout << "  Checking line " << (back_index + 1) << ": \"" << trimmed_back << "\"" << std::endl;

                    // Stop if we hit a block terminator (end of previous
                    // function/class/etc)
                    if (is_block_terminator(back_line)) {
                        std::cout << "  -> Found block terminator, stopping" << std::endl;
                        break;
                    }

                    // Stop if we hit a standalone statement/declaration
                    if (is_standalone_statement(back_line)) {
                        std::cout << "  -> Found standalone statement, stopping" << std::endl;
                        break;
                    }

                    // Skip comments and empty lines but don't include them
                    if (is_comment_or_empty(back_line)) {
                        std::cout << "  -> Skipping comment/empty line" << std::endl;
                        back_index--;
                        continue;
                    }

                    // Include this line in the signature
                    std::cout << "  -> Including this line in signature" << std::endl;
                    signature_lines.insert(signature_lines.begin(), back_line);
                    sig_start = back_index;

                    back_index--;
                }

                // Add the current line with the opening brace
                signature_lines.push_back(current_line);

                std::cout << "Function signature collected from lines " << (sig_start + 1) << " to " << (i + 1)
                          << std::endl;

                // Build the function text starting with the signature
                std::string function_text;
                for (const auto &sig_line : signature_lines) {
                    function_text += sig_line + "\n";
                }

                // Track remaining open braces from the current line
                int nested_braces = open_braces - close_braces;
                size_t body_index = i + 1;

                std::cout << "Collecting function body, starting with " << nested_braces << " open braces" << std::endl;

                // Collect the function body
                while (body_index < lines.size() && nested_braces > 0) {
                    const std::string &body_line = lines[body_index];
                    function_text += body_line + "\n";

                    // Count braces in body line
                    auto [body_open, body_close] = count_braces_in_line(body_line);
                    nested_braces += (body_open - body_close);

                    if (body_open > 0 || body_close > 0) {
                        std::cout << "  Body line " << (body_index + 1) << ": open=" << body_open
                                  << " close=" << body_close << " nested=" << nested_braces << std::endl;
                    }

                    body_index++;
                }

                std::cout << "Function body collection complete. Nested braces: " << nested_braces << std::endl;

                // Add the function if we collected a complete one
                if (nested_braces == 0 && !function_text.empty()) {
                    // Clean up the function text
                    while (!function_text.empty() && function_text.back() == '\n') {
                        function_text.pop_back();
                    }
                    if (!function_text.empty()) {
                        std::cout << "*** ADDING FUNCTION TO RESULTS ***" << std::endl;
                        functions.push_back(function_text);
                    }
                } else {
                    std::cout << "*** INCOMPLETE FUNCTION (nested_braces=" << nested_braces << "), NOT ADDING ***"
                              << std::endl;
                }

                // Skip ahead past the function we just processed
                i = body_index - 1;
                std::cout << "Skipping ahead to line " << (i + 2) << std::endl;

                // We need to continue with the correct brace depth
                // Since we've processed this function, don't apply the opening braces
                // again
                continue;
            }

            // Apply opening braces to depth
            brace_depth += open_braces;

            if (open_braces > 0 || close_braces > 0) {
                std::cout << "New brace depth: " << brace_depth << std::endl;
            }
        }

        std::cout << "Final brace depth: " << brace_depth << std::endl;
        std::cout << "Total functions found: " << functions.size() << std::endl;

        return functions;
    }

    std::vector<std::string> get_function_strings_from_code(const std::string &file_path,
                                                            bool namespace_wrapped = false) {
        std::ifstream file(file_path);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open file " << file_path << std::endl;
            return {};
        }

        std::stringstream buffer;
        buffer << file.rdbuf();
        return extract_top_level_functions(buffer.str(), namespace_wrapped);
    }

    void extract_functions(const std::string &cpp_source, const std::string &name_space,
                           const std::unordered_map<std::string, MetaType> &concrete_type_name_to_meta_type,
                           const std::vector<MetaFunctionSignature> &filter_signatures, FilterMode mode) {
        bool namespace_wrapped = name_space != "";
        // Extract top-level function strings
        std::vector<std::string> function_strings = extract_top_level_functions(cpp_source, namespace_wrapped);

        for (const std::string &func_str : function_strings) {
            try {
                MetaFunction mf(func_str, concrete_type_name_to_meta_type, name_space);
                const auto &sig = mf.signature;

                bool match_found = std::any_of(filter_signatures.begin(), filter_signatures.end(),
                                               [&](const MetaFunctionSignature &fs) { return fs == sig; });

                bool allowed = (mode == FilterMode::Whitelist)   ? match_found
                               : (mode == FilterMode::Blacklist) ? !match_found
                                                                 : true;

                if (allowed) {
                    functions.push_back(std::move(mf));
                }
            } catch (const std::exception &e) {
                std::cout << "invalid func\n" << func_str << std::endl;
            }
        }
    }

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

std::string generate_string_invoker_for_function_with_string_return_type(const MetaFunctionSignature &sig,
                                                                         const std::vector<MetaType> &available_types);

std::string generate_string_invoker_for_function(const MetaFunctionSignature &sig,
                                                 const std::vector<MetaType> &available_types,
                                                 const std ::string &func_postfix = "_string_invoker");

std::string generate_string_invoker_for_function_collection(const MetaFunctionCollection &mfc);

void generate_string_invokers_from_source_code(const std::string &input_header_path,
                                               const std::string &input_source_path,
                                               const std::vector<meta_utils::MetaType> &extended_types,
                                               bool create_top_level_invoker = false,
                                               const std::vector<std::string> &string_signatures = {},
                                               FilterMode mode = FilterMode::None);

MetaFunction generate_interactive_invoker(std::vector<meta_utils::MetaType> available_types);

}; // namespace meta_utils

#endif // META_UTILS_HPP
