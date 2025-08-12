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
#include <functional>
#include <cstddef> // for size_t

#include "sbpt_generated_includes.hpp"

namespace meta_utils {

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

inline MetaType BOOL = MetaType("bool", "[](const std::string &s) { return s == \"true\" || s == \"1\"; }",
                                create_to_string_lambda("bool"), R"(true|false|1|0)");

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

// NOTE: this is the only global state.
inline std::vector<MetaType> concrete_types = {INT, UNSIGNED_INT, FLOAT, DOUBLE, SHORT, LONG, STRING, BOOL};

inline std::unordered_map<std::string, std::function<MetaType(MetaType)>> generic_type_to_metatype_constructor = {
    {"std::vector", [](MetaType mt) -> MetaType { return construct_vector_metatype(mt); }}};

inline std::unordered_map<std::string, MetaType> create_type_name_to_meta_type_map(std::vector<MetaType> meta_types) {
    std::unordered_map<std::string, MetaType> map;
    for (const auto &meta_type : meta_types) {
        map.emplace(meta_type.base_type_name, meta_type);
    }
    return map;
}

std::optional<MetaType> parse_meta_type_from_string(const std::string &type_str);

std::string clean_type_string(const std::string &raw_type);

class MetaParameter {
  public:
    std::string name;
    MetaType type;

    MetaParameter() = default;
    MetaParameter(const std::string &input);

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
    MetaFunctionSignature(const std::string &input, const std::string &name_space) : name_space(name_space) {

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
                name = match[1];
                return_type = get_fully_qualified_name();
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
            sa.add(param.type.get_type_name(), " ", param.name);
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

class MetaVariable {
  public:
    enum class InitStyle { Assignment, Definition };

    std::string type;
    std::string name;
    std::string value;
    InitStyle init_style = InitStyle::Assignment;

    std::string to_assignment() const { return type + " " + name + " = " + value + ";"; }

    std::string to_definition() const { return type + " " + name + "(" + value + ");"; }

    std::string to_initialization() const {
        if (init_style == InitStyle::Assignment)
            return to_assignment();
        else
            return to_definition();
    }
};

class MetaFunction {
  public:
    MetaFunctionSignature signature;
    text_utils::MultilineStringAccumulator body;
    std::string name_space;

    MetaFunction(MetaFunctionSignature signature, text_utils::MultilineStringAccumulator body,
                 std::string name_space = "")
        : signature(signature), body(body), name_space(name_space) {};

    MetaFunction(const std::string &func_str, const std::string &name_space = "") : name_space(name_space) {

        std::cout << "MetaFunction" << std::endl;
        std::cout << "func_str: " << func_str << std::endl;

        static const std::regex function_regex(R"(([\w:<>]+)\s+(\w+)\s*\(([^)]*)\)\s*\{([\s\S]*)\})");

        std::smatch match;
        if (!std::regex_search(func_str, match, function_regex)) {
            throw std::invalid_argument("Function string is not a valid function definition");
        }

        std::string header_signature = match[1].str() + " " + match[2].str() + "(" + match[3].str() + ")";
        signature = MetaFunctionSignature(header_signature, name_space);

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
    std::string name; // used for ifndef guards
    std::vector<MetaFunction> functions;
    std::vector<MetaVariable> variables;
    std::vector<std::string> includes_required_for_declaration;
    std::vector<std::string> includes_required_for_definition;
    std::string name_space;

    MetaFunctionCollection() = default;

    void add_function(MetaFunction mf) {
        mf.name_space = name_space;
        functions.push_back(std::move(mf));
    }

    MetaFunctionCollection(const std::string &header_file_path, const std::string &cpp_file_path,
                           const std::vector<std::string> &string_signatures = {}, FilterMode mode = FilterMode::None) {

        std::string header_source = read_file(header_file_path);
        std::string cpp_source = read_file(cpp_file_path);

        name = extract_function_collection_name(header_source).value_or("meta_function_collection");
        extract_includes(header_source, includes_required_for_declaration);
        extract_includes(cpp_source, includes_required_for_definition);

        name_space = extract_top_level_namespace(cpp_source).value_or("");

        std::vector<MetaFunctionSignature> parsed_signatures;
        for (const auto &str : string_signatures) {
            MetaFunctionSignature mfs(str, name_space);
            parsed_signatures.push_back(mfs);
        }

        extract_functions(cpp_file_path, name_space, parsed_signatures, mode);
    }

    void write_to_header_and_source(const std::string &header_file_path, const std::string &cpp_file_path) {
        std::string header_str = generate_header_file_string();
        std::string cpp_str = generate_cpp_file_string();

        fs_utils::create_file_with_content(header_file_path, header_str);
        fs_utils::create_file_with_content(cpp_file_path, cpp_str);
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

        if (!name_space.empty()) {
            oss << "namespace " << name_space << " {\n\n";
        }

        for (const auto &var : variables) {
            oss << "extern " << var.type << " " << var.name << ";\n";
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
                oss << param.type.get_type_name() << " " << param.name;
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
    void extract_functions(const std::string &cpp_file_path, const std::string &name_space,
                           const std::vector<MetaFunctionSignature> &filter_signatures, FilterMode mode) {

        bool namespace_wrapped = name_space != "";

        std::vector<std::string> function_strings = cpp_parsing::extract_top_level_functions(cpp_file_path);

        for (const std::string &func_str : function_strings) {

            try {
                MetaFunction mf(func_str, name_space);
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

std::string generate_string_invoker_for_function_with_string_return_type(const MetaFunctionSignature &sig);

std::string generate_string_invoker_for_function(const MetaFunctionSignature &sig,
                                                 const std ::string &func_postfix = "_string_invoker");

std::string generate_string_invoker_for_function_collection(std::vector<MetaFunction> mfs_with_same_return_type,
                                                            std::string return_type, std::string func_postfix);

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

void generate_string_invokers_program_wide(std::vector<StringInvokerGenerationSettingsForHeaderSource> settings);

MetaFunctionCollection
generate_string_invokers_from_header_and_source(const StringInvokerGenerationSettingsForHeaderSource &sigsfhs);

MetaFunctionCollection generate_string_invokers_from_header_and_source(
    const std::string &input_header_path, const std::string &input_source_path, bool create_top_level_invoker = false,
    bool create_type_grouped_invokers = false, const std::vector<std::string> &string_signatures = {},
    FilterMode mode = FilterMode::None);

MetaFunction generate_interactive_invoker();

class MetaTypes {
  private:
    std::vector<MetaType> concrete_types = meta_utils::concrete_types;
    std::unordered_map<std::string, MetaType> concrete_type_name_to_meta_type =
        create_type_name_to_meta_type_map(concrete_types);

    std::unordered_map<std::string, std::function<meta_utils::MetaType(meta_utils::MetaType)>>
        extended_generic_type_to_meta_type_constructor = meta_utils::generic_type_to_metatype_constructor;

  public:
    void add_new_concrete_type(const MetaType &meta_type) {
        concrete_types.push_back(meta_type);
        concrete_type_name_to_meta_type = meta_utils::create_type_name_to_meta_type_map(concrete_types);
    }

    const std::vector<MetaType> &get_concrete_types() const { return concrete_types; }

    const std::unordered_map<std::string, MetaType> &get_concrete_type_name_to_meta_type() const {
        return concrete_type_name_to_meta_type;
    }
};

inline MetaTypes meta_types;

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
        hash_combine(seed, std::hash<std::string>{}(mt.string_to_type_func));
        hash_combine(seed, std::hash<std::string>{}(mt.type_to_string_func));
        hash_combine(seed, std::hash<std::string>{}(mt.literal_regex));

        // Hash vector elements recursively
        for (const meta_utils::MetaType &elem : mt.element_types) {
            hash_combine(seed, std::hash<meta_utils::MetaType>{}(elem));
        }

        return seed;
    }
};

} // namespace std

#endif // META_UTILS_HPP
