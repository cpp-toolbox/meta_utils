#include "meta_utils.hpp"
#include "meta_types.hpp"
#include "../glm_utils/glm_utils.hpp"

#include <iostream>
#include <string>
#include <regex>
#include <vector>
#include <sstream>
#include <stdexcept>

std::string trim(const std::string &s) {
    size_t start = s.find_first_not_of(" \t\n\r");
    if (start == std::string::npos)
        return "";
    size_t end = s.find_last_not_of(" \t\n\r");
    return s.substr(start, end - start + 1);
}

std::vector<std::string> split_args(const std::string &args_str) {
    std::vector<std::string> args;
    std::string current;
    int depth = 0;
    for (char c : args_str) {
        if (c == ',' && depth == 0) {
            args.push_back(trim(current));
            current.clear();
        } else {
            if (c == '<' || c == '(')
                depth++;
            else if (c == '>' || c == ')')
                depth--;
            current += c;
        }
    }
    if (!current.empty())
        args.push_back(trim(current));
    return args;
}

// Generates C++ code for invoker function f()
std::string generate_invoker(const std::string &signature) {
    // Parse signature: return_type func_name(arg_type arg_name, ...)
    std::smatch match;
    if (!std::regex_match(signature, match, regex_utils::function_signature_re)) {
        throw std::invalid_argument("Invalid function signature");
    }

    std::string return_type = match[1];
    std::string func_name = match[2];
    std::string args_str = match[3];

    auto args = split_args(args_str);

    std::vector<std::string> arg_types;
    std::vector<std::string> arg_names;

    for (auto &arg : args) {
        if (arg.empty())
            continue;
        size_t space_pos = arg.rfind(' ');
        if (space_pos == std::string::npos) {
            throw std::invalid_argument("Each argument must have type and name");
        }
        arg_types.push_back(arg.substr(0, space_pos));
        arg_names.push_back(arg.substr(space_pos + 1));
    }

    // Build regex pattern for argument parsing
    std::vector<std::string> argument_literal_regexes;
    std::vector<std::string> arg_to_var_conversions;
    std::vector<std::string> call_to_actual_func_args;

    struct TypeConversionData {
        std::string string_to_type_func;
        std::string type_to_string_func;
        std::string literal_regex;
    };

    std::unordered_map<std::string, std::string> extended_type_to_regex = regex_utils::type_to_regex;
    extended_type_to_regex["glm::vec3"] = regex_utils::float_triplet;

    std::string to_string_func = "std::to_string";
    auto get_re = [&](std::string type) { return extended_type_to_regex.at(type); };

    // NOTE: all of these functions take in a string and return the correct type
    std::unordered_map<std::string, TypeConversionData> type_to_type_conversion_data = {
        {types::INT, {"std::stoi", to_string_func, get_re(types::INT)}},
        {types::SHORT, {"std::stoi", to_string_func, get_re(types::SHORT)}},
        {types::LONG, {"std::stoi", to_string_func, get_re(types::LONG)}},
        {types::FLOAT, {"std::stod", to_string_func, get_re(types::FLOAT)}},
        {types::DOUBLE, {"std::stod", to_string_func, get_re(types::DOUBLE)}},
        {types::STRING, {"", "", ""}},
        {"glm::vec3", {"glm_utils::parse_vec3", "vec3_to_string", get_re("glm::vec3")}}};

    for (size_t i = 0; i < arg_types.size(); i++) {
        const auto &typ = arg_types[i];
        const auto &name = arg_names[i];
        int group_num = (int)i + 1;

        TypeConversionData type_conversion_data = type_to_type_conversion_data.at(typ);

        std::string variable_assigment_to_conversion = "    " + typ + " " + name + " = " +
                                                       type_conversion_data.string_to_type_func + "(match[" +
                                                       std::to_string(group_num) + "]);";

        arg_to_var_conversions.push_back(variable_assigment_to_conversion);
        argument_literal_regexes.push_back(regex_utils::capture(type_conversion_data.literal_regex));

        call_to_actual_func_args.push_back(name);
    }

    std::string function_invocation_regex = text_utils::join(
        {regex_utils::start_of_line, func_name, "\\(",
         text_utils::join(argument_literal_regexes, regex_utils::optional_ws + "," + regex_utils::optional_ws), "\\)",
         regex_utils::end_of_line},
        regex_utils::optional_ws);

    // Compose the final code
    std::ostringstream oss;
    oss << "std::optional<std::string> f(const std::string &input) {\n";
    oss << "    std::regex re(R\"(" << function_invocation_regex << ")\");\n";
    oss << "    std::smatch match;\n";
    oss << "    if (!std::regex_match(input, match, re)) return std::nullopt;\n\n";
    for (const auto &conv : arg_to_var_conversions) {
        oss << conv << "\n";
    }
    oss << "\n";
    oss << "    " << return_type << " result = " << func_name << "(" << text_utils::join(call_to_actual_func_args, ", ")
        << ");\n";

    if (return_type == "void") {
        oss << "    return std::string(); // void returns empty string\n";
    } else if (return_type == "std::string") {
        oss << "    return result;\n";
    } else {
        std::string return_type_to_string_func = type_to_type_conversion_data.at(return_type).type_to_string_func;
        oss << "    return " << return_type_to_string_func << "(result);\n";
    }
    oss << "}\n";

    return oss.str();
}

// Helper join function for vector<string>
std::string join(const std::vector<std::string> &v, const std::string &sep) {
    std::string res;
    for (size_t i = 0; i < v.size(); i++) {
        res += v[i];
        if (i + 1 < v.size())
            res += sep;
    }
    return res;
}

// int main() {
//     std::string signature = "int add(int x, int y)";
//     try {
//         std::string generated_code = generate_invoker(signature);
//         std::cout << "Generated invoker function:\n\n" << generated_code << "\n";
//     } catch (const std::exception &e) {
//         std::cerr << "Error: " << e.what() << "\n";
//     }
// }
