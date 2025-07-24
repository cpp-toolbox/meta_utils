#include "meta_utils.hpp"

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
    std::vector<std::string> regex_groups;
    std::vector<std::string> conversions;
    std::vector<std::string> call_args;

    for (size_t i = 0; i < arg_types.size(); i++) {
        const auto &typ = arg_types[i];
        const auto &name = arg_names[i];
        int group_num = (int)i + 1;

        if (typ == "int" || typ == "short" || typ == "long") {
            regex_groups.push_back("(" + regex_utils::int_regex + ")");
            conversions.push_back("    " + typ + " " + name + " = std::stoi(match[" + std::to_string(group_num) +
                                  "]);");
        } else if (typ == "float" || typ == "double") {
            regex_groups.push_back("(" + regex_utils::float_regex + ")");
            conversions.push_back("    " + typ + " " + name + " = std::stod(match[" + std::to_string(group_num) +
                                  "]);");
        } else if (typ == "std::string") {
            regex_groups.push_back(R"(\"([^\"]*)\")");
            conversions.push_back("    " + typ + " " + name + " = match[" + std::to_string(group_num) + "].str();");
        } else {
            regex_groups.push_back("([^,()]+)");
            conversions.push_back("    // TODO: convert match[" + std::to_string(group_num) + "] to " + typ);
            conversions.push_back("    " + typ + " " + name + "; // Manual conversion needed");
        }

        call_args.push_back(name);
    }

    std::string function_invocation_regex =
        text_utils::join({regex_utils::start_of_line, func_name, "\\(",
                          text_utils::join(regex_groups, regex_utils::optional_ws + "," + regex_utils::optional_ws),
                          "\\)", regex_utils::end_of_line},
                         regex_utils::optional_ws);

    // Compose the final code
    std::ostringstream oss;
    oss << "std::optional<std::string> f(const std::string &input) {\n";
    oss << "    std::regex re(R\"(" << function_invocation_regex << ")\");\n";
    oss << "    std::smatch match;\n";
    oss << "    if (!std::regex_match(input, match, re)) return std::nullopt;\n\n";
    for (const auto &conv : conversions) {
        oss << conv << "\n";
    }
    oss << "\n";
    oss << "    " << return_type << " result = " << func_name << "(" << text_utils::join(call_args, ", ") << ");\n";

    if (return_type == "void") {
        oss << "    return std::string(); // void returns empty string\n";
    } else if (return_type == "std::string") {
        oss << "    return result;\n";
    } else {
        oss << "    return std::to_string(result);\n";
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
