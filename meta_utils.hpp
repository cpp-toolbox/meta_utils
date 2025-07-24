#ifndef META_UTILS_HPP
#define META_UTILS_HPP

#include "meta_types.hpp"
#include "../regex_utils/regex_utils.hpp"

#include "sbpt_generated_includes.hpp"

#include <string>
#include <vector>
#include <sstream>

class HeaderAndSource {};

class Class {};

class Variable {
    std::string name;
    std::string value;
};

class Parameter {
  public:
    Parameter(std::string name, type type, bool pass_by_reference, bool const_)
        : name(std::move(name)), type(std::move(type)), pass_by_reference(pass_by_reference), const_(const_) {}

    std::string name;
    std::string type;
    bool pass_by_reference;
    bool const_;

    std::string to_string() const {
        std::ostringstream oss;
        if (const_)
            oss << "const ";
        oss << type << " ";
        if (pass_by_reference)
            oss << "&";
        oss << name;
        return oss.str();
    }
};

class FunctionSignature {
  public:
    FunctionSignature(std::string name, std::vector<Parameter> parameter_list, type return_type)
        : name(std::move(name)), parameter_list(std::move(parameter_list)), return_type(std::move(return_type)) {}

    std::string name;
    std::vector<Parameter> parameter_list;
    std::string return_type;

    std::string to_string() const {
        std::ostringstream oss;
        oss << return_type << " " << name << "(";
        for (size_t i = 0; i < parameter_list.size(); ++i) {
            if (i > 0)
                oss << ", ";
            oss << parameter_list[i].to_string();
        }
        oss << ");";
        return oss.str();
    }
};

class Function {
  public:
    std::string name;
    std::vector<Parameter> parameter_list;
    std::string body;
    type return_type;

    std::string to_string() const {
        std::ostringstream oss;
        oss << return_type << " " << name << "(";
        for (size_t i = 0; i < parameter_list.size(); ++i) {
            if (i > 0)
                oss << ", ";
            oss << parameter_list[i].to_string();
        }
        oss << ")";
        if (!body.empty()) {
            oss << " {\n" << body << "\n}";
        } else {
            oss << ";";
        }
        return oss.str();
    }
};

std::string generate_invoker(const std::string &signature);

#endif // META_UTILS_HPP
