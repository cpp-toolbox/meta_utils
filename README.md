# meta_utils

This is a system which creates a "meta program", a meta program is a reflection system, it allows you to ask questions and do stuff with your existing code. Here are some things it currently does:
- Allows you to take in a string invocation, and actually run the correct function (direct call)
- Allows you to take in a string invocation, and return a function that will do that invocation later on (deferred call)
- Automatically generates `to_string`, `from_string`, `serialize`, `deserialize` for custom types that you register

There should be some deliniation between the logic for the meta program, and this, and this is just tools to operate on source code providing you to parse it into a meta format? idk yet this needs more time in the oven before I truly know what it is

## TODO
- Add string invocation for object methods, the function takes in the target object and a string and calls the correct method
- To string functions should have a mode where it can format with newlines, from string will remove all whitespace before parsing back out.
- Add support for templated types function generation, on way to do it is to maintain the current logic, and then improve it by adding "Converters" on top of it. Ie right now there there is no function exsting for std::vector<int> because we'd have to do it for every type subitition which is really stupid, this allows us to hook into the c++ template system. Also this is blocking me from being able to reduce all the copy pasted code in the conversions, once this is figured out then I can do that properly. See: https://chatgpt.com/share/69277554-cc24-8007-ac9f-929b801f8132
- Add support for recursive custom types, eg a class that contains a vector of itself.
- Add ability to extract classes or structs into an external file (eg get this struct and give it its own file)
- Add ability to take the union or intersection of classes.
- Add ability to autogenerate constructors.
- Automatically build constructors and getters.
- Add ability to extract classes or structs into an external file
- Add ability to extract code into a function
- Add ability to extract implementation out of header file into a source file
- Need a solution to the problem where you change your types and it invalidates the current meta program, but there is also another different change that needs to be compiled before that meta program can be regenerated putting you at a deadlock, a potential solution might be one where you can empty all the definitions so any invalid usages of those types are removed, but then what about type name replacemenets? A temporary solution is to comment out everything where possible
- Add ability to convert between equivalent types, if two types are equivalent, then you can show how to map between the two, the reason for automation is that if you enter a new code base you don't have to create conversion between different equivalent types, that glue would already be built, not exactly sure about the specifics of this yet.
- class decorators: https://chatgpt.com/share/6906f0e6-a12c-8007-b366-b496531a8b71
- Add ability to run as a static ECS system where components can be added just like inheritance etc... need to think about that more.
- Add ability to autogenerate constructors.
- Add ability to take the union or intersection of classes.

## Gotchas
- Some types are not able to be reconstructed, one example of this the `std::regex` object, the problem with it is that you construct it by passing a pattern, and so you would expect `from_string` to take in the pattern and construct the regex, which makes sense and is possible, but the `to_string` can't be defined in a way that allows to say that `r = from_string(to_string(r))` as we don't have access to the pattern, thus we cannot define these functions for this type, keep this in mind.
- This system looks at the source code, and creates code based on it, therefore if you make changes to your code the meta program will not update until you re-run the program, that will generate new meta program code, which needs to be compiled to be used.
- One more time, for the meta program to be updated, we follow this paradigm: modify code -> run program -> compile
- You may not want to add the meta program to your git repository as it adds a lot of generated code (if you want to run metrics on code you write, that might matter)

Put this in your cmakelists if you need conditional logic on if the meta program has been generated or not
```cmake
set(GENERATED_META_PROGRAM_HEADER "${CMAKE_SOURCE_DIR}/src/meta_program/meta_program.hpp")

if(EXISTS "${GENERATED_META_PROGRAM_HEADER}" AND NOT IS_DIRECTORY "${GENERATED_META_PROGRAM_HEADER}")
    message(STATUS "Found generated meta_program.hpp, enabling GENERATED_META_PROGRAM")
    target_compile_definitions(${PROJECT_NAME} PRIVATE GENERATED_META_PROGRAM)
endif()
```
