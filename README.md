# meta_utils

This is a system which creates a "meta program", a meta program is a reflection system, it allows you to ask questions and do stuff with your existing code. Here are some things it currently does:
- Allows you to take in a string invocation, and actually run the correct function (direct call)
- Allows you to take in a string invocation, and return a function that will do that invocation later on (deferred call)

## TODO
- Autogenerate to_string for all types
- Be able to serialize/deserialize all types


## Gotchas
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
