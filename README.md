# meta_utils


Put this in your cmakelists if you need conditional logic on if the meta program has been generated or not
```cmake
set(GENERATED_META_PROGRAM_HEADER "${CMAKE_SOURCE_DIR}/src/meta_program/meta_program.hpp")

if(EXISTS "${GENERATED_META_PROGRAM_HEADER}" AND NOT IS_DIRECTORY "${GENERATED_META_PROGRAM_HEADER}")
    message(STATUS "Found generated meta_program.hpp, enabling GENERATED_META_PROGRAM")
    target_compile_definitions(${PROJECT_NAME} PRIVATE GENERATED_META_PROGRAM)
endif()
```
