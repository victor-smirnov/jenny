
set(CMAKE_C_COMPILER "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/clang")
set(CMAKE_CXX_COMPILER "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/clang++")
set(CMAKE_CXX_STANDARD 20)

add_library(Metalib SHARED hw_metalib.cpp hw_metalib.h)
target_compile_options(Metalib PRIVATE -fintegrated-cc1 -freflection -Wno-language-extension-token)
add_dependencies(Metalib clang)


add_custom_target(TMP0)
add_dependencies(TMP0 Metalib)


add_executable(jenny_hw helloworld8.cpp)
add_dependencies(jenny_hw TMP0)
target_compile_options(jenny_hw PRIVATE -fintegrated-cc1 -freflection -Wno-language-extension-token -jl$<TARGET_FILE:Metalib>)

