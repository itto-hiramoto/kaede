cmake_minimum_required(VERSION 3.10)
project(kaede_runtime C)

# Set C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Source files
set(SOURCES
    src/mutex.c
    src/waitgroup.c
    src/task.c
    src/worker.c
    src/runtime.c
)

if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(aarch64|arm64)$")
    list(APPEND SOURCES src/arch/aarch64.c)
else()
    list(APPEND SOURCES src/arch/x86_64.c)
endif()

# Include directories
include_directories(
    include/
    ${CMAKE_CURRENT_SOURCE_DIR}/../bdwgc/include
)

# Create executable
add_library(kaede_runtime STATIC ${SOURCES})

# pthreads (needed by mn.c and for mutex protection)
find_package(Threads REQUIRED)
target_link_libraries(kaede_runtime PRIVATE Threads::Threads)
target_compile_definitions(kaede_runtime PRIVATE GC_THREADS GC_ALLOW_REGISTER_THREADS)

set_target_properties(kaede_runtime PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Set compiler flags for x86_64 assembly
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(kaede_runtime PRIVATE -Wall -Wextra)
endif()
