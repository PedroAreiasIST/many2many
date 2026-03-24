cmake_minimum_required(VERSION 3.22)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(GCC_COVERAGE_COMPILE_FLAGS "-Wall -Wextra -Wconversion")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -std=c++20 -fopenmp -fbounds-check")
set(CMAKE_CXX_FLAGS_RELEASE " -Ofast -std=c++20 -ffast-math -fno-trapping-math -fassociative-math -fargument-noalias-global -fomit-frame-pointer -fopenmp -march=native -mtune=native  ")
project(ConsoleApplication1)

include(FetchContent)
FetchContent_Declare(
    cereal
    GIT_REPOSITORY https://github.com/USCiLab/cereal.git
    GIT_TAG        v1.3.2
)
set(JUST_INSTALL_CEREAL ON CACHE BOOL "" FORCE)
set(SKIP_PERFORMANCE_COMPARISON ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(cereal)

FetchContent_Declare(
    tbb
    GIT_REPOSITORY https://github.com/oneapi-src/oneTBB.git
    GIT_TAG        v2021.13.0
)
set(TBB_TEST OFF CACHE BOOL "" FORCE)
set(TBB_EXAMPLES OFF CACHE BOOL "" FORCE)
set(TBB_STRICT OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(tbb)

include_directories(pfr_non_boost-master/include
        pfr_non_boost-master/include/pfr
        pfr_non_boost-master/include/pfr/detail
        .
        /usr/local/include)


add_executable(ConsoleApplication1
        main.cpp
        pfr_non_boost-master/include/pfr.hpp
        pfr_non_boost-master/include/pfr/config.hpp
        pfr_non_boost-master/include/pfr/detail/config.hpp
        o2m.hpp
        m2m.hpp
        o2m.cpp
        m2m.cpp
        mm2m.cpp
        symmetries.hpp
        thing.cpp
        thing.hpp
        testm2m.hpp
        testmm2m.hpp
        testeo2m.hpp
        outputtoensight.hpp
        zoo.hpp
)

target_link_libraries(ConsoleApplication1 PRIVATE cereal::cereal)

target_link_libraries(ConsoleApplication1 PUBLIC TBB::tbb)
