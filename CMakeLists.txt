cmake_minimum_required(VERSION 3.22)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(GCC_COVERAGE_COMPILE_FLAGS "-Wall -Wextra -Wconversion")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -std=c++20 -fopenmp -fbounds-check")
set(CMAKE_CXX_FLAGS_RELEASE " -Ofast -std=c++20 -ffast-math -fno-trapping-math -fassociative-math -fargument-noalias-global -fomit-frame-pointer -fopenmp -march=native -mtune=native  ")
project(ConsoleApplication1)

include_directories(pfr_non_boost-master/include
        pfr_non_boost-master/include/pfr
        pfr_non_boost-master/include/pfr/detail
        .
        /usr/local/include
        cereal-1.3.2/include/cereal
        cereal-1.3.2/include/cereal/archives
        cereal-1.3.2/include
)

include(ExternalProject)
ExternalProject_Add(cereal
        GIT_REPOSITORY git@github.com:USCiLab/cereal.git
        PREFIX cereal
        SOURCE_DIR "third-party/cereal"
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND ""
)
include_directories(third-party/cereal/include)

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

