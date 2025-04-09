cmake_minimum_required(VERSION 3.22)

cmake_minimum_required(VERSION 3.22)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(GCC_COVERAGE_COMPILE_FLAGS "-Wall -Wextra -Wconversion")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -std=c++20")
set(CMAKE_CXX_FLAGS_RELEASE " -Ofast -std=c++20 -ltbb -fargument-noalias-global -fomit-frame-pointer -fopenmp -march=native -flto ")

project(ConsoleApplication1)

include_directories(pfr_non_boost-master/include
        pfr_non_boost-master/include/pfr
        pfr_non_boost-master/include/pfr/detail
        /usr/local/include
        /home/pedro/vcpkg-clion/packages/tabulate_x64-linux/include
)

add_executable(ConsoleApplication1
        main.cpp
        pfr_non_boost-master/include/pfr.hpp
        pfr_non_boost-master/include/pfr/config.hpp
        pfr_non_boost-master/include/pfr/detail/config.hpp
        o2m.hpp
        m2m.hpp
        o2m.cpp
        m2m.cpp
        test.cpp
        test.hpp
        mm2m.cpp
        symmetries.hpp
        thing.cpp
        thing.hpp
        testm2m.hpp
        testmm2m.hpp
        testeo2m.hpp
)

find_package(TBB REQUIRED)
target_link_libraries(ConsoleApplication1 PUBLIC TBB::tbb)
