cmake_minimum_required(VERSION 3.22)

cmake_minimum_required(VERSION 3.22)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(GCC_COVERAGE_COMPILE_FLAGS "-Wall -Wextra -Wconversion")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}")
set(CMAKE_CXX_FLAGS_RELEASE "-Ofast -openmp -march=native -Wall -Wextra -Wcast-align")

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
)

find_package(OpenMP REQUIRED)
target_compile_options(ConsoleApplication1 PRIVATE -Wall ${OpenMP_CXX_FLAGS})
target_link_libraries(ConsoleApplication1 PRIVATE OpenMP::OpenMP)

find_package(tabulate CONFIG REQUIRED)
target_link_libraries(ConsoleApplication1 PRIVATE tabulate::tabulate)

#find_package(TBB REQUIRED)
#target_link_libraries(ConsoleApplication1 PUBLIC TBB::tbb)
