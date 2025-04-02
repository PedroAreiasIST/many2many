cmake_minimum_required(VERSION 3.22)

cmake_minimum_required(VERSION 3.22)
project(ConsoleApplication1)

set(CMAKE_CXX_STANDARD 20)

include_directories(pfr_non_boost-master/include
        pfr_non_boost-master/include/pfr
        pfr_non_boost-master/include/pfr/detail
        /usr/local/include)

add_executable(ConsoleApplication1
        main.cpp
        pfr_non_boost-master/include/pfr.hpp
        pfr_non_boost-master/include/pfr/config.hpp
        pfr_non_boost-master/include/pfr/detail/config.hpp
        one2many.hpp
        many2many.hpp
        one2many.cpp
        many2many.cpp
        test.cpp
        test.hpp
        relationmatrix.cpp
        symmetries.hpp
        thing.cpp
        thing.hpp
)

#find_package(TBB REQUIRED)
#target_link_libraries(ConsoleApplication1 PUBLIC TBB::tbb)
