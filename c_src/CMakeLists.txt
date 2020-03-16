cmake_minimum_required(VERSION 3.0.0)
project(erbio VERSION 0.1.0)

link_libraries("Ws2_32.lib")

#include(CTest)
#enable_testing()

aux_source_directory(. SRC_LIST)

add_executable(erbio ${SRC_LIST})

#set(CPACK_PROJECT_NAME ${PROJECT_NAME})
#set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
#include(CPack)
