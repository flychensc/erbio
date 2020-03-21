cmake_minimum_required(VERSION 3.0.0)
project(erbio VERSION 0.1.0)

include_directories("D:/flych/wsl/source/openssl/include")
link_directories("D:/flych/wsl/source/openssl/lib")

link_libraries("libssl")
link_libraries("libcrypto")

# http://openssl.6102.n7.nabble.com/openssl-1-1-0-Linker-error-on-Windows-td66318.html
# https://blog.csdn.net/diaoxuesong/article/details/78664663
link_libraries("Crypt32.lib")
link_libraries("Ws2_32.lib")

#include(CTest)
#enable_testing()

aux_source_directory(. SRC_LIST)

add_executable(erbio ${SRC_LIST})

#set(CPACK_PROJECT_NAME ${PROJECT_NAME})
#set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
#include(CPack)
