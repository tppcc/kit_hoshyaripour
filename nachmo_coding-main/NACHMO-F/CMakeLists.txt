# cmake version, project name, language
cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(neural-fortran Fortran)

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# if build type not specified, default to release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()

# handle integer size
if(INT)
  message(STATUS "Configuring build for ${INT}-bit integers")
  add_definitions(-DINT${INT})
else()
  message(STATUS "Configuring build for 32-bit integers")
  add_definitions(-DINT32)
endif()

# handle real size
if(REAL)
  message(STATUS "Configuring build for ${REAL}-bit reals")
  add_definitions(-DREAL${REAL})
else()
  message(STATUS "Configuring build for 32-bit reals")
  add_definitions(-DREAL32)
endif()


# compiler flags for gfortran
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)

message(STATUS "Configuring to build with -fcoarray=single")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcoarray=single")


  if(BLAS)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fexternal-blas ${BLAS}")
    set(LIBS "${LIBS} blas")
    message(STATUS "Configuring build to use BLAS from ${BLAS}")
  endif()
    
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffast-math")
endif()

# compiler flags for ifort
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)

  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -assume byterecl,realloc_lhs -heap-arrays")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")


endif()

# compiler flags for Cray ftn
if(CMAKE_Fortran_COMPILER_ID MATCHES Cray)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -h noomp")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# library to archive (libneural.a)
add_library(neural src/mod_activation.f90 src/mod_io.f90 src/mod_kinds.f90 src/mod_layer.f90 src/mod_network.f90 src/mod_parallel.f90 src/mod_random.f90)

# Remove leading or trailing whitespace
string(REGEX REPLACE "^ | $" "" LIBS "${LIBS}")

# tests
enable_testing()
foreach(execid network_save network_sync set_activation_function)
  add_executable(test_${execid} test/test_${execid}.f90)
  target_link_libraries(test_${execid} neural ${LIBS})
  add_test(test_${execid} bin/test_${execid})
endforeach()
   
foreach(execid NACHMO)
  add_executable(example_${execid} example/example_${execid}.f90)
  target_link_libraries(example_${execid} neural ${LIBS})
  add_test(example_${execid} bin/example_${execid})
endforeach()
