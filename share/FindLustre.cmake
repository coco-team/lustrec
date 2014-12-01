# - Find Lustre compiler
# Find the Lustre synchronous language compiler with associated includes path.
# See https://cavale.enseeiht.fr/redmine/projects/lustrec
# This module defines
#  LUSTRE_COMPILER, the lustre compiler
#  LUSTRE_COMPILER_VERSION, the version of the lustre compiler
#  LUSTRE_INCLUDE_DIR, where to find dword.h, etc.
#  LUSTRE_FOUND, If false, Lustre was not found.
# On can set LUSTRE_PATH_HINT before using find_package(Lustre) and the
# module with use the PATH as a hint to find lustrec.
#
# The hint can be given on the command line too:
#   cmake -DLUSTRE_PATH_HINT=/DATA/ERIC/Lustre/lustre-x.y /path/to/source
#
# The module defines some functions:
#   Lustre_Compile([NODE <Lustre Main Node>]
#                  LUS_FILES <Lustre files>
#                  [USER_C_FILES <C files>])
#

if(LUSTRE_PATH_HINT)
  message(STATUS "FindLustre: using PATH HINT: ${LUSTRE_PATH_HINT}")
else()
  set(LUSTRE_PATH_HINT)
endif()

#One can add his/her own builtin PATH.
#FILE(TO_CMAKE_PATH "/DATA/ERIC/Lustre/lustre-x.y" MYPATH)
#list(APPEND LUSTRE_PATH_HINT ${MYPATH})

# FIND_PROGRAM twice using NO_DEFAULT_PATH on first shot
find_program(LUSTRE_COMPILER
  NAMES lustrec
  PATHS ${LUSTRE_PATH_HINT}
  PATH_SUFFIXES bin
  NO_DEFAULT_PATH
  DOC "Path to the Lustre compiler command 'lustrec'")

find_program(LUSTRE_COMPILER
  NAMES lustrec
  PATHS ${LUSTRE_PATH_HINT}
  PATH_SUFFIXES bin
  DOC "Path to the Lustre compiler command 'lustrec'")

if(LUSTRE_COMPILER)
    # get the path where the lustre compiler was found
    get_filename_component(LUSTRE_PATH ${LUSTRE_COMPILER} PATH)
    # remove bin
    get_filename_component(LUSTRE_PATH ${LUSTRE_PATH} PATH)
    # add path to LUSTRE_PATH_HINT
    list(APPEND LUSTRE_PATH_HINT ${LUSTRE_PATH})
    execute_process(COMMAND ${LUSTRE_COMPILER} -version
        OUTPUT_VARIABLE LUSTRE_COMPILER_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    message(STATUS "Lustre compiler version is : ${LUSTRE_COMPILER_VERSION}")
endif(LUSTRE_COMPILER)

find_path(LUSTRE_INCLUDE_DIR
          NAMES arrow.h
          PATHS ${LUSTRE_PATH_HINT}
          PATH_SUFFIXES include/lustrec
          DOC "The Lustre include headers")

# Macros used to compile a lustre library
include(CMakeParseArguments)
function(Lustre_Compile)
  set(options "")
  set(oneValueArgs NODE LIBNAME)
  set(multiValueArgs LUS_FILES USER_C_FILES)
  cmake_parse_arguments(LUS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  if (NOT LUS_LIBNAME)
    message(FATAL_ERROR "You should specify LIBNAME for each Lustre_Compile call.")
  endif()

  if(LUS_NODE)
    set(LUSTRE_NODE_OPT "-node ${LUS_NODE}")
    set(LUSTRE_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/lus_${LUS_LIBNAME}/${LUS_NODE}")
  else()
    set(LUSTRE_NODE_OPT "")
    set(LUSTRE_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/lus_${LUS_LIBNAME}")
  endif()

  file(MAKE_DIRECTORY ${LUSTRE_OUTPUT_DIR})
  set(LUSTRE_GENERATED_FILES)
  foreach(LFILE IN LISTS LUS_LUS_FILES)
    get_filename_component(L ${LFILE} NAME_WE)
    list(APPEND LUSTRE_GENERATED_FILES ${LUSTRE_OUTPUT_DIR}/${L}.h ${LUSTRE_OUTPUT_DIR}/${L}.c)
    add_custom_command(
      OUTPUT ${LUSTRE_GENERATED_FILES}
      COMMAND ${LUSTRE_COMPILER} ${LUSTRE_REAL_OPT} -d ${LUSTRE_OUTPUT_DIR} ${LUS_LUS_FILES}
      DEPENDS ${LUS_LUS_FILES}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      COMMENT "Compile Lustre source(s): ${LUS_LUS_FILES}."
      )
  endforeach()

  set_source_files_properties(${LUSTRE_GENERATED_FILES} PROPERTIES GENERATED TRUE)
  include_directories(${LUSTRE_INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${LUSTRE_OUTPUT_DIR})
  add_library(${LUS_LIBNAME} SHARED
              ${LUSTRE_GENERATED_FILES} ${LUS_USER_C_FILES}
             )
  message(STATUS "Lustre: Added rule for building lustre library: ${LUS_LIBNAME}")
endfunction(Lustre_Compile)

# handle the QUIETLY and REQUIRED arguments and set LUSTRE_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(LUSTRE
                                  REQUIRED_VARS LUSTRE_COMPILER LUSTRE_INCLUDE_DIR)
# VERSION FPHSA options not handled by CMake version < 2.8.2)
#                                  VERSION_VAR LUSTRE_COMPILER_VERSION)
mark_as_advanced(LUSTRE_INCLUDE_DIR)
