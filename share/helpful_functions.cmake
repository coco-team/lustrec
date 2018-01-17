

#helpful functions and macros
MACRO(SUBDIRLIST result curdir REGEX_PREFIX)
  FILE(GLOB children RELATIVE ${curdir} ${curdir}/${REGEX_PREFIX}*)
  SET(dirlist "")
  FOREACH(child ${children})
    IF(IS_DIRECTORY ${curdir}/${child})
      LIST(APPEND dirlist ${child})
    ENDIF()
  ENDFOREACH()
  SET(${result} ${dirlist})
ENDMACRO()

MACRO(LUSTREFILES result dir)
  FILE(GLOB children ${dir} ${dir}/*.lus)
  SET(lustreFileslist "")
  FOREACH(child ${children})
    IF(EXISTS ${child} AND NOT IS_DIRECTORY ${child})
      LIST(APPEND lustreFileslist ${child})
    ENDIF()
  ENDFOREACH()
  SET(${result} ${lustreFileslist})
ENDMACRO()


function(JOIN VALUES GLUE OUTPUT)
  string (REGEX REPLACE "([^\\]|^);" "\\1${GLUE}" _TMP_STR "${VALUES}")
  string (REGEX REPLACE "[\\](.)" "\\1" _TMP_STR "${_TMP_STR}") #fixes escaping
  set (${OUTPUT} "${_TMP_STR}" PARENT_SCOPE)
endfunction()




function(CUT_OPTIONS ZUS_OPTS OUTPUT)
  string(REPLACE "--" "_" ZUS_OPTS_CUT ${ZUS_OPTS})
  string(REPLACE "-" "" ZUS_OPTS_CUT ${ZUS_OPTS_CUT})
  string(REPLACE " " "_" ZUS_OPTS_CUT ${ZUS_OPTS_CUT})
  set (${OUTPUT} "${ZUS_OPTS_CUT}" PARENT_SCOPE)
endfunction()

function(get_lustre_name_ext LUS_FILE L E)
  # Extraction name + extension from ${LUS_LUS_FILE} as L and E. E is the smallest extension
  get_filename_component(N ${LUS_FILE} NAME)
  string(REPLACE "." ";" N ${N})
  set(N "${N}")
  list(LENGTH N len)
  math(EXPR idx "${len}-1") 
  list(GET N "${idx}" "${E}")
  set(L "${N}")
  list(REMOVE_AT L "${idx}")
  string(REPLACE ";" "." L "${L}")
#  message("Extraction file ${LUS_FILE} as ${L} and ${E}")
  set(L "${L}" PARENT_SCOPE)
  set(E "${E}" PARENT_SCOPE)
endfunction()
