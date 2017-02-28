PROJECT(ftsort)
ADD_EXECUTABLE(ftsort cmpl.c filesort.c ftsort.c gcls.c memsort.c )
INSTALL_FILES(/syspfiles FILES ftsort.par)
INSTALL_TARGETS(/bin ftsort)