# Component (mission) name. Developers need not change/delete this.
HD_COMPONENT_NAME	= swift

# Software release version number. Developers need not change/delete this.
HD_COMPONENT_VERS	=

# If this directory needs to build a task, list its name here.
HD_CTASK		= xrtpcgrade

# C language source files (.c) to use for the task.
HD_CTASK_SRC_c		= ${HD_CTASK}.c

# F77 language source files (.f) to use for the task.
HD_CTASK_SRC_f		= faintv50.f

# C flags to use in every compilation.
HD_CFLAGS		= ${HD_STD_CFLAGS}

# F77 flags to use in every compilation.
HD_FFLAGS		= ${HD_STD_FFLAGS}

# Library flags to use when linking C task.
HD_CLIBS		= ${HD_LFLAGS} -lswxrt ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

# Tasks to be installed.
HD_INSTALL_TASKS	= ${HD_CTASK}

# Help file(s) to install.
HD_INSTALL_HELP		= ${HD_CTASK}.html

# Parameter file(s) to install.
HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_TEST_PERL_SCRIPTS	= ut_${HD_CTASK}

include ${HD_STD_MAKEFILE}
