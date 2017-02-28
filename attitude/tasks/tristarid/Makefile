HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_SUBDIRS		= StarID

HD_CTASK		= tristarid1

HD_CTASK_SRC_c		= tristarid1.c Analyzer.c Catalog.c \
			EigenvalueDecomposition.c List.c Loader.c Match.c \
			MUtil.c Position.c QMethod.c Solver.c Source.c \
			SourceMap.c SUtil.c Triplet.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lrew ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PERL_SCRIPTS	= tristarid

HD_TEST_PERL_SCRIPTS	= ut-tristarid

HD_TEST_PFILES		= ut-tristarid.par

HD_INSTALL_PFILES	= tristarid.par tristarid1.par

HD_INSTALL_HELP		= tristarid.html tristarid1.html catspec.html

HD_INSTALL_HEADERS	=

include ${HD_STD_MAKEFILE}
