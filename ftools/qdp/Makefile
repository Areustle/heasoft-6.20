HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= cod qdp fig06 fig07
#			  demo hlptex log modqdp rdbqdp

HD_INSTALL_HELP		= cod.hlp cod.dhf

HD_CLEAN		= ${HD_INSTALL_TASKS} cod.dhf

default: task1 task2 task3 task4 cod-dhf

all: default publish

include ${HD_STD_MAKEFILE}

task1:
	${HD_MAKE} cod HD_CTASK=cod HD_CTASK_SRC_f=cod.f

task2:
	${HD_MAKE} qdp HD_CTASK=qdp HD_CTASK_SRC_f=qdp.f

task3:
	${HD_MAKE} fig06 HD_CTASK=fig06 HD_CTASK_SRC_f=fig06.f

task4:
	${HD_MAKE} fig07 HD_CTASK=fig07 HD_CTASK_SRC_f=fig07.f

cod-dhf: cod.hlp
	@rm -f cod.dhf
	@PATH="${HD_BLD_BIN}:${HD_TOP_BIN}:${HD_BLD_LIB}:${HD_TOP_LIB}:${HC_BLD_EXEC_PFX}/lib:${HC_EXEC_PFX}/lib:${PATH}"; \
	LD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_LIB}:${HC_EXEC_PFX}/lib:${HD_LIB}:${LD_LIBRARY_PATH}"; \
	DYLD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_LIB}:${HC_EXEC_PFX}/lib:${HD_LIB}:${DYLD_LIBRARY_PATH}"; \
	export PATH; \
	export LD_LIBRARY_PATH; \
	export DYLD_LIBRARY_PATH; \
	echo "hlpdhf${BSUF} cod.hlp cod.dhf"; \
	hlpdhf${BSUF} cod.hlp cod.dhf
