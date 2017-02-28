HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= dhfhlp dhfshf hlpdhf mkhelp xhelp

HD_INSTALL_HELP		= xhelp.dhf

HD_CLEAN		= ${HD_INSTALL_TASKS} xhelp.dhf

default: task1 task2 task3 task4 task5 make-xhelp-dhf

all: default publish

include ${HD_STD_MAKEFILE}

# unused: hlptohtmlv3.c mkhelp.opt hlptohtml.html hlptohtml.lis fcheck.f

task1:
	${HD_MAKE} dhfhlp HD_CTASK=dhfhlp HD_CTASK_SRC_f=dhfhlp.f

task2:
	${HD_MAKE} dhfshf HD_CTASK=dhfshf HD_CTASK_SRC_f=dhfshf.f

task3:
	${HD_MAKE} hlpdhf HD_CTASK=hlpdhf HD_CTASK_SRC_f=hlpdhf.f

task4:
	${HD_MAKE} mkhelp HD_CTASK=mkhelp HD_CTASK_SRC_f="mkhelp.f bytobl.f gettop.f"

task5:
	${HD_MAKE} xhelp HD_CTASK=xhelp HD_CTASK_SRC_f=xhelp.f

#task6:
#	${HD_MAKE} hlptohtml HD_CTASK=hlptohtml HD_CTASK_SRC_f=hlptohtml.f

make-xhelp-dhf: xhelp.shf
	@rm -f xhelp.dhf
	@PATH="${HD_BLD_BIN}:${HD_TOP_BIN}:${HD_BLD_LIB}:${HD_TOP_LIB}:${HC_BLD_EXEC_PFX}/lib:${HC_EXEC_PFX}/lib:${PATH}"; \
	LD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_LIB}:${HC_EXEC_PFX}/lib:${HD_LIB}:${LD_LIBRARY_PATH}"; \
	DYLD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_LIB}:${HC_EXEC_PFX}/lib:${HD_LIB}:${DYLD_LIBRARY_PATH}"; \
	export PATH; \
	export LD_LIBRARY_PATH; \
	export DYLD_LIBRARY_PATH; \
	echo "Generating xhelp.dhf"; echo "./mkhelp${BSUF} xhelp.shf"; \
	./mkhelp${BSUF} xhelp.shf
