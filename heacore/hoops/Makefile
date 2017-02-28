HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${HOOPS}

HD_LIBRARY_SRC_cxx	= hoops_exception.cxx hoops_group.cxx hoops_limits.cxx hoops_par.cxx \
			  hoops_ape.cxx hoops_prim.cxx hoops_prompt_group.cxx

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS}

HD_CXXLIBS		= ${HD_LFLAGS} -l${HOOPS} -l${PIL} -l${READLINE} \
			  ${SYSLIBS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${PIL}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INC_SUBDIR		= hoops

HD_INSTALL_HEADERS	= hoops/hoops_exception.h hoops/hoops.h \
			  hoops/hoops_itor.h hoops/hoops_limits.h \
			  hoops/hoops_ape_factory.h hoops/hoops_prim.h \
			  hoops/hoops_group.h hoops/hoops_prompt_group.h \
                          hoops/hoops_par.h

HD_CLEAN		= hoops_lim_test hoops_par_test hoops_prim_test \
			  hoops_sample test_tmp

include ${HD_STD_MAKEFILE}

test:
	${HD_MAKE} hoops_par_test HD_CXXTASK=hoops_par_test \
	  HD_CXXTASK_SRC_cxx=hoops_par_test.cxx
	${HD_MAKE} hoops_prim_test HD_CXXTASK=hoops_prim_test \
	  HD_CXXTASK_SRC_cxx=hoops_prim_test.cxx
	-${HD_MAKE} hoops_lim_test HD_CXXTASK=hoops_lim_test \
	  HD_CXXTASK_SRC_cxx=hoops_lim_test.cxx
	${PERL} ./hoops_test

hoops_lim_test hoops_lim_test.o: hoops_lim_test.cxx hoops/hoops_limits.h

hoops_lim_test.cxx: limtester
	${PERL} ./$? > $@

hoops_par_test.cxx: partester
	${PERL} ./$? > $@

hoops_prim_test.cxx: primtester
	${PERL} ./$? > $@

hoops_sample${HIDE_ME}: hoops_sample.cxx
	${HD_MAKE} task-cxx HIDE_ME="hide" HD_TASK="$@" HD_CXXTASK_SRC_cxx="hoops_sample.cxx" 
