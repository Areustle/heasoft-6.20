HD_COMPONENT_NAME       = Xspec

HD_COMPONENT_VERS       = 

HD_CXXTASK              = raysmith

HD_CXXTASK_SRC_cxx      = raysmith.cxx

HD_CXXTASK_SRC_f        = adh.f alflo.f aphot.f atrd.f atread.f autoin.f \
                          blnd.f brems.f bund.f cion.f cthei.f cther.f \
                          cthi.f cthr.f delt.f dimet.f effn.f effnew.f \
                          expf.f fable.f fbg.f fecool.f feline.f gamma.f \
                          gaunt.f gncrch.f grec.f hcool.f heseq.f hline.f \
                          hyseq.f lnprt.f noson.f nquil.f phot.f popmet.f \
                          rayspec_m.f recems.f seaton.f secont.f single.f \
                          solvx.f taup.f twoph.f

HD_CXXFLAGS             = ${HD_STD_CXXFLAGS}

HD_FFLAGS               = ${HD_STD_FFLAGS}

HD_CXXLIBS              = ${HD_LFLAGS} -lXS -l${HEASP} -l${CCFITS} -l${HEAINIT} -l${HEAUTILS} \
		          -l${PIL} -l${HEAIO} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS        = ${HD_CXXTASK}

HD_INSTALL_PFILES       = raysmith.par

HD_INSTALL_HELP		= raysmith.html

include ${HD_STD_MAKEFILE}

