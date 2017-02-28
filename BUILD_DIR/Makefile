HD_TOP_EXEC_PFX		= @exec_prefix@
HD_SELECTED		= @HD_SELECTED@
HD_LN_S			= ln -s
HD_CP_P			= cp -p
BSUF			= @BSUF@

default: all

all: check-selection hd_install
	@${MAKE} ${HD_SELECTED} HD_TARGET=$@
	@echo "Finished make $@" >&2

hd_install: hd_install.c
	${CC} ${CFLAGS} -o $@ hd_install.c

install: check-selection
	@${MAKE} ${HD_SELECTED} HD_TARGET=$@
	@./hd_install . ${HD_TOP_EXEC_PFX}/BUILD_DIR ${HD_CP_P}
	@if [ -d ../glast ]; then \
		./hd_install -f ${HD_TOP_EXEC_PFX}/BUILD_DIR/fermi-init.csh \
			${HD_TOP_EXEC_PFX}/fermi-init.csh ${HD_LN_S}; \
		./hd_install -f ${HD_TOP_EXEC_PFX}/BUILD_DIR/fermi-init.sh \
			${HD_TOP_EXEC_PFX}/fermi-init.sh ${HD_LN_S}; \
	else \
		./hd_install -f ${HD_TOP_EXEC_PFX}/BUILD_DIR/headas-init.csh \
			${HD_TOP_EXEC_PFX}/headas-init.csh ${HD_LN_S}; \
		./hd_install -f ${HD_TOP_EXEC_PFX}/BUILD_DIR/headas-init.sh \
			${HD_TOP_EXEC_PFX}/headas-init.sh ${HD_LN_S}; \
	fi
	@./hd_install -f hd_install${BSUF} ${HD_TOP_EXEC_PFX}/bin/hd_install${BSUF} ${HD_CP_P}
	@./hd_install -f hd_scanenv ${HD_TOP_EXEC_PFX}/bin/hd_scanenv ${HD_CP_P}
	@./hd_install -f hmake ${HD_TOP_EXEC_PFX}/bin/hmake ${HD_CP_P}
	@./hd_install -f lhea-fixperl ${HD_TOP_EXEC_PFX}/bin/lhea-fixperl ${HD_CP_P}
	@${MAKE} install-fguiparfiles
	@echo "Finished make $@" >&2

install-fguiparfiles:
	@if [ -d ../ftools/guis/fgui ]; then \
		echo "Creating .par2 files for fgui:"; \
		HEADAS=${HD_TOP_EXEC_PFX}; export HEADAS; \
		. ${HD_TOP_EXEC_PFX}/headas-init.sh; \
		echo "cd ../ftools/guis/fgui"; \
		cd ../ftools/guis/fgui; \
		echo "`which tclsh` convertpar.tcl ftools"; \
		tclsh${BSUF} convertpar.tcl ftools; \
	fi
	@echo "Finished make $@" >&2

test install-test: check-selection
	@${MAKE} ${HD_SELECTED} HD_TARGET=$@
	@echo "Finished make $@" >&2

run-test:
	@status=0; for dir in ${HD_SELECTED}; do \
	  ${MAKE} $$dir HD_TARGET=$@; \
	  if test $$? -ne 0; then status=1; fi; \
	done; exit $$status
	@echo "Finished make $@" >&2

clean: check-selection
	@${MAKE} ${HD_SELECTED} HD_TARGET=$@
	rm -f hd_install${BSUF}
	@echo "Finished make $@" >&2

distclean: check-selection
	@${MAKE} ${HD_SELECTED} HD_TARGET=$@
	rm -f Makefile confdefs.h config.cache config.log config.out \
	    config.status configure.lineno hd_install${BSUF} headas-setup \
	    fermi-setup
	cp -p Makefile.in Makefile
	@echo "Finished make $@" >&2

${HD_SELECTED}: check-selection
	@if [ -f ../$@/BUILD_DIR/hmakerc ]; then \
	    cd ../$@/BUILD_DIR; \
	    ./hmake ${HD_TARGET}; \
	fi

check-selection:
	@if [ `echo ${HD_SELECTED} | grep -c HD_SELECTED` -gt 0 ]; then \
	  echo "Components have not been selected. Please make sure configure ran successfully." >&2; \
	  exit 2; \
	elif [ "x${HD_SELECTED}" = x ]; then \
	  echo "No components are currently selected. Please run configure to select components."; \
	  exit 2; \
	fi

.DEFAULT:
	@echo "Target $@ is not currently valid. Please use a valid" >&2
	@echo "target, and make sure configure ran successfully." >&2;
	@exit 2;
