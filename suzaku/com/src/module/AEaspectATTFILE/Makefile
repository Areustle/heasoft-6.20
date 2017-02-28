#-------------------------------------------------------------------------------
# Begin general LHEA macros
#-------------------------------------------------------------------------------
# Identity of this directory; used by hmake to set LHEA_SRC
# It is essential that this macro be correctly set, even though
# it is not explicitly used in the Makefile:
LHEA_ID 	=	astroe/com/src/module/AEaspectATTFILE

# Directories in which built items are placed during the build process
BLDINC		=	${LHEA_SRC}/include
BLDLIB		=	${LHEA_SRC}/lib.tmp

# Name of package
PKG		=	astroe

# Top directory of this package tree
PKGTOP		=	${LHEA_SRC}/${PKG}

# Force Bourne shell to be used for commands
SHELL		=	/bin/sh
#-------------------------------------------------------------------------------
# End general LHEA macros
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Begin compilation-related macros
#-------------------------------------------------------------------------------

# C Flags peculiar to this directory
LOC_CFLAGS	=	-I../../../../aste_anl/include/ \
			-I../../../../astetool/include/ -I${LHEA_INC}/
#-------------------------------------------------------------------------------
# End compilation-related macros
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Begin internal library macros
#-------------------------------------------------------------------------------
# Base name for library internal to this package
THISLIB_ROOT	=	common

# Full name for static library internal to this package
THISLIB 	=	${BLDLIB}/lib${THISLIB_ROOT}.a
#-------------------------------------------------------------------------------
# End internal library macros
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Begin public targets
#-------------------------------------------------------------------------------
default: ${THISLIB} insert

all: 
	@${MAKE} default clean LHEA_INC="${BLDINC}"

install: 

clean: 
	-rm -f *.o

distclean: clean
	-rm -f object-list
	-rm -f ${THISLIB}
#-------------------------------------------------------------------------------
# End public targets
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Begin internal library targets
#-------------------------------------------------------------------------------

${THISLIB} :: AEaspectATTFILE.c
	@${MAKE} ${?:.c=.o}

${THISLIB} :: ../../../../aste_anl/include/bnk.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ../../../../astetool/include/aste_att.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ../../../../astetool/include/aste_coord.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ../../../../astetool/include/aste_teldef.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ../../../../astetool/include/aste_time.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ../../../../astetool/include/attfile.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ../../../../astetool/include/teldef.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ${LHEA_INC}/atFunctions.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ${LHEA_INC}/fitsio.h
	@${MAKE} AEaspectATTFILE.o

${THISLIB} :: ${LHEA_INC}/longnam.h
	@${MAKE} AEaspectATTFILE.o

object-files: ${THISLIB}

insert: ${BLDLIB}
	@if [ -s object-list ]; then \
		objects=`cat object-list | sort | uniq | tr '\012' ' '`; \
		${AR} ruv ${THISLIB} $$objects; status=$$?; \
		if [ $$status -ne 0 ]; then exit $$status; \
		else rm -f object-list; fi; \
	fi

# Use this target if you delete object files by hand, to tell
# the Makefile they were deleted.
reset-object-list: 
	rm -f object-list; ls *.o > object-list 2> /dev/null
#-------------------------------------------------------------------------------
# End internal library targets
#-------------------------------------------------------------------------------

AEaspectATTFILE.o: ../../../../aste_anl/include/bnk.h \
		../../../../astetool/include/aste_att.h \
		../../../../astetool/include/aste_coord.h \
		../../../../astetool/include/aste_teldef.h \
		../../../../astetool/include/aste_time.h \
		../../../../astetool/include/attfile.h \
		../../../../astetool/include/teldef.h \
		${LHEA_INC}/atFunctions.h ${LHEA_INC}/fitsio.h \
		${LHEA_INC}/longnam.h
#-------------------------------------------------------------------------------
# Begin utility targets
#-------------------------------------------------------------------------------

# Create any necessary directories
${BLDLIB}: 
	mkdir $@

# Dummy target to force a target to be out of date
force: 
#-------------------------------------------------------------------------------
# End utility targets
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Rules for compilations
#-------------------------------------------------------------------------------

.SUFFIXES: 

.SUFFIXES: .c .o

.c.o: 
	${CC} -c ${LOC_CFLAGS} ${CFLAGS} $<
	@echo $@ >> object-list
