HD_COMPONENT_NAME       = swift

HD_INSTALL_PERL_SCRIPTS	= batgrbproduct

NEW_TASK		= ${HD_INSTALL_PERL_SCRIPTS}

HD_INSTALL_HELP		= ${NEW_TASK}.html

HD_INSTALL_PFILES	= ${NEW_TASK}.par

NEW_TASK_FILES		= ${NEW_TASK} ${NEW_TASK}.par ${NEW_TASK}.html

HD_DISTCLEAN		= ${NEW_TASK_FILES}

default: ${NEW_TASK_FILES}

all: default publish

include ${HD_STD_MAKEFILE}

OLD_TASK		= ../bat-burst-advocate

OLD_TASK_FILES		= ${OLD_TASK} ${OLD_TASK}.par ${OLD_TASK}.html

# NOTE: touch command used so that batgrbproduct is no older than 
# the base script
${NEW_TASK}: ${OLD_TASK} batgrbproduct.sed
	sed -f batgrbproduct.sed $< > $@
	touch -r $< $@

${NEW_TASK}.html: ${OLD_TASK}.html batgrbproduct.sed
	sed -f batgrbproduct.sed $< > $@
	touch -r $< $@

${NEW_TASK}.par: ${OLD_TASK}.par
	cp -p $< $@
	touch -r $< $@
