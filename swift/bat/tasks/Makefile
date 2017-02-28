HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_SUBDIRS		= batbinevt batcelldetect batclean batdetmask \
			  batdph2dpi batdph2pha batdrmgen bateconvert \
			  baterebin batextractor \
			  batfftimage batglobalgti batgrbproduct \
			  bathotpix batid2xy batimgstatpos batmasktaglc \
			  batmaskwtevt batmaskwtimg batoccultgti batoccultmap \
			  batphasimerr batphasyserr batsumdph batsurvey \
			  battblocks battsplit batupdatephakw batwarpimg 

HD_TEST_SUBDIRS		= $(HD_SUBDIRS) .

HD_TEST_PERL_SCRIPTS    = ut_batALLTOOLS.pl

include ${HD_STD_MAKEFILE}

# Special dependencies for Wiki documentation
HELP_DOCS 		= $(wildcard */*.html */*/*.html)
WIKI_DOCS		= $(HELP_DOCS:.html=.wiki)
HTML2WIKI		= $(HOME)/bin/html2wiki

wiki-docs: $(WIKI_DOCS)

%.wiki: %.html 
	$(HTML2WIKI) $< $@
	touch $@_updated
