# Makefile
#
# COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Makefile for smldoc tool
#
# Note: this is a placeholder Makefile while smldoc is under development.
# Eventually we will use the install-smlnj-wrapper script to build the
# executable.
#

STYLES_DIR =		../styles

ML_BUILD =		ml-build
ML_BUILD_FLAGS =	
ML_MAKEDEPEND =		ml-makedepend
ML_MAKEDEPEND_FLAGS =

# heap suffix
#
HEAP_SUFFIX = $(shell sml @SMLsuffix)

PROGRAM =		smldoc
HEAP_IMAGE =		$(PROGRAM).$(HEAP_SUFFIX)
ROOT_CM =		driver/sources.cm
CM_FILES =		front-end/sources.cm \
			markup/sources.cm \
			back-end/sources.cm

# generated source files
#
GEN_FILES =		back-end/style-element.sml

.PHONY:		build
build:		$(HEAP_IMAGE)

$(HEAP_IMAGE):	.depend
	$(ML_BUILD) $(ML_BUILD_FLAGS) $(ROOT_CM) Main.main $(PROGRAM)
	touch $(HEAP_IMAGE)

.depend:	$(CM_FILES) $(GEN_FILES)
	touch .depend
	$(ML_MAKEDEPEND) $(ML_MAKEDEPEND_FLAGS) -n -f .depend $(ROOT_CM) $(HEAP_IMAGE)

back-end/style-element.sml:
	(cd $(STYLES_DIR); make smldoc.css)
	$(STYLES_DIR)/gen-sml.sh $(STYLES_DIR)/smldoc.css back-end/style-element.sml

.PHONY:		clean
clean:
	rm -f $(HEAP_IMAGE)

.PHONY:		dist-clean
dist-clean:	clean
	rm -f .depend back-end/style-element.sml
