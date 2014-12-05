# Makefile for smldoc CSS style file
#

SHELL =		/bin/sh

# lessc from http://lesscss.org/
#
LESSC =		/opt/local/bin/lessc

# lessc flags
#
LESSC_FLAGS =	--verbose

# default color palette
#
PALETTE =	mlton-colors.less

# default layout dimensions and font sizes
#
LAYOUT =	default-layout.less

LESSC_GLOBALS =	--global-var="palette=\"$(PALETTE)\"" \
		--global-var="layout=\"$(LAYOUT)\""

# build the CSS file
#
smldoc.css:	smldoc.less $(PALETTE)
	$(LESSC) $(LESSC_FLAGS) $(LESSC_GLOBALS) smldoc.less > smldoc.css

