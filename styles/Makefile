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

# build the CSS file
#
smldoc.css:	smldoc.less $(PALETTE)
	$(LESSC) $(LESSC_FLAGS) --global-var="color-palette=\"$(PALETTE)\"" smldoc.less > smldoc.css

