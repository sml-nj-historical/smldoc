# Makefile for smldoc user guide
#

SHELL =		/bin/sh

TEXT_FILES =	$(wildcard *.txt)
XML_FILES =	$(TEXT_FILES:.txt=.xml)
PDF_FILES =	$(TEXT_FILES:.txt=.pdf)
HTML_FILES =	$(TEXT_FILES:.txt=.html)

all:		smldoc-guide.html

%.xml : %.txt
	asciidoc --attribute=lang=en -b docbook45 -d article -o $@ $<

%.pdf : %.xml
	dblatex -P latex.class.options=12pt -s conf/lectures-dblatex.sty -t pdf -o $@ $<
	rm -f $<

%.html : %.txt
	asciidoc -b html --attribute=source-highlighter=pygments $<

.PHONY:	clean
clean:
	rm -f $(PDF_FILES) $(HTML_FILES) $(XML_FILES)
