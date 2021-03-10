
SCRIPTS		:= ext/sml-buildscripts

#BUILDER		:= ${SCRIPTS}/polybuild
BUILDER		:= mlton

all:	${SCRIPTS} example

example:	example.mlb Makefile
	${BUILDER} example.mlb

clean:
	rm -f example

${SCRIPTS}:
	./repoint install

%.deps:	%.mlb ${SCRIPTS}
	${SCRIPTS}/mlb-dependencies $< > $@

-include example.deps
