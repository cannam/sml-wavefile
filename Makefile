
SCRIPTS		:= ext/sml-buildscripts

all:	${SCRIPTS} example

example:	example.mlb Makefile
	${SCRIPTS}/polybuild example.mlb

clean:
	rm -f example

${SCRIPTS}:
	./vext install

%.deps:	%.mlb
	${SCRIPTS}/mlb-dependencies $< > $@

-include example.deps
