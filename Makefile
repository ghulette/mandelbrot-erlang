.SUFFIXES:
.SUFFIXES: .erl .beam

.erl.beam: ; erlc -W $<

MODS = complex mandelbrot image render

all: compile

compile: ${MODS:%=%.beam}

#run: compile
#${ERL} -s main start

clean:
	rm -rf *.beam erl_crash.dump
