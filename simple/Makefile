.SUFFIXES:
.SUFFIXES: .erl .beam

.erl.beam: ; erlc -W $<

MODS = complex mandelbrot image render

ERL = erl -noshell

all: compile

compile: ${MODS:%=%.beam}

run: compile
	${ERL} -s render main 200 200 > Mandelbrot.ppm

clean:
	rm -rf *.beam erl_crash.dump Mandelbrot.ppm
