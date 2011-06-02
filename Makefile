# Makefile for lviv
#
# Copyright (c) 2011 Riad S. Wahby <rsw@jfet.org>
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# this might seem strange, but it gives us the files in the correct order
# for debug invocation
DEBUGFILES = $(shell grep include src/lviv.scm | grep -v \; | cut -d \" -f 2 | sed 's/^/src\//')
SRCFILES = src/lviv.scm $(DEBUGFILES)
ARGS  = -
.PHONY: all run runtest clean lviv test debug prof profrun

all: lviv test

# override ARGS on the commandline:
# make ARGS="examples/hello.lviv -" run
run:
	gsi src/lviv.scm $(ARGS)

runtest:
	gsi src/lviv.scm examples/test.lviv examples/hello.lviv

clean:
	rm -rf build

lviv: build/lviv

test: lviv
	build/lviv examples/test.lviv examples/hello.lviv

# run gsi on the source files, but do not run the repl
debug:
	gsi $(DEBUGFILES) -

# build lviv executable with profiling and coverage tests
prof: build/lviv-prof

# this will profile a long run of recursive calls to get an idea of the hot spots
profrun: prof
	cd build && echo '((nop) (1 + *n *upToN) 3 pick *n >= if) (*n) lambda *upToN define 1 100000 upToN' | ./lviv-prof && gprof ./lviv-prof > gprof.out
	cd build && gcov lviv-prof

# real targets from here down

build/lviv: $(SRCFILES)
	mkdir -p build
	GAMBC_CC_VERBOSE=yes gsc -warnings -o $@ -exe $<
	strip $@

build/lviv-prof: $(SRCFILES)
	mkdir -p build
	GAMBC_CC_VERBOSE=yes gsc -warnings -track-scheme -link $<
	mv src/lviv.c build/lviv-prof.c
	mv src/lviv_.c build/lviv-prof_.c
	cd build && gcc -o $(notdir $@) -fprofile-arcs -ftest-coverage -pg lviv-prof.c lviv-prof_.c -lgambc -lm -lutil -ldl -pg

