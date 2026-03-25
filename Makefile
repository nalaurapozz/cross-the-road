
SOURCES := $(wildcard src/*.elm)
TEST_SOURCES := $(wildcard tests/*.elm)
TARGET1 := public/crosstheroad.js
TARGET2 := public/fourbit.js
TARGETS := ${TARGET1} ${TARGET2}

all: test

test: compile .test-result

compile-debug:
	rm -f ${TARGETS} .test-result
	elm make --output=${TARGET1} src/Main.elm --debug
	elm make --output=${TARGET2} src/Main.elm --debug

compile: ${TARGETS}

compile-optimised:
	rm -f ${TARGET1} .test-result
	elm make --output=${TARGET1}.preopt.js src/Main.elm --optimize
	uglifyjs ${TARGET1}.preopt.js --compress \
		'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output=${TARGET1}
	rm ${TARGET1}.preopt.js

.test-result: ${SOURCES} ${TEST_SOURCES}
	elm-test
	touch $@

${TARGET1}: ${SOURCES}
	elm make --output=$@ src/Main.elm

${TARGET2}: ${SOURCES}
	elm make --output=$@ src/MainFourBit.elm

clean:
	rm -f ${TARGETS} .test-result
