REBAR=rebar
COMMON_TEST=ctest
COMMON_TEST_LOG_DIRS=${COMMON_TEST}/logs
CT_DEPS=../deps
CT_TOP=../src

all: deps compile

deps: deps/erlangsp_app

deps/erlangsp_app:
	@${REBAR} get-deps

compile:
	@${REBAR} compile

dialyze: all
	@dialyzer -Wrace_conditions ebin

gc: crash
	@echo 'Removing all emacs backup files'
	@find . -name "*~" -exec rm -f {} \;
	@find . -name "erl_crash.dump" -exec rm -f {} \;
	@echo 'Removing all compile artifacts'
	@rm -f src/*.P
	@rm -f src/*/*.P
	@rm -f src/*.beam
	@rm -f src/*/*.beam
	@echo 'Removing all common_test beams'
	@rm -f ${COMMON_TEST_TOP}/*/*.beam
	@echo 'Removing all common_test logs'
	@rm -rf ${COMMON_TEST_LOG_DIRS}/*.*
	@rm -f ${COMMON_TEST_LOG_DIRS}/variables-ct*

rel: all
	@echo 'Generating erlangsp release'
	@(cd rel; ${REBAR} generate)

clean: gc
	@${REBAR} clean

crash:
	@find . -name "erl_crash.dump" -exec rm -f {} \;

relclean: crash
	@rm -rf rel/esp_example

realclean: clean relclean
	@${REBAR} del-deps
	@rm -rf deps/*

test: all
	@(cd ctest; ct_run -spec erlangsp_app.spec -pz ../ebin -pz ../deps/*/ebin)
