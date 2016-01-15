REPO	        ?= op-worker

# distro for package building
DISTRIBUTION    ?= none
export DISTRIBUTION

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION	    ?= $(shell git describe --tags --always | tr - .)
PKG_ID	         = op-worker-$(PKG_VERSION)
PKG_BUILD	     = 1
BASE_DIR	     = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR	        ?= $(BASE_DIR)/rebar
PKG_VARS_CONFIG  = pkg.vars.config
OVERLAY_VARS    ?=

.PHONY: deps test package

all: test_rel

##
## Rebar targets
##

recompile:
	./rebar compile skip_deps=true

##
## If performance is compiled in cluster_worker ten annotations do not work.
## Make sure they are not included in cluster_worker build.
## todo: find better solution
##
compile:
	sed -i "s/ \"deps\/ctool\/annotations\/performance\.erl\"/%%\"deps\/ctool\/annotations\/performance\.erl\"/" deps/cluster_worker/rebar.config
	rm deps/cluster_worker/ebin/performance.beam || true
	./rebar compile
	sed -i "s/%%\"deps\/ctool\/annotations\/performance\.erl\"/ \"deps\/ctool\/annotations\/performance\.erl\"/" deps/cluster_worker/rebar.config

deps:
	./rebar get-deps

##
## Reltool configs introduce dependency on deps directories (which do not exist)
## Also a release is not nescesary for us.
## We prevent reltool from creating a release.
## todo: find better solution
##
generate: deps compile
	sed -i "s/{sub_dirs, \[\"rel\"\]}\./{sub_dirs, \[\]}\./" deps/cluster_worker/rebar.config
	./rebar generate $(OVERLAY_VARS)
	sed -i "s/{sub_dirs, \[\]}\./{sub_dirs, \[\"rel\"\]}\./" deps/cluster_worker/rebar.config

clean: relclean pkgclean
	./rebar clean

distclean:
	./rebar delete-deps

##
## Release targets
##

rel: generate

test_rel: generate cm_rel appmock_rel

cm_rel:
	ln -sf deps/cluster_worker/cluster_manager/
	make -C cluster_manager/ rel

appmock_rel:
	make -C appmock/ rel

relclean:
	rm -rf rel/test_cluster
	rm -rf rel/op_worker
	rm -rf appmock/rel/appmock
	rm -rf cluster_manager/rel/cluster_manager

##
## Testing targets
##

eunit:
	./rebar eunit skip_deps=true suites=${SUITES}
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

coverage:
	$(BASE_DIR)/bamboos/docker/coverage.escript $(BASE_DIR)

##
## Dialyzer targets local
##

PLT ?= .dialyzer.plt

# Builds dialyzer's Persistent Lookup Table file.
.PHONY: plt
plt:
	dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
	    dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib sasl erts \
		ssl tools runtime_tools crypto inets xmerl snmp public_key eunit \
		mnesia edoc common_test test_server syntax_tools compiler ./deps/*/ebin; \
	fi; exit 0


# Dialyzes the project.
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath

##
## Packaging targets
##

export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE PKG_VARS_CONFIG

package/$(PKG_ID).tar.gz: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	for dep in package/$(PKG_ID) package/$(PKG_ID)/deps/*; do \
	     echo "Processing dependency: `basename $${dep}`"; \
	     vsn=`git --git-dir=$${dep}/.git describe --tags 2>/dev/null`; \
	     mkdir -p $${dep}/priv; \
	     echo "$${vsn}" > $${dep}/priv/vsn.git; \
	     sed -i'' "s/{vsn,\\s*git}/{vsn, \"$${vsn}\"}/" $${dep}/src/*.app.src 2>/dev/null || true; \
	done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package/$(PKG_ID).tar.gz
	cp package/$(PKG_ID).tar.gz .

package: package/$(PKG_ID).tar.gz
	${MAKE} -C package -f $(PWD)/deps/node_package/Makefile

pkgclean:
	rm -rf package
