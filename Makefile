.PHONY: target-clean all install uninstall lib-unix lib-js
.DEFAULT: all

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX), --prefix $(shell opam config var prefix))

all: lib-unix lib-js

target-clean:
	@cp uwt-js.install uwt-js.install.orig
	@cat uwt-js.install.orig | grep -v '__' | grep -v -E '/[^u][^/]+\.ml' >uwt-js.install

uwt-js.install.orig: target-clean

lib-unix:
	@omake lib

lib-js:
	@jbuilder build --dev -p uwt-js

uwt-js.install: lib-js

install: lib-unix lib-js uwt-js.install.orig
	opam-installer $(INSTALL_ARGS)
	omake install

uninstall:
	opam-installer -u $(INSTALL_ARGS)
	omake uninstall

clean:
	rm -rf _build uwt-js.install uwt-js.install.orig
	omake clean
